
#### ========================================================================###
#### ======================== PROJECT SETUP =================================###
#### ========================================================================###
# Comment
#  Setup code copied from tutorial 1 R file (Author: Juan Pablo Ugarte Checura)

# install.packages(c(
#   "pwt10",        # Penn World Tables
#   "WDI",          # World Bank Development Indicators (for income groups)
#   "plm",          # Panel data models
#   "ggplot2",      # Plotting
#   "dplyr",        # Data manipulation
#   "stargazer"     # Regression tables
# ))

library(pwt10)
library(WDI)
library(plm)
library(ggplot2)
library(dplyr)
library(stargazer)
library(tidyr)
library(knitr)
library(tidyverse)
library(readxl)
library(countrycode)

library(Synth)

## Safe mean function wit hnas
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Load processed data
df <- readRDS("data/processed/processed_panel.rds")

# Country lists for treated and control

treated_countries <- c(
  "BEN", # Benin
  "BFA", # Burkina Faso
  "CIV", # Côte d'Ivoire
  "MLI", # Mali
  "NER", # Niger
  "SEN", # Senegal
  "TGO", # Togo
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "TCD", # Chad
  "COG", # Republic of Congo
  "GAB", # Gabon
  "GNQ"  # Equatorial Guinea
)

donor_countries <- c(
  "BGD", # Bangladesh
  "BRB", # Barbados
  "BTN", # Bhutan
  "BOL", # Bolivia
  "BWA", # Botswana
  "CPV", # Cabo Verde
  "DMA", # Dominica
  "ECU", # Ecuador
  "SWZ", # Eswatini
  "GRD", # Grenada
  "LAO", # Lao PDR
  "LSO", # Lesotho
  "MUS", # Mauritius
  "MAR", # Morocco
  "NAM", # Namibia
  "OMN", # Oman
  "PAN", # Panama
  "KNA", # St. Kitts and Nevis
  "LCA", # St. Lucia
  "SYC"  # Seychelles
)

scm_countries <- c(treated_countries, donor_countries)

#### ========================================================================###
#### ======================== SYNTEHETIC CONTROL METHOD ====================###
#### ========================================================================###

# keep only the treated CFA countries and the donor countries.
scm_df <- df |>
  filter(
    iso3c %in% scm_countries,
    year >= 1980,
    year <= 2019
  ) |>
  select(
    iso3c, country, year,
    gdp_pc,
    agriculture, industry, govt_share, invest_share,
    oda, fdi, labour, polity2
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

# Synth requires a balanced panel.
scm_df <- scm_df |>
  tidyr::complete( # add any rows that are missing
    iso3c,
    year = 1980:2019
  ) |>
  arrange(iso3c, year) |> # sorts by country and by year
  group_by(iso3c) |> # for each country
  tidyr::fill(country, .direction = "downup") |> # fill country name from above/below for missing
  ungroup()


scm_df <- scm_df |> # adds devalutaion dummy for treared countr
  mutate(
    devaluation_1994 = ifelse(
      iso3c %in% treated_countries & year == 1994,
      1,
      0
    ),
    unit_name = iso3c # use code as name to avoid inconsistenciese
  )

# generate numeric country ids for Synth
country_ids <- data.frame(
  iso3c = sort(unique(scm_df$iso3c)),
  unit_id = seq_along(sort(unique(scm_df$iso3c)))
)

scm_df <- scm_df |>
  left_join(country_ids, by = "iso3c") |>
  mutate(
    unit_id = as.numeric(unit_id)
  )

# Drop donor countries that are missing a main predictor across whole pre-treatment period
### NOTE 17/5: need to find root cause of these: shouldn't be the case
bad_controls <- scm_df |>
  filter(
    year >= 1980,
    year <= 2001,
    iso3c %in% donor_countries
  ) |>
  group_by(unit_id, iso3c, unit_name) |>
  summarise(
    bad_agriculture = all(is.na(agriculture)),
    bad_industry    = all(is.na(industry)),
    bad_govt        = all(is.na(govt_share)),
    bad_invest      = all(is.na(invest_share)),
    bad_oda         = all(is.na(oda)),
    bad_fdi         = all(is.na(fdi)),
    bad_labour      = all(is.na(labour)),
    bad_polity      = all(is.na(polity2)),
    .groups = "drop"
  ) |>
  filter(
    bad_agriculture | bad_industry | bad_govt | bad_invest |
      bad_oda | bad_fdi | bad_labour | bad_polity
  ) |>
  pull(unit_id)

# Define the treated-country ID and the usable donor-country IDs
treated_id <- country_ids$unit_id[country_ids$iso3c == treated_iso3c]

control_ids <- country_ids$unit_id[
  country_ids$iso3c %in% donor_countries &
    !(country_ids$unit_id %in% bad_controls)
]

# Put Synth ID variables in the first three columns:
# column 1 = numeric unit ID
# column 2 = country name/code
# column 3 = year
scm_df <- scm_df |>
  select(
    unit_id,
    unit_name,
    year,
    iso3c,
    country,
    gdp_pc,
    agriculture,
    industry,
    govt_share,
    invest_share,
    oda,
    fdi,
    labour,
    polity2,
    devaluation_1994
  )

# convert to df
scm_df <- as.data.frame(scm_df)

# Expected panel size: 33 countries * 40 years = 1320 rows.
# The number of usable controls may be below 20 if some donor countries were dropped.
nrow(scm_df) # 1324
length(unique(scm_df$iso3c)) # 33
table(scm_df$year) # 33x40
str(scm_df$unit_id) # 1 1 1 ...

# Show which donor units were dropped
country_ids |>
  filter(unit_id %in% bad_controls)
  
# =====================================================
# Run SCM for one country - to make into function
# =====================================================

treated_iso3c <- "GAB"   # to change

# Dataprep to organise data into form reqruired for synth:
dataprep.out <- dataprep(
  foo = scm_df,

  # predictors used in the paper
  predictors = c(
    "agriculture",
    "industry",
    "govt_share",
    "invest_share",
    "oda",
    "fdi",
    "labour",
    "polity2"
    # "devaluation_1994" # Removed bc no variation across some control units
  ),

  # (averaged over pre period to choose donor weights)
  predictors.op = "mean",

  # Outcome variable
  dependent = "gdp_pc",

  # Country and time identifiers
  
  unit.variable = 1, # country identifiers
  unit.names.variable = 2, # country names
  time.variable = 3, # time var

  # Treated country and donor pool
  treatment.identifier = treated_id, # treated country id
  controls.identifier = control_ids, # donor country ids

  # Pre-treatment period
  time.predictors.prior = 1980:2001, # pre-treatment for weight choice
  time.optimize.ssr = 1980:2001, # pre-treatment for weight choice

  # Full period to plot
  time.plot = 1980:2019, # pre and post-treatment

  # Lagged outcome values used as predictors in the paper 
  # (1980, 1995, and 2001 gdppc) - to match important points
  special.predictors = list(
    list("gdp_pc", 1980, c("mean")),
    list("gdp_pc", 1995, c("mean")),
    list("gdp_pc", 2001, c("mean"))
  )
)

# Estimate synthetic control
# solves for donor-country weights
synth.out <- synth(
  data.prep.obj = dataprep.out
)

# Show predictor balance and donor weights using synth.tab summaries:
# 1. donor weights: which countries make up synthetic contry;
# 2. predictor balance: how similar country is to synthetic before treatment.
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
)

print(synth.tables)

#Add refined optimizer settings to allow for zero-weights
synth.out_set <- synth(
  data.prep.obj = dataprep.out,
  Margin.ipop = .0005, Sigf.ipop = 10, Bound.ipop = 10
)

synth.tables_set <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out_set
)
synth.tables_set

# =====================================================
# Plot actual vs synthetic GDP per capita
# =====================================================
png(
  paste0("output/figures/scm_path_", treated_iso3c, ".png"),
   width = 900, height = 600)

path.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  tr.intake = 2002,
  Ylab = "Real GDP per capita",
  Xlab = "Year",
  Legend = c("Gabon", "Synthetic Gabon"),
  Legend.position = "topleft"
)

dev.off() # close graphic


# =====================================================
# Plot gap between actual and synthetic
# =====================================================

# NOTE 17/5: need to update to have file name automatic from country
png(
  paste0("output/figures/scm_gap_", treated_iso3c, ".png"),
  width = 900, height = 600)

gaps.plot(
  synth.res = synth.out,
  dataprep.res = dataprep.out,
  tr.intake = 2002,
  Ylab = "Gap in real GDP per capita",
  Xlab = "Year"
)

dev.off() # close graphic

