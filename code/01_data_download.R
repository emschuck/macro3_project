# International Macroeconomics Project

# Script 01     : Data download
# Authors       : Elizabeth Schuck, Callista Lodzinski, Mathilde Muller
# Date          : April 2026

# This script carries out the data acquisition, cleaning and preprocessing
#   1. Loading packages, verifying file structure
#   2. Read and inspect data from project files (Polity, ...),
#            WDI API, Penn World Tables
#   3. Data cleaning
#   4. Preliminary data exploration
#   6.


#### ========================================================================###
#### ======================== 1. PROJECT SETUP ==============================###
#### ========================================================================###

#  Setup code copied from tutorial 1 R file (Author: Juan Pablo Ugarte Checura)

# install.packages(c(
#   "pwt10",        # Penn World Tables
#   "WDI",          # World Bank Development Indicators (for income groups)
#   "plm",          # Panel data models
#   "ggplot2",      # Plotting
#   "dplyr",        # Data manipulation
#   "stargazer",    # Regression tables
#   "zoo"           # Interpolation used by fill_linear()
# ))

# Load the packages required for data download, cleaning, analysis, and output.
# Note: tidyverse already includes dplyr, ggplot2, tidyr, readr, etc., but the
# individual library calls are left explicit so readers can see core dependencies.
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
library(zoo) # Used for linear interpolation/extrapolation in fill_linear().

# Create output directories before saving tables, figures, and processed data.
# recursive = TRUE allows nested folders to be created in one call.
# showWarnings = FALSE avoids warnings if the folders already exist.
# Create output directories
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

## Safe mean function with NAs
# mean(x, na.rm = TRUE) returns NaN when all values are missing.
# This helper returns a clean NA_real_ instead, which is safer for tables.
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

#### ======================================================================###
#### ======================= 2. LOAD DATA =================================###
#### ======================================================================###

# WAEMU countries used in the main synthetic-control analysis
waemu <- c(
  "BEN", # Benin
  "BFA", # Burkina Faso
  "CIV", # Côte d'Ivoire
  "MLI", # Mali
  "NER", # Niger
  "SEN", # Senegal
  "TGO" # Togo
)

# Guinea-Bissau not in main analysis, but in table 1
waemu_table1 <- c(
  "BEN", # Benin
  "BFA", # Burkina Faso
  "CIV", # Côte d'Ivoire
  "GNB", # Guinea-Bissau
  "MLI", # Mali
  "NER", # Niger
  "SEN", # Senegal
  "TGO" # Togo
)

# CAEMC countries
caemc <- c(
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "TCD", # Chad
  "COG", # Republic of Congo
  "GNQ", # Equatorial Guinea
  "GAB" # Gabon
)

# Table 3 donor/control countries
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
  "SYC" # Seychelles
)

# Table 2 non-CFA comparison countries
non_cfa_comparison_countries <- c(
  "AGO", # Angola
  "BDI", # Burundi
  "COD", # Congo, Dem. Rep.
  "ETH", # Ethiopia
  "GMB", # Gambia, The
  "GHA", # Ghana
  "GIN", # Guinea
  "KEN", # Kenya
  "MDG", # Madagascar
  "MWI", # Malawi
  "NGA", # Nigeria
  "STP", # Sao Tome and Principe
  "SLE", # Sierra Leone
  "SDN", # Sudan
  "TZA", # Tanzania
  "UGA", # Uganda
  "ZMB", # Zambia
  "ZWE" # Zimbabwe
)

# Full country list for data download.
# unique() removes duplicates across WAEMU, CAEMC, donor, and comparison groups.
countries <- unique(c(
  waemu_table1,
  caemc,
  donor_countries,
  non_cfa_comparison_countries
))

# Override country names to avoid inconsistent labels across WDI/PWT/countrycode.
# This table is also used later for human-readable tables and plots.
country_names <- tibble::tribble(
  ~iso3c, ~country,
  "BEN", "Benin",
  "BFA", "Burkina Faso",
  "CIV", "Côte d'Ivoire",
  "GNB", "Guinea-Bissau",
  "MLI", "Mali",
  "NER", "Niger",
  "SEN", "Senegal",
  "TGO", "Togo",
  "CMR", "Cameroon",
  "CAF", "Central African Republic",
  "TCD", "Chad",
  "COG", "Republic of Congo",
  "GNQ", "Equatorial Guinea",
  "GAB", "Gabon",
  "BGD", "Bangladesh",
  "BRB", "Barbados",
  "BTN", "Bhutan",
  "BOL", "Bolivia",
  "BWA", "Botswana",
  "CPV", "Cabo Verde",
  "DMA", "Dominica",
  "ECU", "Ecuador",
  "SWZ", "Eswatini",
  "GRD", "Grenada",
  "LAO", "Lao PDR",
  "LSO", "Lesotho",
  "MUS", "Mauritius",
  "MAR", "Morocco",
  "NAM", "Namibia",
  "OMN", "Oman",
  "PAN", "Panama",
  "KNA", "St. Kitts and Nevis",
  "LCA", "St. Lucia",
  "SYC", "Seychelles",
  "AGO", "Angola",
  "BDI", "Burundi",
  "COD", "Congo, Dem. Rep.",
  "ETH", "Ethiopia",
  "GMB", "Gambia, The",
  "GHA", "Ghana",
  "GIN", "Guinea",
  "KEN", "Kenya",
  "MDG", "Madagascar",
  "MWI", "Malawi",
  "NGA", "Nigeria",
  "STP", "Sao Tome and Principe",
  "SLE", "Sierra Leone",
  "SDN", "Sudan",
  "TZA", "Tanzania",
  "UGA", "Uganda",
  "ZMB", "Zambia",
  "ZWE", "Zimbabwe"
)


### IMPUTATION PLAN
# Each row below specifies one variable-country imputation rule.
# The code later loops over this table and applies the specified method.

# method options:
#   "zero"          : replace missing values with 0
#   "linear"        : linear interpolation + linear extrapolation at endpoints
#   "nearest_fill"  : fill endpoint/interior gaps using nearest observed value
#
# Use country = "ALL" to apply a rule to all countries.

imputation_plan <- tibble::tribble(
  ~country, ~variable, ~method,
  "ALL", "polity2", "nearest_fill", # Missing for countries less than 500k pop

  "CAF", "industry", "nearest_fill", # Central African Republic; pre-2009
  "GNQ", "industry", "nearest_fill", # pre-2006
  "LAO", "industry", "linear", # pre-1989
  "LCA", "industry", "nearest_fill", # pre-2006

  "BRB", "oda_share", "nearest_fill", # 2010 onwards
  "NAM", "oda_share", "nearest_fill", # pre 1984
  "OMN", "oda_share", "nearest_fill", # 2010 onwards
  "SYC", "oda_share", "nearest_fill", # 2018
  "KNA", "oda_share", "nearest_fill", # 2013 onwards

  "GNQ", "fdi", "zero", # 1980 (near zero after)
  "BTN", "fdi", "zero", # pre 2001 (near zero after)
  "LAO", "fdi", "zero", # pre-1984 (near zero after)
  "NAM", "fdi", "zero", # pre 1985 (near zero after)

  "DMA", "labour", "nearest_fill", # Most
  "GRD", "labour", "nearest_fill", # pre 1988
  "SYC", "labour", "nearest_fill", # pre 1992
  "KNA", "labour", "nearest_fill", # post 2001

  "LAO", "gdp_wdi_current_pc", "linear", # pre-1980


  "ALL", "industry", "nearest_fill", # post 2001
  "ALL", "oda_share", "nearest_fill", # post 2001
  "ALL", "fdi", "nearest_fill", # post 2001
  "ALL", "labour", "nearest_fill" # post 2001
)

# -----------------------------
#  Download Penn World Tables
# -----------------------------
# PWT provides internationally comparable real GDP, expenditure-side output,
# employment, population, and expenditure-share variables.


# Load the packaged PWT 10.01 dataset into memory.
data("pwt10.01")

# Keep only variables needed for this project, restrict to project countries,
# and construct PWT-based controls/outcomes.
pwt <- pwt10.01 |>
  select(
    isocode,
    year,
    rgdpo,
    cgdpo,
    pop,
    emp,
    csh_g,
    csh_i,
    csh_m,
    csh_x,
    hc
  ) |>
  rename(
    iso3c = isocode
  ) |>
  filter(
    iso3c %in% countries,
    year >= 1980,
    year <= 2021
  ) |>
  distinct(iso3c, year, .keep_all = TRUE) |>
  mutate(
    gdp_pc = rgdpo / pop,
    gdp_pc_current = cgdpo / pop,
    labour = 100 * emp / pop,
    govt_share = csh_g,
    invest_share = csh_i
  )


# -----------------------------
# Download World Development Indicator data
# WDI supplies sectoral shares, inflation, ODA, FDI, and GDP variables that are
# either unavailable in PWT or used as alternative measures.
# -----------------------------

# View(WDIsearch("gdp"))

# WDI indicators used for variables not taken from PWT
# Named vector: left-hand names become column names in the downloaded WDI panel;
# right-hand strings are official World Bank indicator codes.
indicators <- c(
  agriculture = "NV.AGR.TOTL.ZS", # NV.AGR.TOTL.CD for levels
  # Agriculture, forestry, and fishing, value added (% of GDP)
  agriculture_alt = "NV.AGR.TOTL.CD", # NV.AGR.TOTL.CD for levels
  # Agriculture, forestry, and fishing, value added (current US$)
  ag_alt_2 = "NP.AGR.TOTL.CN",
  ag_alt_3 = "NA.GDP.AGR.CR",
  industry = "NV.IND.TOTL.ZS", # NV.IND.MANF.ZS for manufacturing only
  # Industry including construction, value added (% of GDP)
  industry_alt = "NV.IND.TOTL.CD",
  # Industry including construction, value added (level)
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  # Foreign direct investment, net inflows (% of GDP)
  govt_share_alt = "NE.CON.GOVT.ZS",
  # General government final consumption expenditure (% of GDP)
  invest_share_alt = "NE.GDI.FTOT.ZS",
  # Gross fixed capital formation (% of GDP)=
  fdi_alt = "BN.KLT.DINV.CD.DRS",
  # Foreign direct investment, net inflows (current USD)
  alt_inflation = "FP.CPI.TOTL.ZG",
  # Inflation, consumer prices (annual %)( not currently used)
  inflation = "NY.GDP.DEFL.KD.ZG",
  # Inflation, GDP deflator (annual %)
  gdp_pc_wdi_alt = "NY.GDP.PCAP.CD",
  # GDP per capita (current US$)
  gdp_pc_wdi = "NY.GDP.PCAP.PP.KD", # GDP, PPP (constant 2021 international $)
  gdp_wdi = "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2021 international $)
  gdp_pc_growth_wdi = "NY.GDP.PCAP.KD.ZG",
  gdp_wdi_current = "NY.GDP.MKTP.CD",
  # GDP growth rate, PPP (constant 2021 international $)
  # real gdp pc
  # gdp_pc_wdi = "NY.GDP.PCAP.PP.CD",
  # GDP per capita, PPP (current international $)
  oda = "DT.ODA.ODAT.GD.ZS",
  # Net official development assistance received (% of GNI)
  oda_alt = "DT.ODA.ODAT.CD"
  # Net ODA received (current USD)

  
)


# Download annual WDI data for all selected countries and years.
# may take some time
wdi <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2021
) |>
  select(-country) |>
  distinct(iso3c, year, .keep_all = TRUE)

# -----------------------------
# Merge WDI and PWT
# -----------------------------

# Merge datasets and add country names.
# left_join keeps the WDI panel as the master 
# dataset and adds matching PWT/country-name rows.
# Main country-year panel used throughout the script.
df <- wdi |>
  left_join(pwt, by = c("iso3c", "year")) |>
  left_join(country_names, by = "iso3c") |>
  mutate(
    region = case_when(
      iso3c %in% waemu_table1 ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% donor_countries ~ "Donor pool",
      iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
      TRUE ~ NA_character_
    ),
    period = case_when(
      year >= 1980 & year <= 2001 ~ "pre",
      year >= 2002 & year <= 2021 ~ "post",
      TRUE ~ NA_character_
    ),
    # ODA as share of GDP annually (both are current US$)
    oda_share = 100 * oda_alt / gdp_wdi_current,
    gdp_wdi_current_pc = gdp_wdi_current / (pop * 1000000) #
  )

# Basic checks: verify that the country-region and 
# pre/post-period counts match expectations.
table(df$region, useNA = "ifany") # Expect 252 840 756 336
table(df$period, useNA = "ifany") # Expect 1080 1188

df |>
  group_by(iso3c, country) |>
  summarise(
    missing_gdp_pc = sum(is.na(gdp_pc)),
    missing_labour = sum(is.na(labour)),
    missing_inflation = sum(is.na(inflation)),
    total_obs = n(),
    .groups = "drop"
  ) |>
  arrange(desc(missing_gdp_pc), desc(missing_labour)) |>
  print(n = Inf)


# =====================================================
# Load Polity data (institutions)
# Polity2 is used as an institutional/political-regime 
# control in the replication.
# =====================================================

# Polity score:
# -10 = full autocracy
# +10 = full democracy

# Read local Polity data. This file must exist in data/raw/polity5/.
polity <- read_excel("./data/raw/polity5/p5v2018.xlsx")

# Standardise country identifiers to ISO3, restrict the sample, and retain polity2 only.
polity_clean <- polity |>
  select(country, year, polity2) |>
  mutate(
    iso3c = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  ) |>
  filter(
    iso3c %in% countries,
    year >= 1980,
    year <= 2019
  ) |>
  select(iso3c, year, polity2)

# =====================================================
# Merge Polity data with WDI dataset
# =====================================================

df <- df |>
  left_join(polity_clean, by = c("iso3c", "year"))

# Check polity values
summary(df$polity2)


# =====================================================
# FAOSTAT Ag data
# This is used as a fallback source for agriculture value-
# added observations missing in WDI.
# =====================================================

# Read local FAOSTAT extract. This file must exist in data/raw/.
ag_extra <- read_csv("data/raw/FAOSTAT_data_en_5-20-2026.csv")

#Clean up - update column name, check data types, select years
ag_extra_clean <- ag_extra |>
  transmute(
    iso3c = `Area Code (ISO3)`,
    year = as.integer(Year),
    agriculture_extra = as.numeric(Value)
  ) |>
  filter(
    year >= 1980,
    year <= 2021
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

# Add agriculture data where missing.
# coalesce() keeps the WDI value when available and uses FAOSTAT as fallback
df <- df |>
  left_join(ag_extra_clean, by = c("iso3c", "year")) |>
  mutate(
    agriculture_original = agriculture,
    agriculture = coalesce(agriculture, agriculture_extra),
    agriculture_filled_from_extra = is.na(agriculture_original) & !is.na(agriculture_extra)
  ) |>
  select(-agriculture_extra)


# =====================================================
# Data imputation functions
# ====================================================

# Replace missing values with zero.
fill_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

# Fill missing values by linear inter/extrapolation using the year variable.
# Requires at least two observed values to define a line.
fill_linear <- function(x, year) {
  # Linear interpolation and extrapolation.
  # rule = 2 means values outside observed range are extended linearly
  # from the nearest available segment.
  if (sum(!is.na(x)) < 2) {
    return(x)
  }

  zoo::na.approx(
    object = x,
    x = year,
    xout = year,
    na.rm = FALSE,
    rule = 2
  )
}

# Fill missing values by carrying observed values forward, then backward.
# Down-up fill, acceptable for slowly moving controls.
fill_nearest <- function(x) {
  # Fill missing values using nearest available observed value.
  # If the whole series is missing, fill with zero.

  # If all are na, fill with zeros
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }

  out <- tidyr::fill(
    tibble(value = x),
    value,
    .direction = "downup"
  )$value

  out
}

# Apply one row of the imputation plan to the panel.
# country_i can be a specific ISO3 code or "ALL";
# variable_i is the column to update;
# method_i chooses which fill function to use.
apply_one_imputation <- function(data, country_i, variable_i, method_i) {
  if (!(variable_i %in% names(data))) {
    warning(paste("Variable not found:", variable_i))
    return(data)
  }

  if (!(method_i %in% c("zero", "linear", "nearest_fill"))) {
    warning(paste("Unknown imputation method:", method_i))
    return(data)
  }

  data |>
    group_by(iso3c) |>
    arrange(year, .by_group = TRUE) |>
    mutate(
      "{variable_i}" := case_when(
        country_i == "ALL" & method_i == "zero" ~
          fill_zero(.data[[variable_i]]),
        country_i == iso3c & method_i == "zero" ~
          fill_zero(.data[[variable_i]]),
        country_i == "ALL" & method_i == "linear" ~
          fill_linear(.data[[variable_i]], year),
        country_i == iso3c & method_i == "linear" ~
          fill_linear(.data[[variable_i]], year),
        country_i == "ALL" & method_i == "nearest_fill" ~
          fill_nearest(.data[[variable_i]]),
        country_i == iso3c & method_i == "nearest_fill" ~
          fill_nearest(.data[[variable_i]]),
        TRUE ~ .data[[variable_i]]
      )
    ) |>
    ungroup()
}


# =====================================================
# Apply imputation and check changes
# =====================================================

# Preserve an unimputed copy so imputation effects can be checked
df_before_imputation <- df

df_imputed <- df

# Sequentially apply every imputation rule. Later rules can overwrite values
# produced by earlier rules if they target the same country-variable pair.
for (i in seq_len(nrow(imputation_plan))) {
  df_imputed <- apply_one_imputation(
    data = df_imputed,
    country_i = imputation_plan$country[i],
    variable_i = imputation_plan$variable[i],
    method_i = imputation_plan$method[i]
  )
}


# CHECK DIFFERENCES

# Summarise missingness by country and variable before/after imputation.
# diagnostic table
make_missing_check <- function(data, variables_to_check) {
  data |>
    select(iso3c, country, year, all_of(variables_to_check)) |>
    pivot_longer(
      cols = all_of(variables_to_check),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(iso3c, country, variable) |>
    summarise(
      n_years = n(),
      n_missing = sum(is.na(value)),
      first_non_missing_year = ifelse(
        all(is.na(value)),
        NA_integer_,
        min(year[!is.na(value)])
      ),
      last_non_missing_year = ifelse(
        all(is.na(value)),
        NA_integer_,
        max(year[!is.na(value)])
      ),
      .groups = "drop"
    ) |>
    arrange(variable, iso3c)
}

variables_imputed <- unique(imputation_plan$variable)

missing_before <- make_missing_check(
  df_before_imputation,
  variables_imputed
)

missing_after <- make_missing_check(
  df_imputed,
  variables_imputed
)

imputation_check <- missing_before |>
  rename(
    n_missing_before = n_missing,
    first_non_missing_before = first_non_missing_year,
    last_non_missing_before = last_non_missing_year
  ) |>
  left_join(
    missing_after |>
      rename(
        n_missing_after = n_missing,
        first_non_missing_after = first_non_missing_year,
        last_non_missing_after = last_non_missing_year
      ),
    by = c("iso3c", "country", "variable", "n_years")
  ) |>
  mutate(
    missing_filled = n_missing_before - n_missing_after
  )

print(imputation_check, n = Inf)


# =====================================================
# Save processed data
# =====================================================
# Save processed panels for reproducibility and for later scripts.
saveRDS(df_imputed, "data/processed/processed_panel_imputed.rds")
saveRDS(df_before_imputation, "data/processed/processed_panel_unimputed.rds")

# View(df)


#### ========================================================================###
#### ==================== == INFLATION TABLES ===============================###
#### ========================================================================###

# Helper function to replicate inflation Tables 1 and 2
# Build an inflation table for a supplied country list.

# The function creates country-level pre/post averages and then
# appends a group average
make_inflation_table <- function(
  data,
  country_order,
  average_label = "Average inflation"
) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1980,
      year <= 2021
    ) |>
    mutate(
      table_period = case_when(
        year >= 1980 & year <= 2001 ~ "1980--2001",
        year >= 2002 & year <= 2021 ~ "2002--2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(table_period)) |>
    group_by(iso3c, country, table_period) |>
    summarise(
      inflation_avg = mean(inflation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = table_period,
      values_from = inflation_avg
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(country, `1980--2001`, `2002--2021`)

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `1980--2001` = mean(`1980--2001`, na.rm = TRUE),
      `2002--2021` = mean(`2002--2021`, na.rm = TRUE)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

# Table 1, Panel A: WAEMU
table1_waemu <- make_inflation_table(
  df_imputed,
  country_order = waemu_table1,
  average_label = "Average inflation for the WAEMU zone"
)

# Additional WAEMU average excluding Guinea-Bissau
# Compute a second WAEMU average excluding Guinea-Bissau, because it joined WAEMU later.
waemu_avg_excl_gnb <- table1_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average inflation for the WAEMU zone"
  )) |>
  summarise(
    country = "Average inflation without Guinea-Bissau",
    `1980--2001` = mean(`1980--2001`, na.rm = TRUE),
    `2002--2021` = mean(`2002--2021`, na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

table1_waemu <- bind_rows(table1_waemu, waemu_avg_excl_gnb)

# Table 1, Panel B: CAEMC
table1_caemc <- make_inflation_table(
  df_imputed,
  country_order = caemc,
  average_label = "Average inflation for the CAEMC zone"
)

# Table 2: Non-CFA comparison countries
table2_non_cfa_inflation <- make_inflation_table(
  df_imputed,
  country_order = non_cfa_comparison_countries,
  average_label = "Average inflation"
)

# Print tables
# View(table1_waemu)
# View(table1_caemc)
# View(table2_non_cfa_inflation)

# Save LaTeX outputs
# Export the WAEMU inflation table as a LaTeX fragment for inclusion in the report.
cat(
  kable(
    table1_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "CFA franc zone countries annual inflation rate: WAEMU"
  ),
  file = "output/tables/table1_panelA_waemu_inflation.tex"
)

cat(
  kable(
    table1_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CFA franc zone countries annual inflation rate: CAEMC"
  ),
  file = "output/tables/table1_panelB_caemc_inflation.tex"
)

cat(
  kable(
    table2_non_cfa_inflation,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA franc zone countries mean annual inflation rate"
  ),
  file = "output/tables/table2_non_cfa_inflation.tex"
)


# -----------------------------
# Regional inflation summary
# -----------------------------

inflation_region <- df_imputed |>
  filter(region %in% c("WAEMU", "CAEMC", "Donor pool", "Non-CFA comparison")) |>
  group_by(region, period) |>
  summarise(
    inflation_avg = mean(inflation, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = inflation_avg
  ) |>
  select(region, pre, post) |>
  mutate(
    pre = round(pre, 2),
    post = round(post, 2)
  )

# View(inflation_region)

#### ========================================================================###
#### ====================== GDP GROWTH GRAPH ================================###
#### ========================================================================###

# GDP per capita growth graph: WAEMU, CAEMC, non-CFA countries

# Use the imputed panel for descriptive outputs so that the generated 
# figures/tables are consistent with the saved processed panel.

analysis_data <- df_imputed

analysis_data <- analysis_data |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(growth = 100 * (gdp_pc / lag(gdp_pc) - 1)) |>
  ungroup()

df_growth <- analysis_data |>
  filter(region %in% c("WAEMU", "CAEMC", "Non-CFA comparison")) |>
  mutate(region_plot = recode(region,
    "Non-CFA comparison" = "Non-CFA countries"
  )) |>
  group_by(region_plot, year) |>
  summarise(growth = mean(growth, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1990, year <= 2021)

# Plot average annual GDP per capita growth by region.
# The vertical line marks 2002, the first post-treatment year
p_gdp_growth <- ggplot(df_growth, aes(year, growth, color = region_plot)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linewidth = 1.1, color = "black") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  scale_y_continuous(breaks = seq(-25, 25, 5), limits = c(-25, 25)) +
  labs(
    title = "Evolution of GDP per capita growth",
    x = NULL,
    y = "Growth rate (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# print(p_gdp_growth)

# Save the plot 
ggsave(
  "output/figures/gdp_pc_growth_regions.png",
  p_gdp_growth,
  width = 12,
  height = 4,
  dpi = 300
)


#### ========================================================================###
#### ============================= GDP TABLES ===============================###
#### ========================================================================###

# =====================================================
# GDP per capita growth appendix tables
# Comparison PWT GDP per capita (gdp_pc) and WDI GDP per capita (gdp_pc_wdi)
# =====================================================


# Compute annual GDP per capita growth for both variables
df_gdp_growth <- analysis_data |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    growth_gdp_pc = 100 * (gdp_pc / lag(gdp_pc) - 1),
    growth_gdp_pc_wdi = 100 * (gdp_pc_wdi / lag(gdp_pc_wdi) - 1)
  ) |>
  ungroup()

# Helper function for GDP growth tables
# Build GDP-growth appendix tables comparing PWT and WDI per-capita GDP
make_gdp_growth_table <- function(data, country_order, average_label) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      period = case_when(
        year >= 1990 & year <= 2001 ~ "1990-2001",
        year >= 2002 & year <= 2021 ~ "2002-2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period)) |>
    group_by(iso3c, country, period) |>
    summarise(
      gdp_pc_growth = safe_mean(growth_gdp_pc),
      gdp_pc_wdi_growth = safe_mean(growth_gdp_pc_wdi),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = period,
      values_from = c(gdp_pc_growth, gdp_pc_wdi_growth)
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(
      country,
      `gdp_pc 1990-2001` = `gdp_pc_growth_1990-2001`,
      `gdp_pc 2002-2021` = `gdp_pc_growth_2002-2021`,
      `gdp_pc_wdi 1990-2001` = `gdp_pc_wdi_growth_1990-2001`,
      `gdp_pc_wdi 2002-2021` = `gdp_pc_wdi_growth_2002-2021`
    )

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
      `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
      `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
      `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# Appendix 3, Panel A: WAEMU
gdp_growth_waemu <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = waemu_table1,
  average_label = "Average for the WAEMU area"
)

# Additional WAEMU average excluding Guinea-Bissau
gdp_growth_waemu_excl_gnb <- gdp_growth_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average for the WAEMU area"
  )) |>
  summarise(
    country = "Average without Guinea-Bissau",
    `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
    `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
    `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
    `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

gdp_growth_waemu <- bind_rows(gdp_growth_waemu, gdp_growth_waemu_excl_gnb)

# Appendix 3, Panel B: CAEMC
gdp_growth_caemc <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = caemc,
  average_label = "Average for the CAEMC zone"
)

# Appendix 4: Non-CFA comparison countries
gdp_growth_non_cfa <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = non_cfa_comparison_countries,
  average_label = "Average"
)

# Print tables
print(gdp_growth_waemu)
print(gdp_growth_caemc)
print(gdp_growth_non_cfa)

# Save LaTeX outputs
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  kable(
    gdp_growth_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "WAEMU countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelA_waemu_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CAEMC countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelB_caemc_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_non_cfa,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix4_non_cfa_gdp_growth.tex"
)

# Print to confirm all code has run
print("Complete")
