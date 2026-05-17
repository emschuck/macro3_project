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

# Create output directories
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


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
  "TGO"  # Togo
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
  "TGO"  # Togo
)

# CAEMC countries
caemc <- c(
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "TCD", # Chad
  "COG", # Republic of Congo
  "GNQ", # Equatorial Guinea
  "GAB"  # Gabon
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
  "SYC"  # Seychelles
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
  "ZWE"  # Zimbabwe
)

# Full country list for data download
countries <- unique(c(
  waemu_table1,
  caemc,
  donor_countries,
  non_cfa_comparison_countries
))


# -----------------------------
#  Download Penn World Tables
# -----------------------------

data("pwt10.01")

pwt <- pwt10.01 |>
  select(
    isocode,
    country,
    year,
    rgdpo, # Output-side real GDP at chained PPPs, million 2021 US$
    pop, # Population, millions
    emp, # Persons engaged, millions
    csh_g, # Government consumption share at current PPPs
    csh_i, # Gross capital formation share at current PPPs
    hc # Human capital index
  ) |>
  rename(
    iso3c = isocode, # marching with wdi
    country_pwt = country
  ) |>
  mutate(
    gdp_pc = rgdpo / pop, # Real GDP per capita, output-side PPP
    labour = 100 * emp / pop, # Employment as % of population
    govt_share = csh_g, # Government consumption share
    invest_share = csh_i # Investment / capital formation share
  )


# -----------------------------
# Download World Development Indicator data
# -----------------------------

#View(WDIsearch("inflation"))

# WDI indicators used for variables not taken from PWT
indicators <- c(
  agriculture = "NV.AGR.TOTL.ZS",
  # Agriculture, forestry, and fishing, value added (% of GDP)
  industry = "NV.IND.TOTL.ZS",
  # Industry including construction, value added (% of GDP)
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  # Foreign direct investment, net inflows (% of GDP)
  alt_inflation = "FP.CPI.TOTL.ZG",
  # Inflation, consumer prices (annual %)( not currently used)
  inflation = "NY.GDP.DEFL.KD.ZG",
  # Inflation, GDP deflator (annual %) 
  oda = "DT.ODA.ODAT.GD.ZS"
  # Net official development assistance received (% of GNI)
)

wdi <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2021 # 2021 for matching with inflation table not 2019 for main analysis
) |>
  rename(country_wdi = country)


# -----------------------------
# Merge WDI and PWT
# -----------------------------

df <- wdi |>
  left_join(pwt, by = c("iso3c", "year")) |>
  mutate(
    country = coalesce(country_wdi, country_pwt), 
    region = case_when( # Label the list the country is from
      iso3c %in% waemu_table1 ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% donor_countries ~ "Donor pool",
      iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
      TRUE ~ NA_character_ # assign missing value to any other cases
    ),
    period = case_when( # label pre and post 2001 (2001 in pre)
      year >= 1980 & year <= 2001 ~ "pre", 
      year >= 2002 & year <= 2021 ~ "post",
      TRUE ~ NA_character_ # assign missing value to any other cases
    )
  ) |>
  # drop temporary name columns (replaced by coalesced column)
  select(-country_wdi, -country_pwt)

# Basic checks
table(df$region, useNA = "ifany") # Expect 252 882 756 336
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


#### =======================================================================###
#### ==================== 3. INFLATION TABLES ===============================###
#### ========================================================================###

# Helper function to replicate inflation Tables 1 and 2
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
  df,
  country_order = waemu_table1,
  average_label = "Average inflation for the WAEMU zone"
)

# Additional WAEMU average excluding Guinea-Bissau
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
  df,
  country_order = caemc,
  average_label = "Average inflation for the CAEMC zone"
)

# Table 2: Non-CFA comparison countries
table2_non_cfa_inflation <- make_inflation_table(
  df,
  country_order = non_cfa_comparison_countries,
  average_label = "Average inflation"
)

# Print tables
View(table1_waemu)
View(table1_caemc)
View(table2_non_cfa_inflation)

# Save LaTeX outputs
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

inflation_region <- df |>
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

View(inflation_region)

# =====================================================
# GDP growth graph
# =====================================================

df <- df |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(growth = (gdp_pc / lag(gdp_pc) - 1) * 100)

library(ggplot2)
df_growth <- df |>
  group_by(region, year) |>
  summarize(growth = mean(growth, na.rm = TRUE))

ggplot(df_growth, aes(x = year, y = growth, color = region)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "black") +
  labs(
    title = "Evolution of GDP per capita growth",
    x = "Year",
    y = "Growth rate (%)"
  )
+theme_minimal()


# =====================================================
# 8. Load Polity data (institutions)
# =====================================================

# Polity score:
# -10 = full autocracy
# +10 = full democracy

polity <- read_excel("../data/raw/polity5/p5v2018.xlsx")

polity_clean <- polity |>
  select(country, year, polity2) |>
  mutate(
    iso3c = countrycode(country,
      origin = "country.name",
      destination = "iso3c"
    )
  ) |>
  filter(
    iso3c %in% countries,
    year >= 1980, year <= 2019
  )


# =====================================================
# 9. Merge Polity data with WDI dataset
# =====================================================

df <- df |>
  left_join(polity_clean, by = c("iso3c", "year"))

# Check polity values
summary(df$polity2)
