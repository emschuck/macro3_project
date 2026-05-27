# International Macroeconomics Project

# Script 01     : Data download
# Authors       : Elizabeth Schuck, Callista Lodzinski, Mathilde Muller
# Date          : April 2026

# This script carries out the data acquisition, cleaning, preprocessing, and
# descriptive-output construction for the CFA franc replication project.
# Main steps:
#   1. Load packages and shared constants
#   2. Load WDI, Penn World Tables, Polity, and FAOSTAT data
#   3. Merge and clean the country-year panel
#   4. Apply documented imputations
#   5. Save imputed and unimputed processed panels
#   6. Produce inflation and GDP-growth descriptive outputs

#### ========================================================================###
#### ======================== 1. PROJECT SETUP ==============================###
#### ========================================================================###

# Setup code copied from tutorial 1 R file (Author: Juan Pablo Ugarte Checura)

# install.packages(c(
#   "pwt10",        # Penn World Tables
#   "WDI",          # World Bank Development Indicators
#   "plm",          # Panel data models
#   "ggplot2",      # Plotting
#   "dplyr",        # Data manipulation
#   "stargazer",    # Regression tables
#   "zoo"           # Interpolation used by fill_linear()
# ))

# Load packages required for data download, cleaning, analysis, and output.
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
library(zoo)

# Load shared project constants.
# This file should define country lists, country labels, donor-pool choices,
# GDP variable settings, and the imputation plan.
constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
  stop("Constants file not found: ", constants_file)
}

source(constants_file)

# Create output directories before saving tables, figures, and processed data.
dir.create("./output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Safe mean function with NAs.
# mean(x, na.rm = TRUE) returns NaN when all values are missing. This helper
# returns a clean NA_real_ instead, which is safer for tables.
if (!exists("safe_mean")) {
  safe_mean <- function(x) {
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
}

### IMPUTATION PLAN
# Each row below specifies one variable-country imputation rule.
# The code later loops over this table and applies the specified method.

# method options:
#   "zero"         : replace missing values with 0
#   "linear"       : linear interpolation + linear extrapolation at endpoints
#   "nearest_fill" : fill endpoint/interior gaps using nearest observed value
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

  "GNQ", "fdi", "zero", # 1980, near zero after
  "BTN", "fdi", "zero", # pre-2001, near zero after
  "LAO", "fdi", "zero", # pre-1984, near zero after
  "NAM", "fdi", "zero", # pre-1985, near zero after

  "DMA", "labour", "nearest_fill", # Most years
  "GRD", "labour", "nearest_fill", # pre-1988
  "SYC", "labour", "nearest_fill", # pre-1992
  "KNA", "labour", "nearest_fill", # post-2001

  "LAO", "wdi_gdp_pc_constant", "linear", # pre-1980

  "ALL", "agriculture", "nearest_fill",
  "ALL", "industry", "nearest_fill",
  "ALL", "oda_share", "nearest_fill",
  "ALL", "fdi", "nearest_fill",
  "ALL", "labour", "nearest_fill",
  "ALL", "govt_share", "nearest_fill",
  "ALL", "invest_share", "nearest_fill",

  "ALL", "wdi_gdp_pc_constant", "nearest_fill",
  "ALL", "wdi_gdp_pc_current", "nearest_fill",
  "ALL", "pwt_rgdpo_pc", "nearest_fill",
  "ALL", "pwt_cgdpo_pc_current", "nearest_fill",
  "ALL", "pwt_wdi_gdp_constant_pc", "nearest_fill"
)

#### ======================================================================###
#### ======================= 2. LOAD SHARED CONSTANTS ======================###
#### ======================================================================###

# The following objects are expected to come from code/00_constants.R.
required_constants <- c(
  "waemu",
  "waemu_table1",
  "caemc",
  "donor_countries",
  "non_cfa_comparison_countries",
  "country_names"
)

missing_constants <- required_constants[!vapply(required_constants, exists, logical(1))]

if (length(missing_constants) > 0) {
  stop(
    "The constants file is missing these required objects: ",
    paste(missing_constants, collapse = ", ")
  )
}

# For the data-download script, include all countries that may be needed by the
# baseline donor pool, updated donor pool, CFA groups, and non-CFA comparison
# tables. This avoids having to rebuild the panel every time the SCM donor-pool
# choice is changed in the constants file.
all_donor_countries_for_download <- unique(c(
  if (exists("donor_countries_original")) donor_countries_original else character(0),
  if (exists("donor_countries_updated")) donor_countries_updated else character(0),
  donor_countries
))

countries <- unique(c(
  waemu_table1,
  caemc,
  all_donor_countries_for_download,
  non_cfa_comparison_countries
))

# Check that every requested country has a label in country_names.
missing_country_labels <- setdiff(countries, country_names$iso3c)

if (length(missing_country_labels) > 0) {
  warning(
    "These countries are in the sample but missing from country_names: ",
    paste(missing_country_labels, collapse = ", ")
  )
}

# Common mistake check: Jordan should use ISO3 code JOR, not JOD.
if ("JOD" %in% country_names$iso3c && !("JOR" %in% country_names$iso3c)) {
  warning("country_names contains JOD for Jordan. Use JOR as the ISO3 country code.")
}

#### ======================================================================###
#### ======================= 3. PENN WORLD TABLES ==========================###
#### ======================================================================###

# PWT provides internationally comparable GDP, employment, population, and
# expenditure-share variables. The pwt10 package stores the dataset locally.
data("pwt10.01")

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
    pwt_rgdpo_pc = rgdpo / pop,
    pwt_cgdpo_pc_current = cgdpo / pop,
    labour = 100 * emp / pop,
    govt_share = csh_g,
    invest_share = csh_i
  )

#### ======================================================================###
#### ======================= 4. WORLD BANK WDI =============================###
#### ======================================================================###

# WDI supplies sectoral shares, inflation, ODA, FDI, and alternative GDP
# variables. This script reads a saved WDI extract for speed and reproducibility.
wdi <- readRDS("data/raw/wdi.rds")

# Keep the same country-year scope as the PWT data.
wdi <- wdi |>
  filter(
    iso3c %in% countries,
    year >= 1980,
    year <= 2021
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

#### ======================================================================###
#### ======================= 5. MERGE WDI AND PWT ==========================###
#### ======================================================================###

# Merge WDI, PWT, and canonical country names. WDI is the master panel because it
# contains most controls and inflation variables used for descriptive tables.
df <- wdi |>
  left_join(pwt, by = c("iso3c", "year")) |>
  left_join(country_names, by = "iso3c") |>
  mutate(
    region = case_when(
      iso3c %in% waemu_table1 ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% all_donor_countries_for_download ~ "Donor pool",
      iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
      TRUE ~ NA_character_
    ),
    period = case_when(
      year >= 1980 & year <= 2001 ~ "pre",
      year >= 2002 & year <= 2021 ~ "post",
      TRUE ~ NA_character_
    ),
    # ODA share. This preserves the original script's denominator. If oda_alt is
    # current US dollars, the preferred denominator is an aggregate current-GDP
    # variable, not GDP per capita. Check the WDI construction script before
    # using this variable in final analysis.
    oda_share = 100 * oda_alt / wdi_gdp_pc_current,
    pwt_wdi_gdp_ppp_constant_pc = wdi_gdp_ppp_constant / (pop * 1000000),
    pwt_wdi_gdp_ppp_current_pc = wdi_gdp_ppp_current / (pop * 1000000),
    pwt_wdi_gdp_constant_pc = wdi_gdp_constant / (pop * 1000000),
    pwt_wdi_gdp_current_pc = wdi_gdp_current / (pop * 1000000)
  )

# Basic checks: verify country-region and pre/post-period counts.
print(table(df$region, useNA = "ifany"))
print(table(df$period, useNA = "ifany"))

df |>
  group_by(iso3c, country) |>
  summarise(
    missing_wdi_gdp_pc_constant = sum(is.na(wdi_gdp_pc_constant)),
    missing_labour = sum(is.na(labour)),
    missing_inflation = sum(is.na(inflation)),
    total_obs = n(),
    .groups = "drop"
  ) |>
  arrange(desc(missing_wdi_gdp_pc_constant), desc(missing_labour)) |>
  print(n = Inf)


#### ======================================================================###
#### ======================= 6. POLITY DATA ================================###
#### ======================================================================###

# Polity2 is used as an institutional/political-regime control.
# Polity score: -10 = full autocracy; +10 = full democracy.
polity <- read_excel("./data/raw/polity5/p5v2018.xlsx")

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

df <- df |>
  left_join(polity_clean, by = c("iso3c", "year"))

summary(df$polity2)

#### ======================================================================###
#### ======================= 7. FAOSTAT AGRICULTURE DATA ===================###
#### ======================================================================###

# FAOSTAT is used as a fallback source for agriculture value-added observations
# missing in WDI.
ag_extra <- read_csv("data/raw/FAOSTAT_data_en_5-20-2026.csv")

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

# Add agriculture data only where the WDI agriculture value is missing.
df <- df |>
  left_join(ag_extra_clean, by = c("iso3c", "year")) |>
  mutate(
    agriculture_original = agriculture,
    agriculture = coalesce(agriculture, agriculture_extra),
    agriculture_filled_from_extra = is.na(agriculture_original) & !is.na(agriculture_extra)
  ) |>
  select(-agriculture_extra)


#### ======================================================================###
#### ======================= 11. EXTENSION DATA =======================###
#### ======================================================================###

# This block constructs two extension variables:
#   XEU = exports to EU as % of total exports
#   MEU = imports from EU as % of total imports
#
# The raw IMF DOTS file should be saved at:
#   data/raw/imf_trade_dots.csv

dots_file <- "data/raw/imf_trade_dots.csv"

if (!file.exists(dots_file)) {
  stop("DOTS raw file not found: ", dots_file)
}

# Countries needed for the extension.
# This includes treated countries, SCM donors, and non-CFA comparison countries
# used in descriptive extension charts.
extension_countries <- unique(c(
  treated_countries,
  donor_countries,
  non_cfa_comparison_countries
))

# Read raw IMF DOTS data.
dot_raw <- readr::read_csv(
  dots_file,
  show_col_types = FALSE
)

# Keep only the columns needed and standardise names.
dot_clean <- dot_raw |>
  dplyr::rename(
    iso3c = COUNTRY.ID,
    indicator = INDICATOR.ID,
    partner = COUNTERPART_COUNTRY.ID,
    year = TIME_PERIOD,
    value = OBS_VALUE
  ) |>
  dplyr::select(
    iso3c,
    indicator,
    partner,
    year,
    value
  ) |>
  dplyr::mutate(
    year = as.integer(year),
    value = as.numeric(value)
  ) |>
  dplyr::filter(
    iso3c %in% extension_countries,
    year %in% extension_plot_period
  )

# Convert DOTS data from long to wide format.
# The expected DOTS codes are:
#   XG_FOB_USD + G001 = exports to world
#   MG_CIF_USD + G001 = imports from world
#   XG_FOB_USD + G163 = exports to EU
#   MG_CIF_USD + G163 = imports from EU
dot_shares <- dot_clean |>
  tidyr::pivot_wider(
    names_from = c(indicator, partner),
    values_from = value
  )

# Check that the columns needed for XEU and MEU exist.
required_dots_columns <- c(
  "XG_FOB_USD_G001",
  "MG_CIF_USD_G001",
  "XG_FOB_USD_G163",
  "MG_CIF_USD_G163"
)

missing_dots_columns <- setdiff(required_dots_columns, names(dot_shares))

if (length(missing_dots_columns) > 0) {
  stop(
    "These required DOTS columns are missing after pivot_wider(): ",
    paste(missing_dots_columns, collapse = ", ")
  )
}

# Construct EU export and import shares.
dot_shares <- dot_shares |>
  dplyr::rename(
    exports_world = XG_FOB_USD_G001,
    imports_world = MG_CIF_USD_G001,
    exports_eu = XG_FOB_USD_G163,
    imports_eu = MG_CIF_USD_G163
  ) |>
  dplyr::mutate(
    XEU = ifelse(
      is.na(exports_world) | exports_world == 0,
      NA_real_,
      100 * exports_eu / exports_world
    ),
    MEU = ifelse(
      is.na(imports_world) | imports_world == 0,
      NA_real_,
      100 * imports_eu / imports_world
    )
  ) |>
  dplyr::select(
    iso3c,
    year,
    XEU,
    MEU
  )

# DOTS omits missing country-years, so explicitly create a balanced country-year panel.
dot_balanced <- dot_shares |>
  tidyr::complete(
    iso3c = extension_countries,
    year = extension_plot_period
  ) |>
  dplyr::arrange(
    iso3c,
    year
  )

# Check DOTS missingness before interpolation.
dots_missing_check <- dot_balanced |>
  dplyr::group_by(iso3c) |>
  dplyr::summarise(
    n_years = dplyr::n(),
    miss_XEU = sum(is.na(XEU)),
    miss_MEU = sum(is.na(MEU)),
    miss_XEU_pre = sum(is.na(XEU[year %in% extension_pre_period])),
    miss_MEU_pre = sum(is.na(MEU[year %in% extension_pre_period])),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(miss_XEU_pre),
    dplyr::desc(miss_MEU_pre),
    iso3c
  )

print(dots_missing_check, n = Inf)

dir.create("output/extension_trade/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  dots_missing_check,
  "output/extension_trade/tables/dots_missing_check_before_interpolation.csv",
  row.names = FALSE
)

# Drop countries with very poor pre-treatment DOTS coverage.
# A country is dropped if either XEU or MEU has more than three missing
# observations in the extension pre-treatment period.
bad_dots_coverage <- dot_balanced |>
  dplyr::filter(year %in% extension_pre_period) |>
  dplyr::group_by(iso3c) |>
  dplyr::summarise(
    miss_XEU_pre = sum(is.na(XEU)),
    miss_MEU_pre = sum(is.na(MEU)),
    .groups = "drop"
  ) |>
  dplyr::filter(
    miss_XEU_pre > 3 |
      miss_MEU_pre > 3
  ) |>
  dplyr::pull(iso3c)

# Fill short gaps of up to two years using linear interpolation.
# Longer gaps remain missing.
dot_clean_final <- dot_balanced |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    XEU = zoo::na.approx(XEU, na.rm = FALSE, maxgap = 2),
    MEU = zoo::na.approx(MEU, na.rm = FALSE, maxgap = 2)
  ) |>
  dplyr::ungroup()

# Final DOTS extension panel.
trade_dots_clean <- dot_clean_final |>
  dplyr::filter(!iso3c %in% bad_dots_coverage) |>
  dplyr::select(
    iso3c,
    year,
    XEU,
    MEU
  ) |>
  dplyr::arrange(
    iso3c,
    year
  )

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

saveRDS(
  trade_dots_clean,
  "data/processed/trade_dots_clean.rds"
)


# =====================================================
# Extension analysis: merge DOTS variables into panel
# =====================================================

# At this point, the WDI trade variables should already be in df because
# df was built from the WDI file earlier in the data-loading script.
missing_wdi_trade_before_dots_merge <- setdiff(
  c("trade_openness", "exports_gdp", "imports_gdp"),
  names(df)
)

if (length(missing_wdi_trade_before_dots_merge) > 0) {
  stop(
    "These WDI trade variables are missing before the DOTS merge: ",
    paste(missing_wdi_trade_before_dots_merge, collapse = ", ")
  )
}

# Load cleaned IMF DOTS EU trade-share variables.
dots_trade <- readRDS("data/processed/trade_dots_clean.rds") |>
  dplyr::select(
    iso3c,
    year,
    XEU,
    MEU
  ) |>
  dplyr::distinct(
    iso3c,
    year,
    .keep_all = TRUE
  )

# Check that the DOTS variables exist before merging.
missing_dots_vars <- setdiff(
  c("XEU", "MEU"),
  names(dots_trade)
)

if (length(missing_dots_vars) > 0) {
  stop(
    "These DOTS variables are missing from trade_dots_clean.rds: ",
    paste(missing_dots_vars, collapse = ", ")
  )
}

# Merge only the DOTS variables into the existing main panel.
# Do not merge WDI trade variables again, because they are already in df.
df <- df |>
  dplyr::left_join(
    dots_trade,
    by = c("iso3c", "year")
  )

# Confirm that all extension variables are present after merging.
missing_extension_vars <- setdiff(
  extension_trade_outcomes,
  names(df)
)

if (length(missing_extension_vars) > 0) {
  stop(
    "These extension variables are missing after merging: ",
    paste(missing_extension_vars, collapse = ", ")
  )
}

# cat("\nAll extension variables are present after DOTS merge.\n")



#### ======================================================================###
#### ======================= 8. IMPUTATION FUNCTIONS =======================###
#### ======================================================================###

# Replace missing values with zero.
fill_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

# Fill missing values by linear interpolation/extrapolation using year.
fill_linear <- function(x, year) {
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
# If the whole series is missing, fill with zero.
fill_nearest <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }

  tidyr::fill(
    tibble(value = x),
    value,
    .direction = "downup"
  )$value
}

# Apply one row of the imputation plan to the panel.
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
        country_i == "ALL" & method_i == "zero" ~ fill_zero(.data[[variable_i]]),
        country_i == iso3c & method_i == "zero" ~ fill_zero(.data[[variable_i]]),
        country_i == "ALL" & method_i == "linear" ~ fill_linear(.data[[variable_i]], year),
        country_i == iso3c & method_i == "linear" ~ fill_linear(.data[[variable_i]], year),
        country_i == "ALL" & method_i == "nearest_fill" ~ fill_nearest(.data[[variable_i]]),
        country_i == iso3c & method_i == "nearest_fill" ~ fill_nearest(.data[[variable_i]]),
        TRUE ~ .data[[variable_i]]
      )
    ) |>
    ungroup()
}

#### ======================================================================###
#### ======================= 9. APPLY IMPUTATION ===========================###
#### ======================================================================###

# Preserve an unimputed copy so imputation effects can be checked.
df_before_imputation <- df

df_imputed <- df

# Validate imputation plan before applying it.
missing_imputation_variables <- setdiff(unique(imputation_plan$variable), names(df_imputed))

if (length(missing_imputation_variables) > 0) {
  warning(
    "These imputation-plan variables are not in df and will be skipped: ",
    paste(missing_imputation_variables, collapse = ", ")
  )
}

imputation_plan_to_apply <- imputation_plan |>
  filter(variable %in% names(df_imputed))

# Sequentially apply every imputation rule. Later rules can overwrite values
# produced by earlier rules if they target the same country-variable pair.
for (i in seq_len(nrow(imputation_plan_to_apply))) {
  df_imputed <- apply_one_imputation(
    data = df_imputed,
    country_i = imputation_plan_to_apply$country[i],
    variable_i = imputation_plan_to_apply$variable[i],
    method_i = imputation_plan_to_apply$method[i]
  )
}

#### ======================================================================###
#### ======================= 10. IMPUTATION CHECKS =========================###
#### ======================================================================###

# Summarise missingness by country and variable before/after imputation.
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

variables_imputed <- unique(imputation_plan_to_apply$variable)

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


#### ======================================================================###
#### ======================= 12. SAVE PROCESSED DATA =======================###
#### ======================================================================###

saveRDS(df_imputed, "data/processed/processed_panel_imputed.rds")
saveRDS(df_before_imputation, "data/processed/processed_panel_unimputed.rds")

#### ========================================================================###
#### ======================== 13. INFLATION TABLES ==========================###
#### ========================================================================###

# Build an inflation table for a supplied country list. The function creates
# country-level pre/post averages and appends a group average.
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

# Table 1, Panel A: WAEMU.
table1_waemu <- make_inflation_table(
  df_imputed,
  country_order = waemu_table1,
  average_label = "Average inflation for the WAEMU zone"
)

# Additional WAEMU average excluding Guinea-Bissau, because it joined WAEMU later.
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

# Table 1, Panel B: CAEMC.
table1_caemc <- make_inflation_table(
  df_imputed,
  country_order = caemc,
  average_label = "Average inflation for the CAEMC zone"
)

# Table 2: Non-CFA comparison countries.
table2_non_cfa_inflation <- make_inflation_table(
  df_imputed,
  country_order = non_cfa_comparison_countries,
  average_label = "Average inflation"
)

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

# Regional inflation summary for quick diagnostics.
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

#### ========================================================================###
#### ============ 14. GDP GRAPH USING FINAL GDP VARIABLE =================###
#### ========================================================================###

# Use the imputed panel for descriptive outputs so generated figures/tables are
# consistent with the saved processed panel used in the SCM replication.
analysis_data <- df_imputed

# Identify the GDP variable actually used in the SCM replication.
# In the final replication this should be WDI GDP per capita in current USD.
scm_gdp_var <- if (exists("gdp_var")) gdp_var else "wdi_gdp_pc_current"

# Check that the selected GDP variable exists in the data.
if (!scm_gdp_var %in% names(analysis_data)) {
  stop(
    "The selected SCM GDP variable '", scm_gdp_var,
    "' is not present in analysis_data."
  )
}

# Create a standardised GDP variable for descriptive output.
analysis_data <- analysis_data |>
  mutate(gdp_used = .data[[scm_gdp_var]])

# Average GDP per capita by region, using the exact GDP variable used in SCM.
df_gdp_used_regions <- analysis_data |>
  filter(region %in% c("WAEMU", "CAEMC", "Non-CFA comparison")) |>
  mutate(region_plot = recode(
    region,
    "Non-CFA comparison" = "Non-CFA countries"
  )) |>
  group_by(region_plot, year) |>
  summarise(
    gdp_used = safe_mean(gdp_used),
    .groups = "drop"
  ) |>
  filter(year >= 1990, year <= 2019)

# Plot average GDP per capita by region. The vertical line marks 2002, the first
# post-treatment year.
p_gdp_used_regions <- ggplot(
  df_gdp_used_regions,
  aes(year, gdp_used, color = region_plot)
) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linewidth = 1.1, color = "black") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  labs(
    title = "Evolution of GDP per capita",
    subtitle = paste0("GDP variable: ", scm_gdp_var),
    x = NULL,
    y = "GDP per capita",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/figures/gdp_pc_used_regions.png",
  p_gdp_used_regions,
  width = 12,
  height = 4,
  dpi = 300
)


#### ====================================================================###
#### ======== 15. GDP TABLES USING FINAL GDP VARIABLE ================###
#### ===================================================================###

# Helper function for appendix tables using the exact GDP variable used in SCM.
make_gdp_used_table <- function(data, country_order, average_label) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1990,
      year <= 2019
    ) |>
    mutate(
      table_period = case_when(
        year >= 1990 & year <= 2001 ~ "1990-2001",
        year >= 2002 & year <= 2019 ~ "2002-2019",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(table_period)) |>
    group_by(iso3c, country, table_period) |>
    summarise(
      mean_gdp_used = safe_mean(gdp_used),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = table_period,
      values_from = mean_gdp_used
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(
      country,
      `Mean GDP pc 1990-2001` = `1990-2001`,
      `Mean GDP pc 2002-2019` = `2002-2019`
    )

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `Mean GDP pc 1990-2001` = safe_mean(`Mean GDP pc 1990-2001`),
      `Mean GDP pc 2002-2019` = safe_mean(`Mean GDP pc 2002-2019`)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# Appendix 3, Panel A: WAEMU.
gdp_used_waemu <- make_gdp_used_table(
  analysis_data,
  country_order = waemu_table1,
  average_label = "Average for the WAEMU area"
)

# Additional WAEMU average excluding Guinea-Bissau.
gdp_used_waemu_excl_gnb <- gdp_used_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average for the WAEMU area"
  )) |>
  summarise(
    country = "Average without Guinea-Bissau",
    `Mean GDP pc 1990-2001` = safe_mean(`Mean GDP pc 1990-2001`),
    `Mean GDP pc 2002-2019` = safe_mean(`Mean GDP pc 2002-2019`)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

gdp_used_waemu <- bind_rows(
  gdp_used_waemu,
  gdp_used_waemu_excl_gnb
)

# Appendix 3, Panel B: CAEMC.
gdp_used_caemc <- make_gdp_used_table(
  analysis_data,
  country_order = caemc,
  average_label = "Average for the CAEMC zone"
)

# Appendix 4: Non-CFA comparison countries.
gdp_used_non_cfa <- make_gdp_used_table(
  analysis_data,
  country_order = non_cfa_comparison_countries,
  average_label = "Average"
)

print(gdp_used_waemu)
print(gdp_used_caemc)
print(gdp_used_non_cfa)

cat(
  kable(
    gdp_used_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "WAEMU countries' mean GDP per capita"
  ),
  file = "output/tables/appendix3_panelA_waemu_gdp_used.tex"
)

cat(
  kable(
    gdp_used_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CAEMC countries' mean GDP per capita"
  ),
  file = "output/tables/appendix3_panelB_caemc_gdp_used.tex"
)

cat(
  kable(
    gdp_used_non_cfa,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA countries' mean GDP per capita "
  ),
  file = "output/tables/appendix4_non_cfa_gdp_used.tex"
)
print("Data Download Complete")
