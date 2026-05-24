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
#### ======================= 11. SAVE PROCESSED DATA =======================###
#### ======================================================================###

saveRDS(df_imputed, "data/processed/processed_panel_imputed.rds")
saveRDS(df_before_imputation, "data/processed/processed_panel_unimputed.rds")

#### ========================================================================###
#### ======================== 12. INFLATION TABLES ==========================###
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
#### ======================== 13. GDP GROWTH GRAPH ==========================###
#### ========================================================================###

# Use the imputed panel for descriptive outputs so generated figures/tables are
# consistent with the saved processed panel.
analysis_data <- df_imputed

analysis_data <- analysis_data |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(growth = 100 * (wdi_gdp_pc_constant / lag(wdi_gdp_pc_constant) - 1)) |>
  ungroup()

df_growth <- analysis_data |>
  filter(region %in% c("WAEMU", "CAEMC", "Non-CFA comparison")) |>
  mutate(region_plot = recode(
    region,
    "Non-CFA comparison" = "Non-CFA countries"
  )) |>
  group_by(region_plot, year) |>
  summarise(growth = mean(growth, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1990, year <= 2021)

# Plot average annual GDP per capita growth by region. The vertical line marks
# 2002, the first post-treatment year.
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

ggsave(
  "output/figures/gdp_pc_growth_regions.png",
  p_gdp_growth,
  width = 12,
  height = 4,
  dpi = 300
)

#### ========================================================================###
#### ======================== 14. GDP GROWTH TABLES =========================###
#### ========================================================================###

# Compute annual GDP per capita growth for WDI constant and current per-capita
# GDP variables. These are descriptive appendix tables, not the SCM outcome.
df_gdp_growth <- analysis_data |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    growth_wdi_gdp_pc_constant = 100 * (wdi_gdp_pc_constant / lag(wdi_gdp_pc_constant) - 1),
    growth_wdi_gdp_pc_current = 100 * (wdi_gdp_pc_current / lag(wdi_gdp_pc_current) - 1)
  ) |>
  ungroup()

# Helper function for GDP growth appendix tables.
make_gdp_growth_table <- function(data, country_order, average_label) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      table_period = case_when(
        year >= 1990 & year <= 2001 ~ "1990-2001",
        year >= 2002 & year <= 2021 ~ "2002-2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(table_period)) |>
    group_by(iso3c, country, table_period) |>
    summarise(
      growth_wdi_gdp_pc_constant = safe_mean(growth_wdi_gdp_pc_constant),
      growth_wdi_gdp_pc_current = safe_mean(growth_wdi_gdp_pc_current),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = table_period,
      values_from = c(
        growth_wdi_gdp_pc_constant,
        growth_wdi_gdp_pc_current
      )
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(
      country,
      `constant GDP pc growth 1990-2001` = `growth_wdi_gdp_pc_constant_1990-2001`,
      `constant GDP pc growth 2002-2021` = `growth_wdi_gdp_pc_constant_2002-2021`,
      `current GDP pc growth 1990-2001` = `growth_wdi_gdp_pc_current_1990-2001`,
      `current GDP pc growth 2002-2021` = `growth_wdi_gdp_pc_current_2002-2021`
    )

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `constant GDP pc growth 1990-2001` = safe_mean(`constant GDP pc growth 1990-2001`),
      `constant GDP pc growth 2002-2021` = safe_mean(`constant GDP pc growth 2002-2021`),
      `current GDP pc growth 1990-2001` = safe_mean(`current GDP pc growth 1990-2001`),
      `current GDP pc growth 2002-2021` = safe_mean(`current GDP pc growth 2002-2021`)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# Appendix 3, Panel A: WAEMU.
gdp_growth_waemu <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = waemu_table1,
  average_label = "Average for the WAEMU area"
)

# Additional WAEMU average excluding Guinea-Bissau.
gdp_growth_waemu_excl_gnb <- gdp_growth_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average for the WAEMU area"
  )) |>
  summarise(
    country = "Average without Guinea-Bissau",
    `constant GDP pc growth 1990-2001` = safe_mean(`constant GDP pc growth 1990-2001`),
    `constant GDP pc growth 2002-2021` = safe_mean(`constant GDP pc growth 2002-2021`),
    `current GDP pc growth 1990-2001` = safe_mean(`current GDP pc growth 1990-2001`),
    `current GDP pc growth 2002-2021` = safe_mean(`current GDP pc growth 2002-2021`)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

gdp_growth_waemu <- bind_rows(
  gdp_growth_waemu,
  gdp_growth_waemu_excl_gnb
)

# Appendix 3, Panel B: CAEMC.
gdp_growth_caemc <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = caemc,
  average_label = "Average for the CAEMC zone"
)

# Appendix 4: Non-CFA comparison countries.
gdp_growth_non_cfa <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = non_cfa_comparison_countries,
  average_label = "Average"
)

print(gdp_growth_waemu)
print(gdp_growth_caemc)
print(gdp_growth_non_cfa)

cat(
  kable(
    gdp_growth_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "WAEMU countries' mean annual GDP per capita growth: WDI constant and current GDP per capita"
  ),
  file = "output/tables/appendix3_panelA_waemu_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CAEMC countries' mean annual GDP per capita growth: WDI constant and current GDP per capita"
  ),
  file = "output/tables/appendix3_panelB_caemc_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_non_cfa,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA countries' mean annual GDP per capita growth: WDI constant and current GDP per capita"
  ),
  file = "output/tables/appendix4_non_cfa_gdp_growth.tex"
)

print("Complete")
