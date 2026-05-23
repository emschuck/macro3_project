
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
library(stringr)

library(Synth)

## Safe mean function wit hnas
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Load processed data
df <- readRDS("data/processed/processed_panel_imputed.rds")

## SELECTED GDP VARIABLE
gdp_var <- "pwt_rgdpo_pc"
# gdp_var_label <- "PWT output-side real GDP per capita, chained PPPs"

df <- df |>
  mutate(
    gdp_selected = .data[[gdp_var]]
  )
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



# Country names for charts
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

# Function to get country name form list
get_country_name <- function(code) {
  country_names |>
    filter(iso3c == code) |>
    pull(country)
}




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
    gdp_selected,
    agriculture, industry, govt_share, invest_share,
    oda_share, fdi, labour, polity2
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
  tidyr::fill(country, .direction = "downup") |> 
  # fill country name from above/below for missing
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
    bad_oda         = all(is.na(oda_share)),
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
treated_iso3c <- "GNQ"   # to change

country_name_treated <- get_country_name(treated_iso3c)


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
    gdp_selected,
    agriculture,
    industry,
    govt_share,
    invest_share,
    oda_share,
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
  



# Variables and years used as special predictors
special_var <- "gdp_selected"
special_years <- c(1980, 1985, 1990, 1995, 2001)


# =====================================================
# Function to run SCM for one treated country
# =====================================================

run_scm_country <- function(treated_iso3c) {

  message("Running SCM for ", treated_iso3c)

  # Get country name for labels
  country_name_treated <- get_country_name(treated_iso3c)

  # Define treated-country ID
  treated_id <- country_ids$unit_id[
    country_ids$iso3c == treated_iso3c
  ]

  # Define usable donor-country IDs
  control_ids <- country_ids$unit_id[
    country_ids$iso3c %in% donor_countries &
      !(country_ids$unit_id %in% bad_controls)
  ]

  # Prepare data for Synth
  dataprep.out <- dataprep(
    foo = scm_df,

    predictors = c(
      "agriculture",
      "industry",
      "govt_share",
      "invest_share",
      "oda_share",
      "fdi",
      "labour",
      "polity2"
    ),

    predictors.op = "mean",

    dependent = "gdp_selected",

    unit.variable = "unit_id", #1
    unit.names.variable = "unit_name", #2
    time.variable = "year",#3

    treatment.identifier = treated_id,
    controls.identifier = control_ids,

    time.predictors.prior = 1980:2001,
    time.optimize.ssr = 1980:2001,

    time.plot = 1980:2019,

    special.predictors = list(
      list("gdp_selected", 1980, c("mean")),
      list("gdp_selected", 1985, c("mean")),
      list("gdp_selected", 1990, c("mean")),
      list("gdp_selected", 1995, c("mean")),
      list("gdp_selected", 2001, c("mean"))
    )
  )

  # Estimate synthetic control
  synth.out <- synth(
    data.prep.obj = dataprep.out
    #optimxmethod='All'
    #optimxmethod = c("Nelder-Mead", "BFGS")
    #genoud = TRUE
    #quadopt = "ipop"
    #Margin.ipop = 5e-04
  )

  # Create summary tables
  synth.tables <- synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out
  )

  # Save path plot
  png(
    paste0("output/figures/scm_path_", treated_iso3c, ".png"),
    width = 900,
    height = 600
  )

  path.plot(
    synth.res = synth.out,
    dataprep.res = dataprep.out,
    tr.intake = 2002,
    Ylab = paste0("Real GDP per capita: ", country_name_treated),
    Xlab = "Year",
    Legend = c(
      country_name_treated,
      paste("Synthetic", country_name_treated)
    ),
    Legend.position = "topleft"
  )

  dev.off()

  # Save gap plot
  png(
    paste0("output/figures/scm_gap_", treated_iso3c, ".png"),
    width = 900,
    height = 600
  )

  gaps.plot(
    synth.res = synth.out,
    dataprep.res = dataprep.out,
    tr.intake = 2002,
    Ylab = paste0("Gap in real GDP per capita: ", country_name_treated),
    Xlab = "Year"
  )

  dev.off()

  # Save donor weights
  write.csv(
    synth.tables$tab.w,
    paste0("output/tables/scm_weights_", treated_iso3c, ".csv"),
    row.names = FALSE
  )

  # Save predictor balance
  write.csv(
    synth.tables$tab.pred,
    paste0("output/tables/scm_predictor_balance_", treated_iso3c, ".csv"),
    row.names = TRUE
  )

  # Return results
  list(
    treated_iso3c = treated_iso3c,
    country_name = country_name_treated,
    dataprep = dataprep.out,
    synth = synth.out,
    tables = synth.tables
  )
}


# =====================================================
# Run SCM for all treated countries
# =====================================================

scm_results <- list()

for (country_code in treated_countries) {

  result <- tryCatch(
    run_scm_country(country_code),
    error = function(e) {
      message("SCM failed for ", country_code, ": ", e$message)
      return(NULL)
    }
  )
  scm_results[[country_code]] <- result
}


# =====================================================
# Synthetic control weights table
# Donor countries as rows, treated countries as columns
# =====================================================


# Optional: short labels for treated-country columns
treated_labels <- tibble::tribble(
  ~iso3c, ~treated_label,
  "BEN", "Benin",
  "BFA", "Burkina F.",
  "CIV", "Ivory Coast",
  "MLI", "Mali",
  "NER", "Niger",
  "SEN", "Senegal",
  "TGO", "Togo",
  "CMR", "Cameroon",
  "CAF", "CAR",
  "TCD", "Chad",
  "COG", "Congo",
  "GAB", "Gabon",
  "GNQ", "E. Guinea"
)

# Optional: donor labels for rows
donor_labels <- tibble::tribble(
  ~unit.names, ~donor_label,
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
  "LAO", "Laos",
  "LSO", "Lesotho",
  "MUS", "Mauritius",
  "MAR", "Morocco",
  "NAM", "Namibia",
  "OMN", "Oman",
  "PAN", "Panama",
  "KNA", "Saint Kitts and N.",
  "LCA", "Saint Lucia",
  "SYC", "Seychelles"
)

# Extract weights from each successful SCM result
weights_long <- bind_rows(
  lapply(names(scm_results), function(treated_code) {

    result <- scm_results[[treated_code]]

    if (is.null(result)) {
      return(NULL)
    }

    result$tables$tab.w |>
      as.data.frame() |>
      mutate(
        treated_iso3c = treated_code
      )
  })
)

# Clean tiny numerical weights for reporting
# Change threshold if needed. 1e-4 means values below 0.0001 become 0.
weight_threshold <- 1e-4

weights_long_clean <- weights_long |>
  mutate(
    w_clean = ifelse(w.weights < weight_threshold, 0, w.weights)
  ) |>
  left_join(treated_labels, by = c("treated_iso3c" = "iso3c")) |>
  left_join(donor_labels, by = "unit.names") |>
  mutate(
    treated_label = ifelse(is.na(treated_label), treated_iso3c, treated_label),
    donor_label = ifelse(is.na(donor_label), unit.names, donor_label)
  )

# Make wide table
weights_wide <- weights_long_clean |>
  select(donor_label, treated_label, w_clean) |>
  pivot_wider(
    names_from = treated_label,
    values_from = w_clean,
    values_fill = 0
  )

# Put donor rows in the same order as donor_countries
weights_wide <- donor_labels |>
  select(donor_label) |>
  left_join(weights_wide, by = "donor_label") |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))

# Round for display
weights_wide_display <- weights_wide |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(weights_wide_display, n = Inf)

# Save CSV
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  weights_wide_display,
  "output/tables/scm_weights_all_treated_wide.csv",
  row.names = FALSE
)

weights_latex_display <- weights_wide_display |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x == 0, "0", sub("^0", "", sprintf("%.3f", .x)))
    )
  )

cat(
  kable(
    weights_latex_display,
    format = "latex",
    booktabs = TRUE,
    caption = "Estimated synthetic control weights by treated country",
    col.names = c("Donor country", names(weights_latex_display)[-1]),
    escape = FALSE
  ),
  file = "output/tables/scm_weights_all_treated_wide_formatted.tex"
)

# =====================================================
# Combine SCM paths and make multi-plots
# =====================================================

extract_scm_path <- function(result) {
  
  if (is.null(result)) {
    return(NULL)
  }
  
  dataprep.out <- result$dataprep
  synth.out <- result$synth
  treated_iso3c <- result$treated_iso3c
  country_name <- result$country_name
  
  actual <- as.numeric(dataprep.out$Y1plot)
  synthetic <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)
  years <- as.numeric(rownames(dataprep.out$Y1plot))
  
  data.frame(
    iso3c = treated_iso3c,
    country = country_name,
    year = years,
    actual = actual,
    synthetic = synthetic,
    gap = actual - synthetic
  )
}

scm_paths <- dplyr::bind_rows(
  lapply(scm_results, extract_scm_path)
)

saveRDS(
  scm_paths,
  "data/processed/scm_paths_all_treated.rds"
)

write.csv(
  scm_paths,
  "output/tables/scm_paths_all_treated.csv",
  row.names = FALSE
)


# =====================================================
# Multi-plot: actual vs synthetic GDP per capita
# =====================================================

scm_paths_long <- scm_paths |>
  tidyr::pivot_longer(
    cols = c(actual, synthetic),
    names_to = "series",
    values_to = "gdp_selected"
  ) |>
  mutate(
    series = dplyr::recode(
      series,
      actual = "Actual",
      synthetic = "Synthetic"
    )
  )

p_paths_all <- ggplot(
  scm_paths_long,
  aes(x = year, y = gdp_selected, linetype = series)
) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = 2002, linetype = "dashed") +
  facet_wrap(~ country, scales = "free_y", ncol = 4) +
  labs(
    title = "Actual and synthetic GDP per capita: WAEMU and CAEMC countries",
    x = NULL,
    y = "GDP per capita",
    linetype = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/figures/scm_paths_all_treated.png",
  plot = p_paths_all,
  width = 14,
  height = 10,
  dpi = 300
)

# =====================================================
# Multi-plot: actual minus synthetic gaps
# =====================================================

p_gaps_all <- ggplot(
  scm_paths,
  aes(x = year, y = gap)
) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2002, linetype = "dashed") +
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  labs(
    title = "GDP per capita gaps: WAEMU and CAEMC countries",
    x = NULL,
    y = "Actual - synthetic"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "output/figures/scm_gaps_all_treated.png",
  plot = p_gaps_all,
  width = 20,
  height = 10,
  dpi = 300
)




# =====================================================
# Export SCM data in wide form only
# =====================================================

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Keep only successful SCM results
scm_success <- scm_results[!vapply(scm_results, is.null, logical(1))]

if (length(scm_success) == 0) {
  stop("No successful SCM results found. scm_results contains only NULL values.")
}

# Variables to export: outcome plus SCM predictors
scm_export_vars <- c(
  "gdp_selected",
  "agriculture",
  "industry",
  "govt_share",
  "invest_share",
  "oda_share",
  "fdi",
  "labour",
  "polity2"
)

# Keep only variables that actually exist in scm_df
scm_export_vars <- scm_export_vars[scm_export_vars %in% names(scm_df)]

if (length(scm_export_vars) == 0) {
  stop("None of the requested export variables are present in scm_df.")
}

extract_weights_for_country <- function(result) {
  result$tables$tab.w |>
    as.data.frame() |>
    transmute(
      treated_iso3c = result$treated_iso3c,
      donor_iso3c = as.character(unit.names),
      donor_weight = as.numeric(w.weights)
    )
}

make_export_for_country <- function(result, data, vars_to_export) {

  treated_code <- result$treated_iso3c
  treated_name <- result$country_name

  weights_i <- extract_weights_for_country(result)

  treated_actual <- data |>
    filter(
      iso3c == treated_code,
      year >= 1980,
      year <= 2019
    ) |>
    select(
      iso3c,
      country,
      year,
      all_of(vars_to_export)
    ) |>
    pivot_longer(
      cols = all_of(vars_to_export),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(
      treated_iso3c = treated_code,
      treated_country = treated_name,
      unit_type = "treated_actual",
      donor_iso3c = NA_character_,
      donor_country = NA_character_,
      donor_weight = NA_real_
    ) |>
    select(
      treated_iso3c,
      treated_country,
      unit_type,
      iso3c,
      country,
      year,
      variable,
      value,
      donor_iso3c,
      donor_country,
      donor_weight
    )

  donor_real <- data |>
    filter(
      iso3c %in% weights_i$donor_iso3c,
      year >= 1980,
      year <= 2019
    ) |>
    select(
      iso3c,
      country,
      year,
      all_of(vars_to_export)
    ) |>
    left_join(
      weights_i,
      by = c("iso3c" = "donor_iso3c")
    ) |>
    pivot_longer(
      cols = all_of(vars_to_export),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(
      treated_country = treated_name,
      unit_type = "donor_real",
      donor_iso3c = iso3c,
      donor_country = country
    ) |>
    select(
      treated_iso3c,
      treated_country,
      unit_type,
      iso3c,
      country,
      year,
      variable,
      value,
      donor_iso3c,
      donor_country,
      donor_weight
    )

  synthetic_values <- donor_real |>
    group_by(
      treated_iso3c,
      treated_country,
      year,
      variable
    ) |>
    summarise(
      value = ifelse(
        all(is.na(value)),
        NA_real_,
        sum(donor_weight * value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(
      unit_type = "synthetic",
      iso3c = paste0("SYN_", treated_code),
      country = paste("Synthetic", treated_name),
      donor_iso3c = NA_character_,
      donor_country = NA_character_,
      donor_weight = NA_real_
    ) |>
    select(
      treated_iso3c,
      treated_country,
      unit_type,
      iso3c,
      country,
      year,
      variable,
      value,
      donor_iso3c,
      donor_country,
      donor_weight
    )

  bind_rows(
    treated_actual,
    synthetic_values,
    donor_real
  )
}

# Temporary long object
scm_export_long <- bind_rows(
  lapply(
    scm_success,
    make_export_for_country,
    data = scm_df,
    vars_to_export = scm_export_vars
  )
)

# Final wide export
scm_export_wide <- scm_export_long |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  arrange(
    treated_iso3c,
    unit_type,
    iso3c,
    year
  )

saveRDS(
  scm_export_wide,
  "data/processed/scm_export_wide.rds"
)














# # =====================================================
# # In-place placebo analysis for SCM
# # =====================================================

# # This placebo analysis follows the in-place / in-space approach:
# # for each treated CFA country, each donor country is treated as if it
# # experienced treatment in 2002. The true treated-country gap is then
# # compared with the distribution of placebo gaps.



# scm_predictors <- c(
#   "agriculture",
#   "industry",
#   "govt_share",
#   "invest_share",
#   "oda_share",
#   "fdi",
#   "labour",
#   "polity2"
# )

# pre_period <- 1980:2001
# post_period <- 2002:2019
# plot_period <- 1980:2019


# outcome_var <- "gdp_selected"
# special_predictor_years <- special_years

# # =====================================================
# # 2. Helper functions
# # =====================================================

# rmspe <- function(x) {
#   sqrt(mean(x^2, na.rm = TRUE))
# }

# safe_ratio <- function(numerator, denominator) {
#   ifelse(is.na(denominator) | denominator == 0, NA_real_, numerator / denominator)
# }

# extract_gap_path <- function(dataprep.out, synth.out, unit_code, unit_name, unit_type) {

#   actual <- as.numeric(dataprep.out$Y1plot)
#   synthetic <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)
#   years <- as.numeric(rownames(dataprep.out$Y1plot))

#   tibble(
#     unit_iso3c = unit_code,
#     unit_name = unit_name,
#     unit_type = unit_type,
#     year = years,
#     actual = actual,
#     synthetic = synthetic,
#     gap = actual - synthetic
#   )
# }

# make_gap_summary <- function(gap_data) {

#   gap_data |>
#     summarise(
#       pre_rmspe = rmspe(gap[year %in% pre_period]),
#       post_rmspe = rmspe(gap[year %in% post_period]),
#       rmspe_ratio = safe_ratio(post_rmspe, pre_rmspe),
#       avg_post_gap = mean(gap[year %in% post_period], na.rm = TRUE),
#       avg_abs_post_gap = mean(abs(gap[year %in% post_period]), na.rm = TRUE),
#       max_abs_post_gap = max(abs(gap[year %in% post_period]), na.rm = TRUE),
#       .groups = "drop"
#     )
# }

# run_placebo_unit <- function(placebo_iso3c, donor_pool_ids) {

#   placebo_id <- country_ids$unit_id[
#     country_ids$iso3c == placebo_iso3c
#   ]

#   placebo_controls <- donor_pool_ids[
#     donor_pool_ids != placebo_id
#   ]

#   placebo_name <- get_country_name(placebo_iso3c)

#   dataprep.out <- dataprep(
#     foo = scm_df,

#     predictors = scm_predictors,
#     predictors.op = "mean",

#     dependent = outcome_var,

#     unit.variable = 1,
#     unit.names.variable = 2,
#     time.variable = 3,

#     treatment.identifier = placebo_id,
#     controls.identifier = placebo_controls,

#     time.predictors.prior = pre_period,
#     time.optimize.ssr = pre_period,
#     time.plot = plot_period,

#     special.predictors = list(
#       list(outcome_var, special_predictor_years[1], c("mean")),
#       list(outcome_var, special_predictor_years[2], c("mean")),
#       list(outcome_var, special_predictor_years[3], c("mean"))
#     )
#   )

#   synth.out <- synth(
#     data.prep.obj = dataprep.out
#   )

#   extract_gap_path(
#     dataprep.out = dataprep.out,
#     synth.out = synth.out,
#     unit_code = placebo_iso3c,
#     unit_name = placebo_name,
#     unit_type = "placebo"
#   )
# }


# # =====================================================
# # 3. Define usable donor pool for placebo analysis
# # =====================================================

# usable_donor_ids <- country_ids$unit_id[
#   country_ids$iso3c %in% donor_countries &
#     !(country_ids$unit_id %in% bad_controls)
# ]

# usable_donor_codes <- country_ids |>
#   filter(unit_id %in% usable_donor_ids) |>
#   pull(iso3c)



# # =====================================================
# # 4. Run placebo analysis for each treated country
# # =====================================================

# all_placebo_paths <- list()
# all_placebo_summaries <- list()
# all_placebo_yearly_pvalues <- list()
# all_placebo_conclusions <- list()

# for (treated_code in treated_countries) {

#   cat("\n=====================================================\n")
#   cat("PLACEBO ANALYSIS FOR", treated_code, "-", get_country_name(treated_code), "\n")
#   cat("=====================================================\n")

#   real_result <- scm_results[[treated_code]]

#   if (is.null(real_result)) {
#     cat("Skipping", treated_code, "- no successful SCM result available.\n")
#     next
#   }

#   # Extract true treated-country gap from already-estimated SCM result
#   real_gap <- extract_gap_path(
#     dataprep.out = real_result$dataprep,
#     synth.out = real_result$synth,
#     unit_code = treated_code,
#     unit_name = real_result$country_name,
#     unit_type = "treated"
#   )

#   real_summary <- make_gap_summary(real_gap) |>
#     mutate(
#       treated_iso3c = treated_code,
#       treated_country = real_result$country_name,
#       placebo_iso3c = treated_code,
#       placebo_country = real_result$country_name,
#       unit_type = "treated"
#     )

#   # Run placebos: each usable donor is treated as if treated in 2002
#   placebo_paths_i <- list()

#   for (placebo_code in usable_donor_codes) {

#     placebo_paths_i[[placebo_code]] <- tryCatch(
#       run_placebo_unit(
#         placebo_iso3c = placebo_code,
#         donor_pool_ids = usable_donor_ids
#       ),
#       error = function(e) {
#         message("Placebo failed for ", treated_code, " / ", placebo_code, ": ", e$message)
#         return(NULL)
#       }
#     )
#   }

#   placebo_paths_i <- placebo_paths_i[
#     !vapply(placebo_paths_i, is.null, logical(1))
#   ]

#   if (length(placebo_paths_i) == 0) {
#     cat("No successful placebo runs for", treated_code, "\n")
#     next
#   }

#   placebo_paths_i <- bind_rows(placebo_paths_i)

#   placebo_summaries_i <- placebo_paths_i |>
#     group_by(unit_iso3c, unit_name, unit_type) |>
#     group_modify(~ make_gap_summary(.x)) |>
#     ungroup() |>
#     mutate(
#       treated_iso3c = treated_code,
#       treated_country = real_result$country_name,
#       placebo_iso3c = unit_iso3c,
#       placebo_country = unit_name
#     ) |>
#     select(
#       treated_iso3c,
#       treated_country,
#       placebo_iso3c,
#       placebo_country,
#       unit_type,
#       pre_rmspe,
#       post_rmspe,
#       rmspe_ratio,
#       avg_post_gap,
#       avg_abs_post_gap,
#       max_abs_post_gap
#     )

#   summary_i <- bind_rows(
#     real_summary |>
#       select(
#         treated_iso3c,
#         treated_country,
#         placebo_iso3c,
#         placebo_country,
#         unit_type,
#         pre_rmspe,
#         post_rmspe,
#         rmspe_ratio,
#         avg_post_gap,
#         avg_abs_post_gap,
#         max_abs_post_gap
#       ),
#     placebo_summaries_i
#   )

#   # -----------------------------------------------------
#   # Rank-based p-values
#   # -----------------------------------------------------

#   real_ratio <- real_summary$rmspe_ratio[1]
#   real_avg_abs_gap <- real_summary$avg_abs_post_gap[1]
#   real_avg_gap <- real_summary$avg_post_gap[1]

#   n_units <- nrow(summary_i)

#   # Two-sided RMSPE-ratio p-value:
#   # share of treated + placebo units with ratio at least as large as true treated ratio.
#   p_rmspe_ratio <- mean(summary_i$rmspe_ratio >= real_ratio, na.rm = TRUE)

#   # Two-sided average absolute post-gap p-value.
#   p_avg_abs_gap <- mean(summary_i$avg_abs_post_gap >= real_avg_abs_gap, na.rm = TRUE)

#   # One-sided p-value based on direction of treated average post-gap.
#   if (real_avg_gap >= 0) {
#     p_avg_gap_directional <- mean(summary_i$avg_post_gap >= real_avg_gap, na.rm = TRUE)
#     direction_text <- "positive"
#   } else {
#     p_avg_gap_directional <- mean(summary_i$avg_post_gap <= real_avg_gap, na.rm = TRUE)
#     direction_text <- "negative"
#   }

#   rank_rmspe <- rank(
#     -summary_i$rmspe_ratio,
#     ties.method = "min"
#   )[summary_i$unit_type == "treated"]

#   rank_abs_gap <- rank(
#     -summary_i$avg_abs_post_gap,
#     ties.method = "min"
#   )[summary_i$unit_type == "treated"]

#   # -----------------------------------------------------
#   # Year-specific placebo p-values
#   # -----------------------------------------------------

#   combined_paths_i <- bind_rows(
#     real_gap,
#     placebo_paths_i
#   ) |>
#     mutate(
#       treated_iso3c = treated_code,
#       treated_country = real_result$country_name
#     )

#   real_yearly <- combined_paths_i |>
#     filter(unit_type == "treated") |>
#     select(year, treated_gap = gap)

#   placebo_yearly <- combined_paths_i |>
#     filter(unit_type == "placebo") |>
#     select(year, unit_iso3c, placebo_gap = gap)

#   yearly_pvalues_i <- placebo_yearly |>
#     left_join(real_yearly, by = "year") |>
#     group_by(year) |>
#     summarise(
#       treated_iso3c = treated_code,
#       treated_country = real_result$country_name,
#       treated_gap = first(treated_gap),

#       # Two-sided p-value using absolute gaps.
#       p_abs_gap = mean(abs(c(treated_gap[1], placebo_gap)) >= abs(treated_gap[1]), na.rm = TRUE),

#       # One-sided p-value using the sign of treated gap.
#       p_directional_gap = ifelse(
#         treated_gap[1] >= 0,
#         mean(c(treated_gap[1], placebo_gap) >= treated_gap[1], na.rm = TRUE),
#         mean(c(treated_gap[1], placebo_gap) <= treated_gap[1], na.rm = TRUE)
#       ),

#       n_placebos = sum(!is.na(placebo_gap)),
#       .groups = "drop"
#     ) |>
#     mutate(
#       significant_10pct_abs = p_abs_gap <= 0.10,
#       significant_5pct_abs = p_abs_gap <= 0.05,
#       significant_10pct_directional = p_directional_gap <= 0.10,
#       significant_5pct_directional = p_directional_gap <= 0.05
#     )

#   # -----------------------------------------------------
#   # Printed conclusion
#   # -----------------------------------------------------

#   conclusion_i <- tibble(
#     treated_iso3c = treated_code,
#     treated_country = real_result$country_name,
#     n_successful_placebos = nrow(placebo_summaries_i),
#     n_units_ranked = n_units,
#     pre_rmspe = real_summary$pre_rmspe[1],
#     post_rmspe = real_summary$post_rmspe[1],
#     rmspe_ratio = real_ratio,
#     rmspe_ratio_rank = rank_rmspe,
#     p_rmspe_ratio = p_rmspe_ratio,
#     avg_post_gap = real_avg_gap,
#     avg_abs_post_gap = real_avg_abs_gap,
#     avg_abs_gap_rank = rank_abs_gap,
#     p_avg_abs_gap = p_avg_abs_gap,
#     p_avg_gap_directional = p_avg_gap_directional,
#     avg_gap_direction = direction_text,
#     conclusion_10pct = p_rmspe_ratio <= 0.10,
#     conclusion_5pct = p_rmspe_ratio <= 0.05
#   )

#   cat("Successful placebo countries:", nrow(placebo_summaries_i), "\n")
#   cat("Treated pre-treatment RMSPE:", round(real_summary$pre_rmspe[1], 4), "\n")
#   cat("Treated post-treatment RMSPE:", round(real_summary$post_rmspe[1], 4), "\n")
#   cat("Treated post/pre RMSPE ratio:", round(real_ratio, 4), "\n")
#   cat("RMSPE-ratio rank:", rank_rmspe, "out of", n_units, "\n")
#   cat("RMSPE-ratio placebo p-value:", round(p_rmspe_ratio, 4), "\n")
#   cat("Average post-treatment gap:", round(real_avg_gap, 4), "\n")
#   cat("Average absolute post-treatment gap:", round(real_avg_abs_gap, 4), "\n")
#   cat("Average absolute gap placebo p-value:", round(p_avg_abs_gap, 4), "\n")
#   cat("Directional average gap p-value:", round(p_avg_gap_directional, 4), "\n")

#   if (p_rmspe_ratio <= 0.05) {
#     cat("Conclusion: RMSPE-ratio evidence is significant at the 5% level.\n")
#   } else if (p_rmspe_ratio <= 0.10) {
#     cat("Conclusion: RMSPE-ratio evidence is significant at the 10% level, but not the 5% level.\n")
#   } else {
#     cat("Conclusion: RMSPE-ratio evidence is not significant at the 10% level.\n")
#   }

#   if (real_avg_gap > 0) {
#     cat("Interpretation: the treated country is above its synthetic control on average after 2002.\n")
#   } else if (real_avg_gap < 0) {
#     cat("Interpretation: the treated country is below its synthetic control on average after 2002.\n")
#   } else {
#     cat("Interpretation: the treated country has zero average post-treatment gap.\n")
#   }

#   # Store results
#   all_placebo_paths[[treated_code]] <- combined_paths_i
#   all_placebo_summaries[[treated_code]] <- summary_i
#   all_placebo_yearly_pvalues[[treated_code]] <- yearly_pvalues_i
#   all_placebo_conclusions[[treated_code]] <- conclusion_i
# }


# # =====================================================
# # 5. Combine and save placebo outputs
# # =====================================================

# placebo_paths_all <- bind_rows(all_placebo_paths)
# placebo_summaries_all <- bind_rows(all_placebo_summaries)
# placebo_yearly_pvalues_all <- bind_rows(all_placebo_yearly_pvalues)
# placebo_conclusions_all <- bind_rows(all_placebo_conclusions)

# saveRDS(
#   placebo_paths_all,
#   "data/processed/scm_placebo_paths_all.rds"
# )

# saveRDS(
#   placebo_summaries_all,
#   "data/processed/scm_placebo_summaries_all.rds"
# )

# saveRDS(
#   placebo_yearly_pvalues_all,
#   "data/processed/scm_placebo_yearly_pvalues_all.rds"
# )

# saveRDS(
#   placebo_conclusions_all,
#   "data/processed/scm_placebo_conclusions_all.rds"
# )

# write.csv(
#   placebo_summaries_all,
#   "output/tables/scm_placebo_summaries_all.csv",
#   row.names = FALSE
# )

# write.csv(
#   placebo_yearly_pvalues_all,
#   "output/tables/scm_placebo_yearly_pvalues_all.csv",
#   row.names = FALSE
# )

# write.csv(
#   placebo_conclusions_all,
#   "output/tables/scm_placebo_conclusions_all.csv",
#   row.names = FALSE
# )


# # =====================================================
# # 6. Print overall conclusion table
# # =====================================================

# cat("\n=====================================================\n")
# cat("OVERALL PLACEBO CONCLUSIONS\n")
# cat("=====================================================\n")

# placebo_conclusions_all |>
#   arrange(p_rmspe_ratio) |>
#   mutate(
#     p_rmspe_ratio = round(p_rmspe_ratio, 4),
#     rmspe_ratio = round(rmspe_ratio, 4),
#     avg_post_gap = round(avg_post_gap, 4),
#     avg_abs_post_gap = round(avg_abs_post_gap, 4),
#     conclusion = case_when(
#       p_rmspe_ratio <= 0.05 ~ "Significant at 5%",
#       p_rmspe_ratio <= 0.10 ~ "Significant at 10%",
#       TRUE ~ "Not significant at 10%"
#     )
#   ) |>
#   select(
#     treated_iso3c,
#     treated_country,
#     n_successful_placebos,
#     rmspe_ratio,
#     rmspe_ratio_rank,
#     p_rmspe_ratio,
#     avg_post_gap,
#     avg_gap_direction,
#     conclusion
#   ) |>
#   print(n = Inf)


# # =====================================================
# # 7. Plot placebo gaps for each treated country
# # =====================================================

# for (treated_code in unique(placebo_paths_all$treated_iso3c)) {

#   plot_data_i <- placebo_paths_all |>
#     filter(treated_iso3c == treated_code)

#   treated_name_i <- unique(plot_data_i$treated_country)

#   p_i <- ggplot() +
#     geom_line(
#       data = plot_data_i |> filter(unit_type == "placebo"),
#       aes(x = year, y = gap, group = unit_iso3c),
#       alpha = 0.35,
#       linewidth = 0.5
#     ) +
#     geom_line(
#       data = plot_data_i |> filter(unit_type == "treated"),
#       aes(x = year, y = gap),
#       linewidth = 1.2
#     ) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     geom_vline(xintercept = 2002, linetype = "dashed") +
#     labs(
#       title = paste0("In-place placebo gaps: ", treated_name_i, " (", treated_code, ")"),
#       subtitle = "Black line is treated country; grey lines are donor-country placebos",
#       x = NULL,
#       y = "Actual - synthetic"
#     ) +
#     theme_minimal(base_size = 11)

#   ggsave(
#     filename = paste0("output/figures/scm_placebo_gaps_", treated_code, ".png"),
#     plot = p_i,
#     width = 10,
#     height = 6,
#     dpi = 300
#   )
# }


# # =====================================================
# # 8. Optional combined p-value chart
# # =====================================================

# p_placebo_pvalues <- placebo_conclusions_all |>
#   mutate(
#     treated_country = factor(
#       treated_country,
#       levels = treated_country[order(p_rmspe_ratio)]
#     )
#   ) |>
#   ggplot(aes(x = treated_country, y = p_rmspe_ratio)) +
#   geom_col() +
#   geom_hline(yintercept = 0.10, linetype = "dashed") +
#   geom_hline(yintercept = 0.05, linetype = "dotted") +
#   coord_flip() +
#   labs(
#     title = "Placebo p-values based on post/pre RMSPE ratios",
#     subtitle = "Dashed line = 10%; dotted line = 5%",
#     x = NULL,
#     y = "Placebo p-value"
#   ) +
#   theme_minimal(base_size = 11)

# ggsave(
#   filename = "output/figures/scm_placebo_pvalues_rmspe_ratio.png",
#   plot = p_placebo_pvalues,
#   width = 9,
#   height = 6,
#   dpi = 300
# )

# cat("\nPlacebo analysis complete.\n")



print("Complete")