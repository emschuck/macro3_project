
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
    gdp_pc_current,
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
    gdp_pc_current,
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
special_var <- "gdp_pc_current"
special_years <- c(1980, 1995, 2001)







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

    dependent = "gdp_pc_current",

    unit.variable = 1,
    unit.names.variable = 2,
    time.variable = 3,

    treatment.identifier = treated_id,
    controls.identifier = control_ids,

    time.predictors.prior = 1980:2001,
    time.optimize.ssr = 1980:2001,

    time.plot = 1980:2019,

    special.predictors = list(
      list("gdp_pc_current", 1980, c("mean")),
      list("gdp_pc_current", 1995, c("mean")),
      list("gdp_pc_current", 2001, c("mean"))
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
    values_to = "gdp_pc_current"
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
  aes(x = year, y = gdp_pc_current, linetype = series)
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

print("Complete")