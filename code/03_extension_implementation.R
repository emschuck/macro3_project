#### ========================================================================###
#### ================= EXTENSION: TRADE VARIABLES SCM =======================###
#### ========================================================================###

# This script runs synthetic control analysis for trade-extension outcomes.
# It assumes that trade variables have already been added to the processed panel.
# Placebo analysis is kept in a separate file.
# Outputs are combined charts and summary tables only.

#### ========================================================================###
#### ======================== 1. PROJECT SETUP ==============================###
#### ========================================================================###

# Data manipulation and plotting packages.
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(stringr)
library(knitr)

# Synthetic control package.
library(Synth)

# Load shared project constants.
constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
  stop("Constants file not found: ", constants_file)
}

source(constants_file)

# Check that the constants file contains all objects used in this script.
# This script does not define fallbacks, so missing constants should stop execution.
required_constants <- c(
  "waemu",
  "caemc",
  "treated_countries",
  "donor_countries",
  "non_cfa_comparison_countries",
  "country_names",
  "get_country_name",
  "extension_pre_period",
  "extension_post_period",
  "extension_plot_period",
  "treatment_year",
  "extension_trade_outcomes",
  "extension_trade_outcome_labels",
  "extension_scm_predictors",
  "extension_output_dir",
  "extension_processed_dir",
  "extension_special_years"
)

missing_constants <- required_constants[
  !vapply(required_constants, exists, logical(1))
]

if (length(missing_constants) > 0) {
  stop(
    "These required objects are missing from the constants file: ",
    paste(missing_constants, collapse = ", ")
  )
}

# Create extension output folders if they do not already exist.
# Figures, tables, and processed R objects are stored separately.
dir.create(file.path(extension_output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(extension_output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(extension_processed_dir, recursive = TRUE, showWarnings = FALSE)


#### ========================================================================###
#### ======================== 2. LOAD DATA ==================================###
#### ========================================================================###

# Load the project processed panel.
# This file should already contain:
#   - trade-extension outcomes: XEU, MEU, trade_openness, exports_gdp, imports_gdp
#   - SCM predictors: agriculture, government share, investment share, ODA, FDI, labour, polity2
df <- readRDS("data/processed/processed_panel_imputed.rds")

# Check that all extension outcomes are available in the processed panel.
# If this fails, update the WDI download and data-loading scripts first.
missing_trade_outcomes <- setdiff(extension_trade_outcomes, names(df))

if (length(missing_trade_outcomes) > 0) {
  stop(
    "These extension trade outcomes are missing from df: ",
    paste(missing_trade_outcomes, collapse = ", "),
    ". Add them in the WDI download / data-loading scripts first."
  )
}

# Check that all SCM predictors are available.
# These are used to build the synthetic controls.
missing_predictors <- setdiff(extension_scm_predictors, names(df))

if (length(missing_predictors) > 0) {
  stop(
    "These extension SCM predictors are missing from df: ",
    paste(missing_predictors, collapse = ", ")
  )
}

# Countries used for SCM estimation.
# SCM uses only treated countries and donor countries.
scm_countries <- unique(c(treated_countries, donor_countries))

# Countries used for descriptive group-average plots.
# These plots show four groups:
#   1. WAEMU
#   2. CAEMC
#   3. Donor pool
#   4. Non-donor comparison countries
descriptive_countries <- unique(c(
  waemu,
  caemc,
  donor_countries,
  non_cfa_comparison_countries
))

# Warn if any selected SCM countries are missing from the processed panel.
missing_from_data <- setdiff(scm_countries, unique(df$iso3c))

if (length(missing_from_data) > 0) {
  warning(
    "These selected SCM countries are not present in df$iso3c: ",
    paste(missing_from_data, collapse = ", ")
  )
}

# Warn if any descriptive countries are missing from the processed panel.
missing_from_descriptive_data <- setdiff(descriptive_countries, unique(df$iso3c))

if (length(missing_from_descriptive_data) > 0) {
  warning(
    "These descriptive countries are not present in df$iso3c: ",
    paste(missing_from_descriptive_data, collapse = ", ")
  )
}


#### ========================================================================###
#### ======================== 3. DESCRIPTIVE CHECKS =========================###
#### ========================================================================###

# Check missingness for each extension outcome.
# This table is useful before SCM because missing pre-treatment outcomes can
# cause Synth::dataprep() to fail.
extension_missing_check <- df |>
  filter(
    iso3c %in% descriptive_countries,
    year %in% extension_plot_period
  ) |>
  select(
    iso3c,
    country,
    year,
    all_of(extension_trade_outcomes)
  ) |>
  pivot_longer(
    cols = all_of(extension_trade_outcomes),
    names_to = "outcome",
    values_to = "value"
  ) |>
  group_by(iso3c, country, outcome) |>
  summarise(
    n_years = n(),
    n_missing = sum(is.na(value)),
    n_missing_pre = sum(is.na(value[year %in% extension_pre_period])),
    n_missing_post = sum(is.na(value[year %in% extension_post_period])),
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
  mutate(
    group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% donor_countries ~ "Donor pool",
      iso3c %in% non_cfa_comparison_countries ~ "Non-donor comparison",
      TRUE ~ "Other"
    )
  ) |>
  arrange(outcome, group, desc(n_missing_pre), iso3c)

# Save missingness diagnostics for later inspection.
write.csv(
  extension_missing_check,
  file.path(extension_output_dir, "tables", "extension_trade_missing_check.csv"),
  row.names = FALSE
)

print(extension_missing_check, n = Inf)

# Create group-average data for descriptive line charts.
# This is not the SCM estimate; it is only a descriptive comparison of trends.
extension_grouped <- df |>
  filter(
    iso3c %in% descriptive_countries,
    year %in% extension_plot_period
  ) |>
  mutate(
    group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% donor_countries ~ "Donor pool",
      iso3c %in% non_cfa_comparison_countries ~ "Non-donor comparison",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(group)) |>
  select(
    iso3c,
    year,
    group,
    all_of(extension_trade_outcomes)
  ) |>
  pivot_longer(
    cols = all_of(extension_trade_outcomes),
    names_to = "outcome",
    values_to = "value"
  ) |>
  group_by(group, year, outcome) |>
  summarise(
    value = mean(value, na.rm = TRUE),
    n_countries = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  mutate(
    outcome_label = extension_trade_outcome_labels[outcome]
  )

# Save the group averages behind the descriptive figure.
write.csv(
  extension_grouped,
  file.path(extension_output_dir, "tables", "extension_trade_group_averages.csv"),
  row.names = FALSE
)

# Combined descriptive plot for all trade outcomes.
# The four country groups are shown with different coloured lines.
p_group_averages <- ggplot(
  extension_grouped,
  aes(x = year, y = value, color = group)
) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = treatment_year, linetype = "dashed") +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Trade-extension variables: group averages",
    subtitle = paste0("Dashed vertical line = ", treatment_year),
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(extension_output_dir, "figures", "extension_trade_group_averages.png"),
  plot = p_group_averages,
  width = 12,
  height = 9,
  dpi = 300
)




#### ========================================================================###
#### ======================== 4. SCM DATA PREPARATION =======================###
#### ========================================================================###

# Build a balanced base panel for all SCM outcomes and predictors.
# The outcome is not fixed here; it is selected inside run_trade_scm_country().
scm_base_df <- df |>
  filter(
    iso3c %in% scm_countries,
    year %in% extension_plot_period
  ) |>
  select(
    iso3c,
    country,
    year,
    all_of(extension_trade_outcomes),
    all_of(extension_scm_predictors)
  ) |>
  distinct(iso3c, year, .keep_all = TRUE) |>
  complete(
    iso3c,
    year = extension_plot_period
  ) |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  fill(country, .direction = "downup") |>
  ungroup() |>
  mutate(
    unit_name = iso3c
  )

# Create numeric country IDs required by Synth.
# Synth cannot use ISO3 strings directly as unit identifiers.
country_ids <- data.frame(
  iso3c = sort(unique(scm_base_df$iso3c)),
  unit_id = seq_along(sort(unique(scm_base_df$iso3c)))
)

# Merge numeric unit IDs into the SCM panel.
scm_base_df <- scm_base_df |>
  left_join(country_ids, by = "iso3c") |>
  mutate(
    unit_id = as.numeric(unit_id)
  )

# Put Synth ID variables first.
# This makes the panel easier to inspect and keeps the dataprep() call clear.
scm_base_df <- scm_base_df |>
  select(
    unit_id,
    unit_name,
    year,
    iso3c,
    country,
    all_of(extension_trade_outcomes),
    all_of(extension_scm_predictors)
  ) |>
  as.data.frame()


#### ========================================================================###
#### ======================== 5. HELPER FUNCTIONS ===========================###
#### ========================================================================###

# Root mean squared prediction error.
# This is used to compare pre-treatment and post-treatment fit.
safe_rmspe <- function(x) {
  sqrt(mean(x^2, na.rm = TRUE))
}

# Extract actual and synthetic paths from one successful SCM result.
# The output is a tidy country-year panel for plotting and summaries.
extract_scm_path <- function(result, outcome_var) {
  if (is.null(result)) {
    return(NULL)
  }

  dataprep.out <- result$dataprep
  synth.out <- result$synth

  actual <- as.numeric(dataprep.out$Y1plot)
  synthetic <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)
  years <- as.numeric(rownames(dataprep.out$Y1plot))

  tibble(
    outcome = outcome_var,
    treated_iso3c = result$treated_iso3c,
    treated_country = result$country_name,
    year = years,
    actual = actual,
    synthetic = synthetic,
    gap = actual - synthetic
  )
}

# Extract donor weights from one successful SCM result.
# This identifies which donor countries contribute to each synthetic control.
extract_weights <- function(result, outcome_var) {
  if (is.null(result)) {
    return(NULL)
  }

  result$tables$tab.w |>
    as.data.frame() |>
    transmute(
      outcome = outcome_var,
      treated_iso3c = result$treated_iso3c,
      treated_country = result$country_name,
      donor_iso3c = as.character(unit.names),
      donor_weight = as.numeric(w.weights)
    )
}

# Extract predictor balance table from one successful SCM result.
# This compares treated predictor values with synthetic predictor values.
extract_predictor_balance <- function(result, outcome_var) {
  if (is.null(result)) {
    return(NULL)
  }

  result$tables$tab.pred |>
    as.data.frame() |>
    tibble::rownames_to_column("predictor") |>
    mutate(
      outcome = outcome_var,
      treated_iso3c = result$treated_iso3c,
      treated_country = result$country_name,
      .before = 1
    )
}


#### ========================================================================###
#### ======================== 6. SCM FUNCTION ===============================###
#### ========================================================================###

run_trade_scm_country <- function(treated_iso3c, outcome_var) {

  message("Running SCM for ", treated_iso3c, " / ", outcome_var)

  # Get the human-readable treated-country label.
  country_name_treated <- get_country_name(treated_iso3c)

  # Select the current outcome and rename it to outcome_selected.
  # This allows the same SCM code to run for all trade outcomes.
  scm_df_i <- scm_base_df |>
    mutate(
      outcome_selected = .data[[outcome_var]]
    ) |>
    select(
      unit_id,
      unit_name,
      year,
      iso3c,
      country,
      outcome_selected,
      all_of(extension_scm_predictors)
    ) |>
    as.data.frame()

  # Identify the numeric treated-unit ID required by Synth.
  treated_id <- country_ids$unit_id[
    country_ids$iso3c == treated_iso3c
  ]

  if (length(treated_id) == 0) {
    stop("Treated country not found in country_ids: ", treated_iso3c)
  }

  # Check whether the treated country has usable pre-treatment outcome data.
  # If this fails, Synth cannot estimate an SCM for this treated country/outcome.
  treated_pre_check <- scm_df_i |>
    filter(
      unit_id == treated_id,
      year %in% extension_pre_period
    )

  if (
    any(is.na(treated_pre_check$outcome_selected)) ||
      any(!is.finite(treated_pre_check$outcome_selected))
  ) {
    stop(
      "Treated country has missing pre-treatment outcome data for ",
      treated_iso3c,
      " / ",
      outcome_var,
      "."
    )
  }

  # Drop donor countries that are not usable in the pre-treatment period.
  # Synth requires the dependent variable to be observed in every optimization year.
  # Therefore, bad_outcome uses any(), not all().
  bad_controls_i <- scm_df_i |>
    filter(
      year %in% extension_pre_period,
      iso3c %in% donor_countries
    ) |>
    group_by(unit_id, iso3c, unit_name) |>
    summarise(
      bad_outcome = any(is.na(outcome_selected)) | any(!is.finite(outcome_selected)),
      bad_agriculture = all(is.na(agriculture)) | all(!is.finite(agriculture)),
      bad_govt = all(is.na(govt_share)) | all(!is.finite(govt_share)),
      bad_invest = all(is.na(invest_share)) | all(!is.finite(invest_share)),
      bad_oda = all(is.na(oda_share)) | all(!is.finite(oda_share)),
      bad_fdi = all(is.na(fdi)) | all(!is.finite(fdi)),
      bad_labour = all(is.na(labour)) | all(!is.finite(labour)),
      bad_polity = all(is.na(polity2)) | all(!is.finite(polity2)),
      .groups = "drop"
    ) |>
    filter(
      bad_outcome |
        bad_agriculture |
        bad_govt |
        bad_invest |
        bad_oda |
        bad_fdi |
        bad_labour |
        bad_polity
    ) |>
    pull(unit_id)

  # Keep only donor units that passed the pre-treatment missingness screen.
  control_ids <- country_ids$unit_id[
    country_ids$iso3c %in% donor_countries &
      !(country_ids$unit_id %in% bad_controls_i)
  ]

  message(
    "Usable controls for ",
    treated_iso3c,
    " / ",
    outcome_var,
    ": ",
    length(control_ids)
  )

  if (length(control_ids) < 2) {
    stop(
      "Fewer than two usable donor countries remain for ",
      treated_iso3c,
      " / ",
      outcome_var,
      "."
    )
  }

  # Match on both:
    # 1. the average pre-treatment outcome, and
    # 2. selected pre-treatment outcome years from the constants file.
    special_predictors_i <- c(
      list(
        list("outcome_selected", extension_pre_period, c("mean"))
      ),
      lapply(
        extension_special_years,
        function(y) {
          list("outcome_selected", y, c("mean"))
        }
      )
    )
  # Prepare Synth inputs.
  # predictors are averaged over extension_pre_period.
  # time.optimize.ssr defines the pre-treatment fit period.
  dataprep.out <- dataprep(
    foo = scm_df_i,

    predictors = extension_scm_predictors,
    predictors.op = "mean",

    dependent = "outcome_selected",

    unit.variable = "unit_id",
    unit.names.variable = "unit_name",
    time.variable = "year",

    treatment.identifier = treated_id,
    controls.identifier = control_ids,

    time.predictors.prior = extension_pre_period,
    time.optimize.ssr = extension_pre_period,
    time.plot = extension_plot_period,

    special.predictors = special_predictors_i
  )

  # Estimate synthetic-control weights.
  synth.out <- synth(
    data.prep.obj = dataprep.out
  )

  # Build donor-weight and predictor-balance tables.
  synth.tables <- synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out
  )

  # Return one complete result object.
  list(
    treated_iso3c = treated_iso3c,
    country_name = country_name_treated,
    outcome = outcome_var,
    dataprep = dataprep.out,
    synth = synth.out,
    tables = synth.tables
  )
}


#### ========================================================================###
#### ======================== 7. RUN SCM FOR ALL OUTCOMES ===================###
#### ========================================================================###

# Lists to collect results across all outcomes and treated countries.
all_results <- list()
all_scm_paths <- list()
all_weights <- list()
all_predictor_balance <- list()

# Loop over each trade outcome.
for (outcome_var in extension_trade_outcomes) {

  cat("\n=====================================================\n")
  cat("EXTENSION SCM OUTCOME:", outcome_var, "\n")
  cat("=====================================================\n")

  outcome_results <- list()

  # Run SCM separately for each treated country.
  for (country_code in treated_countries) {

    result <- tryCatch(
      run_trade_scm_country(
        treated_iso3c = country_code,
        outcome_var = outcome_var
      ),
      error = function(e) {
        message(
          "SCM failed for ",
          country_code,
          " / ",
          outcome_var,
          ": ",
          e$message
        )
        return(NULL)
      }
    )

    outcome_results[[country_code]] <- result
  }

  # Keep successful results only.
  outcome_success <- outcome_results[
    !vapply(outcome_results, is.null, logical(1))
  ]

  all_results[[outcome_var]] <- outcome_results

  if (length(outcome_success) == 0) {
    message("No successful SCM results for outcome: ", outcome_var)
    next
  }

  # Extract paths, weights, and predictor balance tables.
  all_scm_paths[[outcome_var]] <- bind_rows(
    lapply(outcome_success, extract_scm_path, outcome_var = outcome_var)
  )

  all_weights[[outcome_var]] <- bind_rows(
    lapply(outcome_success, extract_weights, outcome_var = outcome_var)
  )

  all_predictor_balance[[outcome_var]] <- bind_rows(
    lapply(outcome_success, extract_predictor_balance, outcome_var = outcome_var)
  )
}

# Combine all successful outcomes.
scm_paths_all <- bind_rows(all_scm_paths)
scm_weights_all <- bind_rows(all_weights)
scm_predictor_balance_all <- bind_rows(all_predictor_balance)

if (nrow(scm_paths_all) == 0) {
  stop("No successful extension SCM paths were produced.")
}


#### ========================================================================###
#### ======================== 8. SAVE SCM OUTPUTS ===========================###
#### ========================================================================###

# Save full R objects for later placebo analysis and replication.
saveRDS(
  all_results,
  file.path(extension_processed_dir, "extension_trade_scm_results_all.rds")
)

saveRDS(
  scm_paths_all,
  file.path(extension_processed_dir, "extension_trade_scm_paths_all.rds")
)

saveRDS(
  scm_weights_all,
  file.path(extension_processed_dir, "extension_trade_scm_weights_all.rds")
)

saveRDS(
  scm_predictor_balance_all,
  file.path(extension_processed_dir, "extension_trade_scm_predictor_balance_all.rds")
)

# Save CSV outputs for inspection and tables.
write.csv(
  scm_paths_all,
  file.path(extension_output_dir, "tables", "extension_trade_scm_paths_all.csv"),
  row.names = FALSE
)

write.csv(
  scm_weights_all,
  file.path(extension_output_dir, "tables", "extension_trade_scm_weights_all.csv"),
  row.names = FALSE
)

write.csv(
  scm_predictor_balance_all,
  file.path(extension_output_dir, "tables", "extension_trade_scm_predictor_balance_all.csv"),
  row.names = FALSE
)


#### ========================================================================###
#### ======================== 9. SCM CHARTS BY OUTCOME ======================###
#### ========================================================================###

# Add readable outcome labels for chart titles.
scm_paths_plot <- scm_paths_all |>
  mutate(
    outcome_label = extension_trade_outcome_labels[outcome]
  )

# Convert actual/synthetic series to long format for path plots.
scm_paths_long <- scm_paths_plot |>
  pivot_longer(
    cols = c(actual, synthetic),
    names_to = "series",
    values_to = "value"
  ) |>
  mutate(
    series = recode(
      series,
      actual = "Actual",
      synthetic = "Synthetic"
    )
  )

# Get the list of outcomes that actually produced successful SCM results.
successful_outcomes <- unique(scm_paths_plot$outcome)

# Create one actual-vs-synthetic chart and one gap chart for each outcome.
for (outcome_var in successful_outcomes) {

  # Get readable label for this outcome.
  outcome_label_i <- extension_trade_outcome_labels[outcome_var]

  # Create a safe filename stub.
  outcome_file_stub <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    outcome_var
  )

  # Keep only this outcome for the path chart.
  paths_i <- scm_paths_long |>
    filter(outcome == outcome_var)

  # Keep only this outcome for the gap chart.
  gaps_i <- scm_paths_plot |>
    filter(outcome == outcome_var)

  # -----------------------------------------------------
  # Actual-vs-synthetic path chart for this outcome
  # -----------------------------------------------------

  p_paths_i <- ggplot(
    paths_i,
    aes(x = year, y = value, linetype = series)
  ) +
    geom_line(linewidth = 0.6) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    facet_wrap(~ treated_country, scales = "free_y", ncol = 4) +
    labs(
      title = paste0("Extension SCM: actual and synthetic — ", outcome_label_i),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      extension_output_dir,
      "figures",
      paste0("extension_trade_scm_paths_", outcome_file_stub, ".png")
    ),
    plot = p_paths_i,
    width = 18,
    height = 12,
    dpi = 300
  )

  # -----------------------------------------------------
  # Actual-minus-synthetic gap chart for this outcome
  # -----------------------------------------------------

  p_gaps_i <- ggplot(
    gaps_i,
    aes(x = year, y = gap)
  ) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    facet_wrap(~ treated_country, scales = "free_y", ncol = 4) +
    labs(
      title = paste0("Extension SCM: actual minus synthetic — ", outcome_label_i),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = "Actual - synthetic"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(
      extension_output_dir,
      "figures",
      paste0("extension_trade_scm_gaps_", outcome_file_stub, ".png")
    ),
    plot = p_gaps_i,
    width = 18,
    height = 12,
    dpi = 300
  )

  message("Saved SCM charts for outcome: ", outcome_var)
}
#### ========================================================================###
#### ======================== 10. SUMMARY TABLE =============================###
#### ========================================================================###

# Summarise pre-treatment fit and post-treatment gaps.
scm_summary <- scm_paths_all |>
  group_by(outcome, treated_iso3c, treated_country) |>
  summarise(
    pre_rmspe = safe_rmspe(gap[year %in% extension_pre_period]),
    post_rmspe = safe_rmspe(gap[year %in% extension_post_period]),
    rmspe_ratio = ifelse(pre_rmspe == 0, NA_real_, post_rmspe / pre_rmspe),
    avg_post_gap = mean(gap[year %in% extension_post_period], na.rm = TRUE),
    avg_abs_post_gap = mean(abs(gap[year %in% extension_post_period]), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    outcome_label = extension_trade_outcome_labels[outcome]
  )

# Save summary table.
write.csv(
  scm_summary,
  file.path(extension_output_dir, "tables", "extension_trade_scm_summary.csv"),
  row.names = FALSE
)

cat(
  kable(
    scm_summary,
    format = "latex",
    booktabs = TRUE,
    caption = "Extension SCM summary for trade outcomes"
  ),
  file = file.path(extension_output_dir, "tables", "extension_trade_scm_summary.tex")
)

print(scm_summary, n = Inf)

print("Extension trade SCM analysis complete.")
