#### ========================================================================###
#### ============== EXTENSION: TRADE VARIABLES PLACEBO TESTING ==============###
#### ========================================================================###

# This script runs in-space placebo tests for the trade-extension SCM analysis.
#
# The main extension SCM script estimates one synthetic control for each treated
# country and each trade outcome. 
#
# For each outcome and treated country, the script:
#   1. Uses the same donor pool as the main SCM.
#   2. Re-runs SCM many times, each time pretending that one donor is treated.
#   3. Computes placebo gaps.
#   4. Computes pre-treatment RMSPE, post-treatment RMSPE, and RMSPE ratios.
#   5. Computes a rank-style placebo p-value.
#   6. Saves separate charts and tables for each outcome.
#
# Outputs are saved separately by outcome.

#### ========================================================================###
#### ======================== 1. PROJECT SETUP ==============================###
#### ========================================================================###

# Core data manipulation and plotting packages.
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(knitr)
library(Synth)
library(progress)

# Load the shared constants file.
# This file should define country lists, extension years, outcomes, labels,
# predictors, and output folders.
constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
    stop("Constants file not found: ", constants_file)
}

source(constants_file)

# Check that the constants file contains all objects used below.
# Important: these must be object names in quotation marks.
required_constants <- c(
    "treated_countries",
    "donor_countries",
    "country_names",
    "get_country_name",
    "treatment_year",
    "extension_pre_period",
    "extension_post_period",
    "extension_plot_period",
    "extension_special_years",
    "extension_trade_outcomes",
    "extension_trade_outcome_labels",
    "extension_scm_predictors",
    "extension_output_dir",
    "extension_processed_dir"
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

# Create placebo-specific output folders.
# These are kept separate from the main SCM figures and tables.
placebo_output_dir <- file.path(extension_output_dir, "placebo")

dir.create(file.path(placebo_output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(placebo_output_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(extension_processed_dir, "placebo"), recursive = TRUE, showWarnings = FALSE)


#### ========================================================================###
#### ======================== 2. LOAD DATA ==================================###
#### ========================================================================###

# Load the processed panel.
# This must already contain all extension outcome variables:
#   XEU, MEU, trade_openness, exports_gdp, imports_gdp
# or whichever subset you define in extension_trade_outcomes.
df <- readRDS("data/processed/processed_panel_imputed.rds")

# Load main SCM paths.
# These are used to obtain the treated-country gaps from the main analysis.
main_paths_file <- file.path(
    extension_processed_dir,
    "extension_trade_scm_paths_all.rds"
)

if (!file.exists(main_paths_file)) {
    stop(
        "Main SCM paths file not found: ",
        main_paths_file,
        ". Run the main extension SCM script first."
    )
}

main_scm_paths <- readRDS(main_paths_file)

# Check that the extension outcome variables exist in the processed panel.
missing_trade_outcomes <- setdiff(extension_trade_outcomes, names(df))

if (length(missing_trade_outcomes) > 0) {
    stop(
        "These extension trade outcomes are missing from df: ",
        paste(missing_trade_outcomes, collapse = ", "),
        ". Add them in the WDI/DOTS data-loading scripts first."
    )
}

# Check that all SCM predictors exist.
missing_predictors <- setdiff(extension_scm_predictors, names(df))

if (length(missing_predictors) > 0) {
    stop(
        "These extension SCM predictors are missing from df: ",
        paste(missing_predictors, collapse = ", ")
    )
}

# The placebo tests use the same country universe as the main SCM:
# treated countries plus donor countries.
scm_countries <- unique(c(treated_countries, donor_countries))

# Check whether all selected countries are in the panel.
missing_from_data <- setdiff(scm_countries, unique(df$iso3c))

if (length(missing_from_data) > 0) {
    warning(
        "These selected countries are missing from df$iso3c: ",
        paste(missing_from_data, collapse = ", ")
    )
}


#### ========================================================================###
#### ======================== 3. PREPARE SCM BASE PANEL =====================###
#### ========================================================================###

# Build one base panel containing:
#   - country identifiers
#   - years
#   - all extension outcomes
#   - all SCM predictors
#
# The specific outcome used by Synth is selected later inside the placebo
# function. This avoids rebuilding the full panel from scratch for each outcome.
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
    distinct(
        iso3c,
        year,
        .keep_all = TRUE
    ) |>
    complete(
        iso3c,
        year = extension_plot_period
    ) |>
    arrange(
        iso3c,
        year
    ) |>
    group_by(iso3c) |>
    fill(
        country,
        .direction = "downup"
    ) |>
    ungroup() |>
    mutate(
        unit_name = iso3c
    )

# Synth requires numeric unit identifiers.
# We create one stable numeric ID for each country in the SCM base panel.
country_ids <- data.frame(
    iso3c = sort(unique(scm_base_df$iso3c)),
    unit_id = seq_along(sort(unique(scm_base_df$iso3c)))
)

# Add numeric unit IDs to the base panel.
scm_base_df <- scm_base_df |>
    left_join(
        country_ids,
        by = "iso3c"
    ) |>
    mutate(
        unit_id = as.numeric(unit_id)
    )

# Put the Synth identifier variables first.
# This is not strictly necessary, but it makes debugging easier.
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
#### ======================== 4. HELPER FUNCTIONS ===========================###
#### ========================================================================###

# Root mean squared prediction error.
# This is used to measure how closely the synthetic control matches the actual
# country before and after treatment.
safe_rmspe <- function(x) {
    sqrt(mean(x^2, na.rm = TRUE))
}

# Make a safe filename component from an outcome name or country code.
# This avoids file-path problems if labels contain spaces or punctuation.
make_file_stub <- function(x) {
    gsub("[^A-Za-z0-9]+", "_", x)
}

# Extract actual and synthetic paths from a successful placebo SCM result.
extract_placebo_path <- function(result, outcome_var, treated_iso3c, placebo_iso3c) {
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
        treated_iso3c = treated_iso3c,
        treated_country = get_country_name(treated_iso3c),
        placebo_iso3c = placebo_iso3c,
        placebo_country = get_country_name(placebo_iso3c),
        year = years,
        actual = actual,
        synthetic = synthetic,
        gap = actual - synthetic
    )
}

# Compute pre/post RMSPE and the post/pre RMSPE ratio for one gap series.
summarise_gap_series <- function(gap_data, unit_type) {
    gap_data |>
        group_by(
            outcome,
            treated_iso3c,
            treated_country,
            placebo_iso3c,
            placebo_country
        ) |>
        summarise(
            pre_rmspe = safe_rmspe(gap[year %in% extension_pre_period]),
            post_rmspe = safe_rmspe(gap[year %in% extension_post_period]),
            rmspe_ratio = ifelse(
                pre_rmspe == 0,
                NA_real_,
                post_rmspe / pre_rmspe
            ),
            avg_post_gap = mean(
                gap[year %in% extension_post_period],
                na.rm = TRUE
            ),
            avg_abs_post_gap = mean(
                abs(gap[year %in% extension_post_period]),
                na.rm = TRUE
            ),
            .groups = "drop"
        ) |>
        mutate(
            unit_type = unit_type
        )
}


#### ========================================================================###
#### ======================== 5. PLACEBO SCM FUNCTION =======================###
#### ========================================================================###

# This function runs one placebo SCM.
#
# Inputs:
#   treated_iso3c:
#     The actual treated country whose donor pool context is being studied.
#
#   outcome_var:
#     The trade outcome currently being analysed.
#
#   placebo_iso3c:
#     The donor country that is temporarily treated as if it received treatment.
#
# Logic:
#   For each actual treated country, we use the donor pool as the comparison
#   set. Then, for the placebo test, one donor becomes the placebo treated unit.
#   The remaining donors become controls.
#
# Example:
#   Actual treated country: BEN
#   Placebo country: BGD
#   Controls: all donors except BGD
#
# This produces a placebo gap for BGD. Repeating this for every donor gives the
# placebo distribution for BEN's donor pool and outcome.
run_extension_placebo_scm <- function(treated_iso3c, outcome_var, placebo_iso3c) {
    # message(
    #     "Running placebo SCM for treated context ",
    #     treated_iso3c,
    #     " / placebo ",
    #     placebo_iso3c,
    #     " / outcome ",
    #     outcome_var
    # )

    # Select the current outcome and rename it to outcome_selected.
    # Synth then uses the same dependent-variable name regardless of outcome.
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

    # Numeric ID for the placebo treated unit.
    placebo_id <- country_ids$unit_id[
        country_ids$iso3c == placebo_iso3c
    ]

    if (length(placebo_id) == 0) {
        stop("Placebo country not found in country_ids: ", placebo_iso3c)
    }

    # The controls are all donor countries except the current placebo country.
    placebo_control_codes <- setdiff(donor_countries, placebo_iso3c)

    # Synth requires the placebo treated unit to have complete outcome data in
    # the optimization period. If not, this placebo run is invalid.
    placebo_pre_check <- scm_df_i |>
        filter(
            unit_id == placebo_id,
            year %in% extension_pre_period
        )

    if (
        any(is.na(placebo_pre_check$outcome_selected)) ||
            any(!is.finite(placebo_pre_check$outcome_selected))
    ) {
        stop(
            "Placebo country has missing pre-treatment outcome data: ",
            placebo_iso3c,
            " / ",
            outcome_var
        )
    }

    # Drop controls that cannot be used by Synth.
    # The dependent variable must be observed in every optimization-period year.
    # Predictors are treated less strictly here: a donor is dropped only if a
    # predictor is completely missing over the whole pre-treatment period.
    bad_controls_i <- scm_df_i |>
        filter(
            year %in% extension_pre_period,
            iso3c %in% placebo_control_codes
        ) |>
        group_by(
            unit_id,
            iso3c,
            unit_name
        ) |>
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

    # Numeric IDs for valid placebo controls.
    control_ids <- country_ids$unit_id[
        country_ids$iso3c %in% placebo_control_codes &
            !(country_ids$unit_id %in% bad_controls_i)
    ]

    if (length(control_ids) < 2) {
        stop(
            "Fewer than two valid placebo controls remain for ",
            placebo_iso3c,
            " / ",
            outcome_var
        )
    }

    # Keep only special years that fall within the extension pre-treatment period.
    # This prevents accidental use of years outside the current sample window.
    extension_special_years_i <- extension_special_years[
        extension_special_years %in% extension_pre_period
    ]

    # Match on both:
    #   1. the mean pre-treatment outcome, and
    #   2. selected special pre-treatment outcome years.
    #
    # This mirrors the main extension SCM matching specification.
    special_predictors_i <- c(
        list(
            list("outcome_selected", extension_pre_period, c("mean"))
        ),
        lapply(
            extension_special_years_i,
            function(y) {
                list("outcome_selected", y, c("mean"))
            }
        )
    )

    # Prepare the data in the format required by Synth.
    dataprep.out <- dataprep(
        foo = scm_df_i,
        predictors = extension_scm_predictors,
        predictors.op = "mean",
        dependent = "outcome_selected",
        unit.variable = "unit_id",
        unit.names.variable = "unit_name",
        time.variable = "year",
        treatment.identifier = placebo_id,
        controls.identifier = control_ids,
        time.predictors.prior = extension_pre_period,
        time.optimize.ssr = extension_pre_period,
        time.plot = extension_plot_period,
        special.predictors = special_predictors_i
    )

    # Estimate the placebo synthetic control.
    synth.out <- suppressMessages(
        suppressWarnings(
            capture.output(
                synth_result <- synth(
                    data.prep.obj = dataprep.out
                )
            )
        )
    )

synth.out <- synth_result

    # Return the complete placebo result.
    list(
        treated_iso3c = treated_iso3c,
        placebo_iso3c = placebo_iso3c,
        outcome = outcome_var,
        dataprep = dataprep.out,
        synth = synth.out
    )
}


#### ========================================================================###
#### ======================== 6. RUN PLACEBOS ===============================###
#### ========================================================================###

# These objects collect all placebo outputs.
all_placebo_results <- list()
all_placebo_paths <- list()
all_placebo_summary <- list()
all_placebo_run_log <- list()

# Count the total number of placebo SCM runs.
# Total runs = outcomes × treated countries × donor placebo countries.
total_placebo_runs <- length(extension_trade_outcomes) *
    length(treated_countries) *
    length(donor_countries)

# Create a terminal progress bar.
placebo_progress <- progress_bar$new(
    format = paste0(
        "Placebo tests [:bar] :percent | ",
        "Run :current/:total | ",
        "ETA: :eta | ",
        ":outcome / treated :treated / placebo :placebo"
    ),
    total = total_placebo_runs,
    clear = FALSE,
    width = 100
)

# Loop over outcomes separately.
# This guarantees that the final reporting is separated by trade variable.
for (outcome_var in extension_trade_outcomes) {
    cat("\n=====================================================\n")
    cat("EXTENSION PLACEBO OUTCOME:", outcome_var, "\n")
    cat("=====================================================\n")

    outcome_placebo_results <- list()
    outcome_placebo_paths <- list()
    outcome_run_log <- list()

    # For each actual treated country, run donor placebo tests.
    for (treated_code in treated_countries) {
        cat("\n-----------------------------------------------------\n")
        cat("Treated-country context:", treated_code, "\n")
        cat("Outcome:", outcome_var, "\n")
        cat("-----------------------------------------------------\n")

        treated_context_results <- list()
        treated_context_paths <- list()
        treated_context_log <- list()

        # Each donor is treated as placebo-treated once.
        for (placebo_code in donor_countries) {
            placebo_progress$tick(
                tokens = list(
                    outcome = outcome_var,
                    treated = treated_code,
                    placebo = placebo_code
                )
            )

            placebo_result <- tryCatch(
                {
                    result_i <- run_extension_placebo_scm(
                        treated_iso3c = treated_code,
                        outcome_var = outcome_var,
                        placebo_iso3c = placebo_code
                    )

                    treated_context_log[[placebo_code]] <- tibble(
                        outcome = outcome_var,
                        treated_iso3c = treated_code,
                        placebo_iso3c = placebo_code,
                        success = TRUE,
                        error_message = NA_character_
                    )

                    result_i
                },
                error = function(e) {
                    message(
                        "Placebo failed for treated context ",
                        treated_code,
                        " / placebo ",
                        placebo_code,
                        " / outcome ",
                        outcome_var,
                        ": ",
                        e$message
                    )

                    treated_context_log[[placebo_code]] <- tibble(
                        outcome = outcome_var,
                        treated_iso3c = treated_code,
                        placebo_iso3c = placebo_code,
                        success = FALSE,
                        error_message = e$message
                    )

                    NULL
                }
            )

            treated_context_results[[placebo_code]] <- placebo_result

            if (!is.null(placebo_result)) {
                treated_context_paths[[placebo_code]] <- extract_placebo_path(
                    result = placebo_result,
                    outcome_var = outcome_var,
                    treated_iso3c = treated_code,
                    placebo_iso3c = placebo_code
                )
            }
        }

        # Save placebo results for this treated-country context.
        outcome_placebo_results[[treated_code]] <- treated_context_results

        # Save paths for this treated-country context.
        outcome_placebo_paths[[treated_code]] <- bind_rows(treated_context_paths)

        # Save the run log for this treated-country context.
        outcome_run_log[[treated_code]] <- bind_rows(treated_context_log)

        cat(
            "Successful placebos for ",
            treated_code,
            " / ",
            outcome_var,
            ": ",
            sum(bind_rows(treated_context_log)$success),
            " out of ",
            length(donor_countries),
            "\n",
            sep = ""
        )
    }

    # Store all placebo result objects and paths for this outcome.
    all_placebo_results[[outcome_var]] <- outcome_placebo_results
    all_placebo_paths[[outcome_var]] <- bind_rows(outcome_placebo_paths)
    all_placebo_run_log[[outcome_var]] <- bind_rows(outcome_run_log)

    # Create placebo summary statistics for this outcome.
    if (nrow(all_placebo_paths[[outcome_var]]) > 0) {
        all_placebo_summary[[outcome_var]] <- summarise_gap_series(
            gap_data = all_placebo_paths[[outcome_var]],
            unit_type = "Placebo donor"
        )
    } else {
        all_placebo_summary[[outcome_var]] <- tibble()
        message("No successful placebo paths for outcome: ", outcome_var)
    }
}

# Combine all outcomes for saving.
placebo_paths_all <- bind_rows(all_placebo_paths)
placebo_summary_all <- bind_rows(all_placebo_summary)
placebo_run_log_all <- bind_rows(all_placebo_run_log)

# Save logs even if some or all placebo tests fail.
write.csv(
    placebo_run_log_all,
    file.path(placebo_output_dir, "tables", "extension_trade_placebo_run_log_all.csv"),
    row.names = FALSE
)

# Stop clearly if no placebo paths were produced.
if (nrow(placebo_paths_all) == 0) {
    stop(
        "No successful extension placebo paths were produced. ",
        "Check the placebo run log in output/extension_trade/placebo/tables."
    )
}


#### ========================================================================###
#### ======================== 7. ADD TREATED RESULTS ========================###
#### ========================================================================###

# To compute placebo p-values, we need to compare the treated-country RMSPE
# ratio with the placebo RMSPE ratios.
#
# The treated gaps were already produced by the main extension SCM script and
# are stored in main_scm_paths.
treated_summary_all <- main_scm_paths |>
    filter(
        outcome %in% extension_trade_outcomes
    ) |>
    mutate(
        placebo_iso3c = treated_iso3c,
        placebo_country = treated_country
    ) |>
    summarise_gap_series(
        unit_type = "Actual treated"
    )

# Combine actual treated summaries and placebo summaries.
# This is the table used to compute ranks and p-values.
rmspe_comparison_all <- bind_rows(
    treated_summary_all,
    placebo_summary_all
)

# Compute rank-style placebo p-values.
#
# For each treated country and outcome:
#   p-value = share of actual + placebo units with RMSPE ratio greater than
#             or equal to the treated country's RMSPE ratio.
#
# With 20 donors plus 1 treated country, the smallest possible p-value is 1/21.
placebo_pvalues_all <- rmspe_comparison_all |>
    group_by(
        outcome,
        treated_iso3c,
        treated_country
    ) |>
    mutate(
        treated_rmspe_ratio = rmspe_ratio[
            unit_type == "Actual treated"
        ][1],
        n_units_in_reference_distribution = sum(!is.na(rmspe_ratio)),
        placebo_rank_pvalue = mean(
            rmspe_ratio >= treated_rmspe_ratio,
            na.rm = TRUE
        )
    ) |>
    ungroup() |>
    filter(
        unit_type == "Actual treated"
    ) |>
    select(
        outcome,
        treated_iso3c,
        treated_country,
        pre_rmspe,
        post_rmspe,
        rmspe_ratio,
        avg_post_gap,
        avg_abs_post_gap,
        n_units_in_reference_distribution,
        placebo_rank_pvalue
    ) |>
    arrange(
        outcome,
        placebo_rank_pvalue,
        treated_iso3c
    )

# Add readable outcome labels.
placebo_pvalues_all <- placebo_pvalues_all |>
    mutate(
        outcome_label = extension_trade_outcome_labels[outcome]
    )


#### ========================================================================###
#### ======================== 8. SAVE PLACEBO OUTPUTS =======================###
#### ========================================================================###

# Save full R objects for later inspection.
saveRDS(
    all_placebo_results,
    file.path(extension_processed_dir, "placebo", "extension_trade_placebo_results_all.rds")
)

saveRDS(
    placebo_paths_all,
    file.path(extension_processed_dir, "placebo", "extension_trade_placebo_paths_all.rds")
)

saveRDS(
    placebo_summary_all,
    file.path(extension_processed_dir, "placebo", "extension_trade_placebo_summary_all.rds")
)

saveRDS(
    placebo_pvalues_all,
    file.path(extension_processed_dir, "placebo", "extension_trade_placebo_pvalues_all.rds")
)

# Save combined CSV files.
write.csv(
    placebo_paths_all,
    file.path(placebo_output_dir, "tables", "extension_trade_placebo_paths_all.csv"),
    row.names = FALSE
)

write.csv(
    placebo_summary_all,
    file.path(placebo_output_dir, "tables", "extension_trade_placebo_summary_all.csv"),
    row.names = FALSE
)

write.csv(
    placebo_pvalues_all,
    file.path(placebo_output_dir, "tables", "extension_trade_placebo_pvalues_all.csv"),
    row.names = FALSE
)

# Save separate p-value tables by outcome.
# This is useful for reporting each trade variable separately.
for (outcome_var in unique(placebo_pvalues_all$outcome)) {
    outcome_file_stub <- make_file_stub(outcome_var)

    pvalues_i <- placebo_pvalues_all |>
        filter(outcome == outcome_var) |>
        arrange(placebo_rank_pvalue, treated_iso3c)

    write.csv(
        pvalues_i,
        file.path(
            placebo_output_dir,
            "tables",
            paste0("extension_trade_placebo_pvalues_", outcome_file_stub, ".csv")
        ),
        row.names = FALSE
    )

    cat(
        kable(
            pvalues_i,
            format = "latex",
            booktabs = TRUE,
            caption = paste0(
                "Extension placebo p-values for ",
                extension_trade_outcome_labels[outcome_var]
            )
        ),
        file = file.path(
            placebo_output_dir,
            "tables",
            paste0("extension_trade_placebo_pvalues_", outcome_file_stub, ".tex")
        )
    )
}


#### ========================================================================###
#### ======================== 9. PLACEBO CHARTS BY OUTCOME ==================###
#### ========================================================================###

# The plots below are saved separately for each outcome.
#
# Chart 1:
#   Placebo gaps and treated gaps.
#   Treated gap is shown as a thicker line.
#
# Chart 2:
#   Placebo p-values by treated country.
#
# We avoid one huge combined chart because it is hard to read.

# Add readable labels to placebo paths.
placebo_paths_plot <- placebo_paths_all |>
    mutate(
        outcome_label = extension_trade_outcome_labels[outcome],
        line_type = "Placebo donor"
    )

# Add readable labels to main treated paths.
treated_paths_plot <- main_scm_paths |>
    filter(
        outcome %in% extension_trade_outcomes
    ) |>
    mutate(
        outcome_label = extension_trade_outcome_labels[outcome],
        placebo_iso3c = treated_iso3c,
        placebo_country = treated_country,
        line_type = "Actual treated"
    ) |>
    select(
        outcome,
        outcome_label,
        treated_iso3c,
        treated_country,
        placebo_iso3c,
        placebo_country,
        year,
        gap,
        line_type
    )

# Combine placebo and treated paths for plotting.
gap_plot_all <- bind_rows(
    placebo_paths_plot |>
        select(
            outcome,
            outcome_label,
            treated_iso3c,
            treated_country,
            placebo_iso3c,
            placebo_country,
            year,
            gap,
            line_type
        ),
    treated_paths_plot
)

# Save one placebo-gap chart per outcome.
for (outcome_var in unique(gap_plot_all$outcome)) {
    outcome_file_stub <- make_file_stub(outcome_var)
    outcome_label_i <- extension_trade_outcome_labels[outcome_var]

    gap_plot_i <- gap_plot_all |>
        filter(outcome == outcome_var)

    # Split the data so the treated line can be drawn more prominently.
    placebo_lines_i <- gap_plot_i |>
        filter(line_type == "Placebo donor")

    treated_lines_i <- gap_plot_i |>
        filter(line_type == "Actual treated")

    p_placebo_gaps_i <- ggplot() +
        geom_line(
            data = placebo_lines_i,
            aes(
                x = year,
                y = gap,
                group = placebo_iso3c
            ),
            linewidth = 0.35,
            alpha = 0.35
        ) +
        geom_line(
            data = treated_lines_i,
            aes(
                x = year,
                y = gap
            ),
            linewidth = 0.9
        ) +
        geom_hline(
            yintercept = 0,
            linetype = "dashed"
        ) +
        geom_vline(
            xintercept = treatment_year,
            linetype = "dashed"
        ) +
        facet_wrap(
            ~treated_country,
            scales = "free_y",
            ncol = 4
        ) +
        labs(
            title = paste0(
                "Extension placebo gaps — ",
                outcome_label_i
            ),
            subtitle = paste0(
                "Thin lines = donor placebos; thick line = actual treated country. Dashed vertical line = ",
                treatment_year
            ),
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
            placebo_output_dir,
            "figures",
            paste0("extension_trade_placebo_gaps_", outcome_file_stub, ".png")
        ),
        plot = p_placebo_gaps_i,
        width = 18,
        height = 12,
        dpi = 300
    )

    # P-value chart for this outcome.
    pvalues_i <- placebo_pvalues_all |>
        filter(outcome == outcome_var) |>
        mutate(
            treated_country = factor(
                treated_country,
                levels = treated_country[order(placebo_rank_pvalue)]
            )
        )

    p_pvalues_i <- ggplot(
        pvalues_i,
        aes(
            x = treated_country,
            y = placebo_rank_pvalue
        )
    ) +
        geom_col() +
        geom_hline(
            yintercept = 0.10,
            linetype = "dashed"
        ) +
        geom_hline(
            yintercept = 0.05,
            linetype = "dashed"
        ) +
        coord_flip() +
        labs(
            title = paste0(
                "Extension placebo p-values — ",
                outcome_label_i
            ),
            subtitle = "Lower values mean the treated-country RMSPE ratio is more unusual relative to donor placebos",
            x = NULL,
            y = "Rank placebo p-value"
        ) +
        theme_minimal(base_size = 10) +
        theme(
            panel.grid.minor = element_blank()
        )

    ggsave(
        filename = file.path(
            placebo_output_dir,
            "figures",
            paste0("extension_trade_placebo_pvalues_", outcome_file_stub, ".png")
        ),
        plot = p_pvalues_i,
        width = 10,
        height = 7,
        dpi = 300
    )

    message("Saved placebo charts for outcome: ", outcome_var)
}


#### ========================================================================###
#### ======================== 10. PRINT SUMMARY =============================###
#### ========================================================================###

cat("\n=====================================================\n")
cat("EXTENSION PLACEBO TESTING COMPLETE\n")
cat("=====================================================\n")

cat("\nPlacebo outputs saved to:\n")
cat(placebo_output_dir, "\n")

cat("\nCombined p-value table:\n")
print(placebo_pvalues_all, n = Inf)

cat("\nSuccessful placebo runs:\n")
print(
    placebo_run_log_all |>
        count(outcome, success),
    n = Inf
)

cat("\nDone.\n")