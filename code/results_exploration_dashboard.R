# This Shiny dashboard reads the saved outputs from the replication and
# extension analyses.
#
# Expected project files:
#   code/00_constants.R
#   data/processed/processed_panel_imputed.rds
#   data/processed/processed_panel_unimputed.rds
#   data/processed/extension_trade/extension_trade_scm_paths_all.rds
#   output/extension_trade/tables/extension_trade_group_averages.csv
#

#### ========================================================================###
#### ======================== 1. PACKAGES ==================================###
#### ========================================================================###

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)

#### ========================================================================###
#### ======================== 2. PROJECT SETUP ===============================###
#### ========================================================================###

constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
  stop("Could not find constants file: ", constants_file)
}

source(constants_file)

# Dashboard/chart background.
chart_bg <- "#f5f3f3"

# -------------------------------------------------------------------------- #
# Safe file readers
# -------------------------------------------------------------------------- #

safe_read_rds <- function(path) {
  if (file.exists(path)) {
    readRDS(path)
  } else {
    NULL
  }
}

safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

# -------------------------------------------------------------------------- #
# Label helpers
# -------------------------------------------------------------------------- #

get_var_label <- function(var) {

  if (
    exists("extension_trade_outcome_labels") &&
      var %in% names(extension_trade_outcome_labels)
  ) {
    return(unname(extension_trade_outcome_labels[var]))
  }

  label_lookup <- c(
    wdi_gdp_pc_current = "GDP per capita, current US$",
    gdp_selected = "GDP per capita",
    XEU = "Exports to EU (% of total exports)",
    MEU = "Imports from EU (% of total imports)",
    trade_openness = "Trade openness (% of GDP)",
    exports_gdp = "Exports of goods and services (% of GDP)",
    imports_gdp = "Imports of goods and services (% of GDP)",
    agriculture = "Agriculture, value added (% of GDP)",
    industry = "Industry, value added (% of GDP)",
    govt_share = "Government share",
    invest_share = "Investment share",
    oda_share = "ODA share",
    fdi = "FDI",
    labour = "Labour",
    polity2 = "Polity score"
  )

  if (var %in% names(label_lookup)) {
    return(unname(label_lookup[var]))
  }

  var
}

country_label <- function(code) {
  if (exists("get_country_name")) {
    return(get_country_name(code))
  }

  code
}

#### ========================================================================###
#### ======================== 3. LOAD DATA ==================================###
#### ========================================================================###

# Final/imputed panel used in the main analysis. 
panel_imputed <- safe_read_rds("data/processed/processed_panel_imputed.rds")

if (is.null(panel_imputed)) {
  stop("Could not find data/processed/processed_panel_imputed.rds")
}

# Unimputed panel, saved before interpolation/imputation.
panel_unimputed <- safe_read_rds("data/processed/processed_panel_unimputed.rds")

if (is.null(panel_unimputed)) {
  warning(
    "Could not find data/processed/processed_panel_unimputed.rds. ",
    "Imputed-vs-unimputed charts will show a warning plot."
  )
}

# Use imputed panel as the default panel throughout the dashboard.
panel_df <- panel_imputed

# Extension SCM paths
extension_paths <- safe_read_rds(
  file.path(extension_processed_dir, "extension_trade_scm_paths_all.rds")
)

extension_group_averages <- safe_read_csv(
  file.path(extension_output_dir, "tables", "extension_trade_group_averages.csv")
)

extension_group_averages_reduced <- safe_read_csv(
  file.path(extension_output_dir, "tables", "extension_trade_group_averages_reduced.csv")
)

extension_placebo_pvalues <- safe_read_rds(
  file.path(extension_processed_dir, "placebo", "extension_trade_placebo_pvalues_all.rds")
)


# The dashboard checks both likely locations.
gdp_paths <- safe_read_rds("data/processed/scm_paths_all_treated.rds")

if (is.null(gdp_paths)) {
  gdp_paths <- safe_read_csv("output/tables/scm_paths_all_treated.csv")
}

if (!is.null(gdp_paths)) {
  gdp_paths <- gdp_paths |>
    dplyr::rename(
      treated_iso3c = iso3c
    )
}

# Add dashboard country groups to the panel.
panel_df <- panel_df |>
  mutate(
    dashboard_group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CEMAC",
      iso3c %in% treated_countries ~ "Other treated CFA",
      iso3c %in% donor_countries ~ "Updated donor pool",
      TRUE ~ "Other"
    )
  )

if (!is.null(panel_unimputed)) {
  panel_unimputed <- panel_unimputed |>
    mutate(
      dashboard_group = case_when(
        iso3c %in% waemu ~ "WAEMU",
        iso3c %in% caemc ~ "CEMAC",
        iso3c %in% treated_countries ~ "Other treated CFA",
        iso3c %in% donor_countries ~ "Updated donor pool",
        TRUE ~ "Other"
      )
    )
}

#### ========================================================================###
#### ======================== 4. DASHBOARD CHOICES ==========================###
#### ========================================================================###

# -------------------------------------------------------------------------- #
# Restricted raw variables
# -------------------------------------------------------------------------- #



dashboard_raw_variables <- c(
  "wdi_gdp_pc_current",
  "gdp_selected",
  "XEU",
  "MEU",
  "trade_openness",
  "exports_gdp",
  "imports_gdp",
  "agriculture",
  "industry",
  "govt_share",
  "invest_share",
  "oda_share",
  "fdi",
  "labour",
  "polity2"
)

dashboard_raw_variables <- dashboard_raw_variables[
  dashboard_raw_variables %in% names(panel_df)
]

if (length(dashboard_raw_variables) == 0) {
  stop("None of the dashboard_raw_variables were found in the processed panel.")
}

raw_variable_choices <- dashboard_raw_variables

# -------------------------------------------------------------------------- #
# Restricted imputation-comparison variables
# -------------------------------------------------------------------------- #

dashboard_imputation_variables <- c(
  "agriculture",
  "industry",
  "govt_share",
  "invest_share",
  "oda_share",
  "fdi",
  "labour",
  "polity2",
  "wdi_gdp_pc_current",
  "gdp_selected",
  "trade_openness",
  "exports_gdp",
  "imports_gdp"
)

dashboard_imputation_variables <- dashboard_imputation_variables[
  dashboard_imputation_variables %in% names(panel_df)
]

if (length(dashboard_imputation_variables) == 0) {
  dashboard_imputation_variables <- raw_variable_choices
}

# -------------------------------------------------------------------------- #
# Extension SCM outcomes
# -------------------------------------------------------------------------- #

if (!is.null(extension_paths)) {
  extension_outcome_choices <- sort(unique(extension_paths$outcome))
} else if (exists("extension_trade_outcomes")) {
  extension_outcome_choices <- extension_trade_outcomes[
    extension_trade_outcomes %in% names(panel_df)
  ]
} else {
  extension_outcome_choices <- character(0)
}

# -------------------------------------------------------------------------- #
# Country and group choices
# -------------------------------------------------------------------------- #

all_country_codes <- sort(unique(panel_df$iso3c))

country_choices <- setNames(
  all_country_codes,
  paste0(
    vapply(all_country_codes, country_label, character(1)),
    " (",
    all_country_codes,
    ")"
  )
)

default_country <- if ("SEN" %in% all_country_codes) {
  "SEN"
} else {
  all_country_codes[1]
}

group_choices <- c(
  "WAEMU",
  "CEMAC",
  "Updated donor pool",
  "Treated CFA countries",
  "All SCM countries"
)

#### ========================================================================###
#### ======================== 5. PLOT HELPERS ===============================###
#### ========================================================================###

base_dashboard_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = chart_bg, color = NA),
      panel.background = element_rect(fill = chart_bg, color = NA),
      legend.background = element_rect(fill = chart_bg, color = NA),
      legend.box.background = element_rect(fill = chart_bg, color = NA),
      strip.background = element_rect(fill = chart_bg, color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

empty_plot <- function(label) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = label, size = 5) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = chart_bg, color = NA),
      panel.background = element_rect(fill = chart_bg, color = NA)
    )
}

get_group_codes <- function(group_name) {
  switch(
    group_name,
    "WAEMU" = waemu,
    "CEMAC" = caemc,
    "Updated donor pool" = donor_countries,
    "Treated CFA countries" = treated_countries,
    "All SCM countries" = unique(c(treated_countries, donor_countries)),
    unique(c(treated_countries, donor_countries))
  )
}

plot_raw_country <- function(country_code, variable, year_min, year_max) {

  if (!(variable %in% names(panel_df))) {
    return(empty_plot(paste("Variable not found:", variable)))
  }

  data_i <- panel_df |>
    filter(
      iso3c == country_code,
      year >= year_min,
      year <= year_max
    )

  if (nrow(data_i) == 0) {
    return(empty_plot(paste("No data for", country_code)))
  }

  ggplot(data_i, aes(x = year, y = .data[[variable]])) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.3, na.rm = TRUE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": ", get_var_label(variable)),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = get_var_label(variable)
    ) +
    base_dashboard_theme()
}

plot_raw_group <- function(group_name, variable, year_min, year_max) {

  if (!(variable %in% names(panel_df))) {
    return(empty_plot(paste("Variable not found:", variable)))
  }

  selected_codes <- get_group_codes(group_name)

  data_i <- panel_df |>
    filter(
      iso3c %in% selected_codes,
      year >= year_min,
      year <= year_max
    ) |>
    group_by(year) |>
    summarise(
      value = mean(.data[[variable]], na.rm = TRUE),
      n_countries = sum(!is.na(.data[[variable]])),
      .groups = "drop"
    )

  if (nrow(data_i) == 0) {
    return(empty_plot(paste("No group data for", group_name)))
  }

  ggplot(data_i, aes(x = year, y = value)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.3, na.rm = TRUE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(group_name, ": average ", get_var_label(variable)),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = get_var_label(variable)
    ) +
    base_dashboard_theme()
}

plot_imputed_vs_unimputed_country <- function(country_code, variable, year_min, year_max) {

  if (is.null(panel_unimputed)) {
    return(
      empty_plot(
        paste0(
          "Unimputed panel not found.\n",
          "Save data/processed/processed_panel_unimputed.rds before imputation."
        )
      )
    )
  }

  if (!(variable %in% names(panel_imputed))) {
    return(empty_plot(paste("Variable not in imputed panel:", variable)))
  }

  if (!(variable %in% names(panel_unimputed))) {
    return(empty_plot(paste("Variable not in unimputed panel:", variable)))
  }

  imputed_i <- panel_imputed |>
    filter(
      iso3c == country_code,
      year >= year_min,
      year <= year_max
    ) |>
    transmute(
      iso3c,
      year,
      value = .data[[variable]],
      series = "Imputed/final"
    )

  unimputed_i <- panel_unimputed |>
    filter(
      iso3c == country_code,
      year >= year_min,
      year <= year_max
    ) |>
    transmute(
      iso3c,
      year,
      value = .data[[variable]],
      series = "Original/unimputed"
    )

  if (nrow(imputed_i) == 0 && nrow(unimputed_i) == 0) {
    return(empty_plot(paste("No imputation comparison data for", country_code)))
  }

  ggplot() +
    geom_line(
      data = imputed_i,
      aes(x = year, y = value, linetype = series),
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    geom_point(
      data = unimputed_i,
      aes(x = year, y = value, shape = series, color = series),
      size = 2.2,
      na.rm = TRUE
    ) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": imputed vs unimputed"),
      subtitle = get_var_label(variable),
      x = NULL,
      y = get_var_label(variable),
      linetype = NULL,
      shape = NULL
    ) +
    base_dashboard_theme()
}

plot_extension_scm_path <- function(country_code, outcome, year_min, year_max) {

  if (is.null(extension_paths)) {
    return(empty_plot("Extension SCM paths file not found."))
  }

  data_i <- extension_paths |>
    filter(
      treated_iso3c == country_code,
      outcome == !!outcome,
      year >= year_min,
      year <= year_max
    ) |>
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

  if (nrow(data_i) == 0) {
    return(empty_plot(paste0("No SCM path for ", country_code, " / ", outcome)))
  }

  ggplot(data_i, aes(x = year, y = value, linetype = series)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": actual vs synthetic"),
      subtitle = get_var_label(outcome),
      x = NULL,
      y = get_var_label(outcome),
      linetype = NULL
    ) +
    base_dashboard_theme()
}

plot_extension_scm_gap <- function(country_code, outcome, year_min, year_max) {

  if (is.null(extension_paths)) {
    return(empty_plot("Extension SCM paths file not found."))
  }

  data_i <- extension_paths |>
    filter(
      treated_iso3c == country_code,
      outcome == !!outcome,
      year >= year_min,
      year <= year_max
    )

  if (nrow(data_i) == 0) {
    return(empty_plot(paste0("No SCM gap for ", country_code, " / ", outcome)))
  }

  ggplot(data_i, aes(x = year, y = gap)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": actual minus synthetic"),
      subtitle = get_var_label(outcome),
      x = NULL,
      y = "Actual - synthetic"
    ) +
    base_dashboard_theme()
}

plot_extension_group_average <- function(
    group_average_source,
    selected_groups,
    outcome,
    year_min,
    year_max
) {

  data_source <- if (group_average_source == "Reduced") {
    extension_group_averages_reduced
  } else {
    extension_group_averages
  }

  if (is.null(data_source)) {
    return(empty_plot("Group average file not found."))
  }

  data_i <- data_source |>
    filter(
      group %in% selected_groups,
      outcome == !!outcome,
      year >= year_min,
      year <= year_max
    )

  if (nrow(data_i) == 0) {
    return(empty_plot("No group-average data for this selection."))
  }

  ggplot(data_i, aes(x = year, y = value, color = group)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0("Group averages: ", get_var_label(outcome)),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = get_var_label(outcome),
      color = NULL
    ) +
    base_dashboard_theme()
}

plot_gdp_scm_path <- function(country_code, year_min, year_max) {

  if (is.null(gdp_paths)) {
    return(empty_plot("GDP SCM paths file not found."))
  }

  needed_cols <- c("treated_iso3c", "year", "actual", "synthetic")

  if (!all(needed_cols %in% names(gdp_paths))) {
    return(empty_plot("GDP paths file found, but expected columns are missing."))
  }

  data_i <- gdp_paths |>
    filter(
      treated_iso3c == country_code,
      year >= year_min,
      year <= year_max
    ) |>
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

  if (nrow(data_i) == 0) {
    return(empty_plot(paste0("No GDP SCM path for ", country_code)))
  }

  ggplot(data_i, aes(x = year, y = value, linetype = series)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": GDP actual vs synthetic"),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = "GDP per capita",
      linetype = NULL
    ) +
    base_dashboard_theme()
}

plot_gdp_scm_gap <- function(country_code, year_min, year_max) {

  if (is.null(gdp_paths)) {
    return(empty_plot("GDP SCM paths file not found."))
  }

  needed_cols <- c("treated_iso3c", "year", "gap")

  if (!all(needed_cols %in% names(gdp_paths))) {
    return(empty_plot("GDP paths file found, but expected columns are missing."))
  }

  data_i <- gdp_paths |>
    filter(
      treated_iso3c == country_code,
      year >= year_min,
      year <= year_max
    )

  if (nrow(data_i) == 0) {
    return(empty_plot(paste0("No GDP SCM gap for ", country_code)))
  }

  ggplot(data_i, aes(x = year, y = gap)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = treatment_year, linetype = "dashed") +
    labs(
      title = paste0(country_label(country_code), ": GDP actual minus synthetic"),
      subtitle = paste0("Dashed line = ", treatment_year),
      x = NULL,
      y = "Actual - synthetic"
    ) +
    base_dashboard_theme()
}

#### ========================================================================###
#### ======================== 6. CHART CONTROL MODULE =======================###
#### ========================================================================###

chart_controls <- function(id, title) {
  ns <- NS(id)

  extension_choices_named <- if (length(extension_outcome_choices) > 0) {
    setNames(
      extension_outcome_choices,
      vapply(extension_outcome_choices, get_var_label, character(1))
    )
  } else {
    c("No extension outcomes found" = "")
  }

  tagList(
    h4(title),

    selectInput(
      ns("chart_type"),
      "Chart type",
      choices = c(
        "Raw country trend",
        "Raw group average",
        "Imputed vs unimputed country trend",
        "Extension SCM: actual vs synthetic",
        "Extension SCM: gap",
        "Extension group averages",
        "GDP SCM: actual vs synthetic",
        "GDP SCM: gap"
      ),
      selected = "Raw country trend"
    ),

    conditionalPanel(
      condition = sprintf(
        paste(
          "input['%s'] == 'Raw country trend'",
          "input['%s'] == 'Imputed vs unimputed country trend'",
          "input['%s'] == 'Extension SCM: actual vs synthetic'",
          "input['%s'] == 'Extension SCM: gap'",
          "input['%s'] == 'GDP SCM: actual vs synthetic'",
          "input['%s'] == 'GDP SCM: gap'",
          sep = " || "
        ),
        ns("chart_type"),
        ns("chart_type"),
        ns("chart_type"),
        ns("chart_type"),
        ns("chart_type"),
        ns("chart_type")
      )
    ),

    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Raw group average'",
        ns("chart_type")
      ),
      selectInput(
        ns("group"),
        "Group",
        choices = group_choices,
        selected = "WAEMU"
      )
    ),

    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Extension group averages'",
        ns("chart_type")
      ),
      checkboxGroupInput(
        ns("groups_multi"),
        "Groups",
        choices = c("WAEMU", "CEMAC", "Donor pool", "Non-donor comparison"),
        selected = c("WAEMU", "CEMAC", "Donor pool")
      ),
      radioButtons(
        ns("group_average_source"),
        "Group average source",
        choices = c("Full", "Reduced"),
        selected = "Full",
        inline = TRUE
      )
    ),

    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Raw country trend' || input['%s'] == 'Raw group average'",
        ns("chart_type"),
        ns("chart_type")
      ),
      selectInput(
        ns("raw_variable"),
        "Raw variable",
        choices = setNames(
          raw_variable_choices,
          vapply(raw_variable_choices, get_var_label, character(1))
        ),
        selected = if ("trade_openness" %in% raw_variable_choices) {
          "trade_openness"
        } else {
          raw_variable_choices[1]
        }
      )
    ),

    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Imputed vs unimputed country trend'",
        ns("chart_type")
      ),
      selectInput(
        ns("imputation_variable"),
        "Variable",
        choices = setNames(
          dashboard_imputation_variables,
          vapply(dashboard_imputation_variables, get_var_label, character(1))
        ),
        selected = if ("agriculture" %in% dashboard_imputation_variables) {
          "agriculture"
        } else {
          dashboard_imputation_variables[1]
        }
      )
    ),

    conditionalPanel(
      condition = sprintf(
        paste(
          "input['%s'] == 'Extension SCM: actual vs synthetic'",
          "input['%s'] == 'Extension SCM: gap'",
          "input['%s'] == 'Extension group averages'",
          sep = " || "
        ),
        ns("chart_type"),
        ns("chart_type"),
        ns("chart_type")
      ),
      selectInput(
        ns("extension_outcome"),
        "Extension outcome",
        choices = extension_choices_named,
        selected = if ("XEU" %in% extension_outcome_choices) {
          "XEU"
        } else if (length(extension_outcome_choices) > 0) {
          extension_outcome_choices[1]
        } else {
          ""
        }
      )
    )
  )
}

#### ========================================================================###
#### ======================== 7. USER INTERFACE =============================###
#### ========================================================================###

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        paste0(
          "body { background-color: ", chart_bg, "; }",
          ".well { background-color: #ffffff; border-radius: 10px; }",
          ".selectize-input { background-color: #ffffff; }",
          ".control-label { font-weight: 600; }"
        )
      )
    )
  ),

  titlePanel("Macro 3: CFA franc replication paper data visualisation"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Global settings"),

      sliderInput(
        "year_range",
        "Year range",
        min = min(panel_df$year, na.rm = TRUE),
        max = max(panel_df$year, na.rm = TRUE),
        value = c(
          max(min(panel_df$year, na.rm = TRUE), 1980),
          min(max(panel_df$year, na.rm = TRUE), 2021)
        ),
        step = 1,
        sep = ""
      ),

      selectInput(
        "global_country",
        "Country",
        choices = country_choices,
        selected = default_country
    ),  

      hr(),

      chart_controls("left", "Left chart"),

      hr(),

      chart_controls("right", "Right chart")
    ),



    mainPanel(
      width = 9,

      fluidRow(
        column(
          width = 6,
          plotOutput("left_plot", height = "520px")
        ),
        column(
          width = 6,
          plotOutput("right_plot", height = "520px")
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 12,
          h4("Data status"),
          verbatimTextOutput("data_status")
        )
      )
    )
  )
)

#### ========================================================================###
#### ======================== 8. SERVER =====================================###
#### ========================================================================###

server <- function(input, output, session) {

  make_plot <- function(prefix) {

    chart_type <- input[[paste0(prefix, "-chart_type")]]
    year_range <- input$year_range

    year_min <- year_range[1]
    year_max <- year_range[2]

    country_selected <- input$global_country

    if (is.null(chart_type)) {
      return(empty_plot("No chart type selected."))
    }

    if (chart_type == "Raw country trend") {
      return(
        plot_raw_country(
          country_code = country_selected,
          variable = input[[paste0(prefix, "-raw_variable")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "Raw group average") {
      return(
        plot_raw_group(
          group_name = input[[paste0(prefix, "-group")]],
          variable = input[[paste0(prefix, "-raw_variable")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "Imputed vs unimputed country trend") {
      return(
        plot_imputed_vs_unimputed_country(
          country_code = country_selected,
          variable = input[[paste0(prefix, "-imputation_variable")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "Extension SCM: actual vs synthetic") {
      return(
        plot_extension_scm_path(
          country_code = country_selected,
          outcome = input[[paste0(prefix, "-extension_outcome")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "Extension SCM: gap") {
      return(
        plot_extension_scm_gap(
          country_code = country_selected,
          outcome = input[[paste0(prefix, "-extension_outcome")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "Extension group averages") {
      return(
        plot_extension_group_average(
          group_average_source = input[[paste0(prefix, "-group_average_source")]],
          selected_groups = input[[paste0(prefix, "-groups_multi")]],
          outcome = input[[paste0(prefix, "-extension_outcome")]],
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "GDP SCM: actual vs synthetic") {
      return(
        plot_gdp_scm_path(
          country_code = country_selected,
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    if (chart_type == "GDP SCM: gap") {
      return(
        plot_gdp_scm_gap(
          country_code = country_selected,
          year_min = year_min,
          year_max = year_max
        )
      )
    }

    empty_plot("Unknown chart type.")
  }

  output$left_plot <- renderPlot({
    make_plot("left")
  })

  output$right_plot <- renderPlot({
    make_plot("right")
  })

  output$data_status <- renderPrint({
    cat("Loaded files:\n")
    cat("- processed_panel_imputed.rds: ", !is.null(panel_imputed), "\n")
    cat("- processed_panel_unimputed.rds: ", !is.null(panel_unimputed), "\n")
    cat("- extension_trade_scm_paths_all.rds: ", !is.null(extension_paths), "\n")
    cat("- extension_trade_group_averages.csv: ", !is.null(extension_group_averages), "\n")
    cat("- extension_trade_group_averages_reduced.csv: ", !is.null(extension_group_averages_reduced), "\n")
    cat("- GDP SCM paths file: ", !is.null(gdp_paths), "\n")
    cat("- extension placebo p-values: ", !is.null(extension_placebo_pvalues), "\n\n")

    cat("Restricted raw variables:\n")
    print(raw_variable_choices)

    cat("\nRestricted imputation-comparison variables:\n")
    print(dashboard_imputation_variables)

    cat("\nAvailable extension SCM outcomes:\n")
    print(extension_outcome_choices)

    cat("\nCountry groups:\n")
    cat("WAEMU:", paste(waemu, collapse = ", "), "\n")
    cat("CEMAC:", paste(caemc, collapse = ", "), "\n")
    cat("Updated donor pool:", paste(donor_countries, collapse = ", "), "\n")
  })
}

#### ========================================================================###
#### ======================== 9. RUN APP ====================================###
#### ========================================================================###

shinyApp(ui = ui, server = server)