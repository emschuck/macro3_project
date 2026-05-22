library(shiny)
library(tidyverse)

# =====================================================
# Load datasets
# =====================================================

df_unimputed <- readRDS("./data/processed/processed_panel_unimputed.rds")
df_imputed <- readRDS("./data/processed/processed_panel_imputed.rds")
df_synthetic <- readRDS("./data/processed/scm_export_wide.rds")

# Keep common year range
df_unimputed <- df_unimputed |>
  filter(year >= 1980, year <= 2021)

df_imputed <- df_imputed |>
  filter(year >= 1980, year <= 2021)

df_synthetic <- df_synthetic |>
  filter(
    year >= 1980,
    year <= 2019,
    unit_type %in% c("treated_actual", "synthetic")
  )

# =====================================================
# Helper functions
# =====================================================

get_numeric_vars <- function(data, id_vars) {
  data |>
    select(-any_of(id_vars)) |>
    select(where(is.numeric)) |>
    names()
}

make_country_choices_panel <- function(data) {
  data |>
    distinct(region, iso3c, country) |>
    filter(!is.na(region), !is.na(iso3c), !is.na(country)) |>
    arrange(region, country) |>
    mutate(label = paste0(country, " (", iso3c, ")"))
}

make_country_choices_synthetic <- function(data) {
  data |>
    distinct(treated_iso3c, treated_country) |>
    filter(!is.na(treated_iso3c), !is.na(treated_country)) |>
    arrange(treated_country) |>
    mutate(
      region = "Treated countries",
      iso3c = treated_iso3c,
      country = treated_country,
      label = paste0(treated_country, " (", treated_iso3c, ")")
    ) |>
    select(region, iso3c, country, label)
}

# Identifier variables to exclude from variable dropdown
panel_id_vars <- c(
  "iso2c",
  "iso3c",
  "country",
  "year",
  "region",
  "period"
)

synthetic_id_vars <- c(
  "treated_iso3c",
  "treated_country",
  "unit_type",
  "iso3c",
  "country",
  "year",
  "donor_iso3c",
  "donor_country",
  "donor_weight"
)

# =====================================================
# User interface
# =====================================================

ui <- fluidPage(
  titlePanel("Data Exploration Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "source_selected",
        label = "Select data source:",
        choices = c(
          "Unimputed panel" = "unimputed",
          "Imputed panel" = "imputed",
          "Synthetic export" = "synthetic"
        ),
        selected = "unimputed"
      ),

      selectInput(
        inputId = "group_selected",
        label = "Select country group:",
        choices = NULL
      ),

      selectInput(
        inputId = "country_selected",
        label = "Select country:",
        choices = NULL
      ),

      selectInput(
        inputId = "variable_selected",
        label = "Select variable:",
        choices = NULL
      )
    ),

    mainPanel(
      h3("Data availability"),
      tableOutput("missing_summary"),

      h3("Chart"),
      plotOutput("variable_chart", height = "450px"),

      h3("Data preview"),
      tableOutput("data_preview")
    )
  )
)

# =====================================================
# Server
# =====================================================

server <- function(input, output, session) {

  # Return selected dataset
  active_data <- reactive({
    req(input$source_selected)

    if (input$source_selected == "unimputed") {
      df_unimputed
    } else if (input$source_selected == "imputed") {
      df_imputed
    } else {
      df_synthetic
    }
  })

  # Return active country choices
  active_country_choices <- reactive({
    if (input$source_selected %in% c("unimputed", "imputed")) {
      make_country_choices_panel(active_data())
    } else {
      make_country_choices_synthetic(active_data())
    }
  })

  # Return active variable choices
  active_variable_choices <- reactive({
    if (input$source_selected %in% c("unimputed", "imputed")) {
      get_numeric_vars(active_data(), panel_id_vars)
    } else {
      get_numeric_vars(active_data(), synthetic_id_vars)
    }
  })

  # Update group dropdown when source changes
  observeEvent(input$source_selected, {
    choices_i <- active_country_choices()

    group_choices_i <- choices_i |>
      distinct(region) |>
      arrange(region) |>
      pull(region)

    updateSelectInput(
      session,
      inputId = "group_selected",
      choices = group_choices_i,
      selected = group_choices_i[1]
    )

    vars_i <- active_variable_choices()

    updateSelectInput(
      session,
      inputId = "variable_selected",
      choices = vars_i,
      selected = vars_i[1]
    )
  }, ignoreInit = FALSE)

  # Update country dropdown when source or group changes
  observeEvent(
    list(input$source_selected, input$group_selected),
    {
      req(input$group_selected)

      countries_i <- active_country_choices() |>
        filter(region == input$group_selected)

      updateSelectInput(
        session,
        inputId = "country_selected",
        choices = setNames(countries_i$iso3c, countries_i$label),
        selected = countries_i$iso3c[1]
      )
    },
    ignoreInit = FALSE
  )

  # Selected data for plotting
  selected_data <- reactive({
    req(
      input$source_selected,
      input$country_selected,
      input$variable_selected
    )

    if (input$source_selected %in% c("unimputed", "imputed")) {

      active_data() |>
        filter(
          iso3c == input$country_selected,
          year >= 1980,
          year <= 2021
        ) |>
        mutate(
          value = .data[[input$variable_selected]],
          series = "Actual"
        )

    } else {

      active_data() |>
        filter(
          treated_iso3c == input$country_selected,
          unit_type %in% c("treated_actual", "synthetic"),
          year >= 1980,
          year <= 2019
        ) |>
        mutate(
          value = .data[[input$variable_selected]],
          series = case_when(
            unit_type == "treated_actual" ~ "Actual",
            unit_type == "synthetic" ~ "Synthetic",
            TRUE ~ unit_type
          )
        )
    }
  })

  # Missing summary
  output$missing_summary <- renderTable({
    d <- selected_data()

    d |>
      group_by(series) |>
      summarise(
        country = first(country),
        variable = input$variable_selected,
        years_expected = n_distinct(year),
        observations_available = sum(!is.na(value)),
        observations_missing = sum(is.na(value)),
        first_available_year = ifelse(
          all(is.na(value)),
          NA_integer_,
          min(year[!is.na(value)])
        ),
        last_available_year = ifelse(
          all(is.na(value)),
          NA_integer_,
          max(year[!is.na(value)])
        ),
        min_value = ifelse(
          all(is.na(value)),
          NA_real_,
          min(value, na.rm = TRUE)
        ),
        max_value = ifelse(
          all(is.na(value)),
          NA_real_,
          max(value, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  })

  # Chart
  output$variable_chart <- renderPlot({
    d <- selected_data()

    ggplot(d, aes(x = year, y = value, linetype = series)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      geom_point(size = 1.3, na.rm = TRUE) +
      geom_vline(xintercept = 2002, linetype = "dashed") +
      labs(
        title = paste(
          input$variable_selected,
          "for",
          unique(d$country)
        ),
        subtitle = paste(
          "Source:",
          input$source_selected,
          "| Dashed line marks 2002"
        ),
        x = "Year",
        y = input$variable_selected,
        linetype = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom"
      )
  })

  # Preview table
  output$data_preview <- renderTable({
    selected_data() |>
      select(
        year,
        series,
        value
      ) |>
      arrange(year, series)
  })
}

shinyApp(ui = ui, server = server)