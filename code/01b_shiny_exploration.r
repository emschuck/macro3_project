library(shiny)
library(tidyverse)

# Load compiled dataset
df <- readRDS("./data/processed/processed_panel_imputed.rds")


# Keep only 1980-2021
df <- df |>
  filter(year >= 1980, year <= 2018)

# Automatically include all numeric variables except year
dashboard_vars <- df |>
  select(where(is.numeric)) |>
  select(-any_of("year")) |>
  names()

# Country group choices
group_choices <- df |>
  distinct(region) |>
  filter(!is.na(region)) |>
  arrange(region) |>
  pull(region)

# Country choices by group
country_choices <- df |>
  distinct(region, iso3c, country) |>
  filter(!is.na(region), !is.na(iso3c), !is.na(country)) |>
  arrange(region, country) |>
  mutate(label = paste0(country, " (", iso3c, ")"))

ui <- fluidPage(
  titlePanel("Data Exploration Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "group_selected",
        label = "Select country group:",
        choices = group_choices,
        selected = "WAEMU"
      ),

      selectInput(
        inputId = "country_selected",
        label = "Select country:",
        choices = NULL
      ),

      selectInput(
        inputId = "variable_selected",
        label = "Select variable:",
        choices = dashboard_vars,
        selected = dashboard_vars[1]
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

server <- function(input, output, session) {

  selected_data <- reactive({
    df |>
      filter(
        iso3c == input$country_selected,
        year >= 1980,
        year <= 2021
      ) |>
      mutate(value = .data[[input$variable_selected]])
  })

  observeEvent(input$group_selected, {

    countries_in_group <- country_choices |>
      filter(region == input$group_selected)

    updateSelectInput(
      session,
      inputId = "country_selected",
      choices = setNames(countries_in_group$iso3c, countries_in_group$label),
      selected = countries_in_group$iso3c[1]
    )
  })

  output$missing_summary <- renderTable({
    d <- selected_data()

    tibble(
      country = unique(d$country),
      variable = input$variable_selected,
      years_expected = 42,
      observations_available = sum(!is.na(d$value)),
      observations_missing = sum(is.na(d$value)),
      first_available_year = ifelse(
        all(is.na(d$value)),
        NA,
        min(d$year[!is.na(d$value)])
      ),
      last_available_year = ifelse(
        all(is.na(d$value)),
        NA,
        max(d$year[!is.na(d$value)])
      ),
      min_value = ifelse(
        all(is.na(d$value)),
        NA,
        min(d$value, na.rm = TRUE)
      ),
      max_value = ifelse(
        all(is.na(d$value)),
        NA,
        max(d$value, na.rm = TRUE)
      )
    )
  })

  output$variable_chart <- renderPlot({
    d <- selected_data()

    ggplot(d, aes(x = year, y = value)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      geom_point(size = 1.5, na.rm = TRUE) +
      geom_vline(xintercept = 2002, linetype = "dashed") +
      labs(
        title = paste(
          input$variable_selected,
          "for",
          unique(d$country),
          paste0("(", input$country_selected, ")")
        ),
        subtitle = "Dashed line marks 2002",
        x = "Year",
        y = input$variable_selected
      ) +
      theme_minimal(base_size = 12)
  })

  output$data_preview <- renderTable({
    selected_data() |>
      transmute(
        year,
        value
      )
  })
}

shinyApp(ui = ui, server = server)






# library(tidyverse)
# library(readr)

# # Load processed panel
# df <- readRDS("./data/processed/processed_panel.rds")

# # Create output folder
# dir.create("./output/data_checks", recursive = TRUE, showWarnings = FALSE)

# # Panel identifier columns
# id_vars <- c("iso3c", "country", "year", "region", "period")

# # Variables to check
# vars_to_check <- setdiff(names(df), id_vars)

# # Long missingness table: one row per country-variable
# missing_by_country_variable <- df %>%
#   pivot_longer(
#     cols = all_of(vars_to_check),
#     names_to = "variable",
#     values_to = "value",
#     values_transform = list(value = as.character)
#   ) %>%
#   group_by(region, iso3c, country, variable) %>%
#   summarise(
#     years_observed = n(),
#     non_missing = sum(!is.na(value)),
#     missing = sum(is.na(value)),
#     missing_share = missing / years_observed,
#     first_non_missing_year = ifelse(
#       all(is.na(value)),
#       NA_integer_,
#       min(year[!is.na(value)])
#     ),
#     last_non_missing_year = ifelse(
#       all(is.na(value)),
#       NA_integer_,
#       max(year[!is.na(value)])
#     ),
#     .groups = "drop"
#   ) %>%
#   arrange(region, iso3c, variable)

# # Save long CSV
# write_csv(
#   missing_by_country_variable,
#   "./output/data_checks/missing_by_country_variable.csv"
# )

# # Wide version: one row per country, one missing-count column per variable
# missing_wide <- missing_by_country_variable %>%
#   select(region, iso3c, country, variable, missing) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = missing,
#     names_prefix = "missing_"
#   ) %>%
#   arrange(region, iso3c)

# # Save wide CSV
# write_csv(
#   missing_wide,
#   "./output/data_checks/missing_by_country_wide.csv"
# )

# # Print rows with any missing values
# missing_by_country_variable %>%
#   filter(missing > 0) %>%
#   arrange(desc(missing)) %>%
#   print(n = 100)