# =====================================================
# GDP source and calculation exploration
# Downloads WDI + PWT, then tests different GDP methods
# =====================================================

library(tidyverse)
library(WDI)
library(pwt10)
library(knitr)
library(readr)
library(patchwork)

# Set output directory

output_dir <- "output/gdp_exploration"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

table_output_dir <- "output/gdp_exploration/tables"
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)


csv_output_dir <- "output/gdp_exploration/csvs"
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)


chart_output_dir <- "output/gdp_exploration/charts"
dir.create(chart_output_dir, recursive = TRUE, showWarnings = FALSE)


country_output_dir <- "output/gdp_exploration/countries"
dir.create(country_output_dir, recursive = TRUE, showWarnings = FALSE)

# Country groups

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

caemc <- c(
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "TCD", # Chad
  "COG", # Republic of Congo
  "GNQ", # Equatorial Guinea
  "GAB"  # Gabon
)

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

gdp_exploration_countries <- c(
  waemu_table1,
  caemc,
  non_cfa_comparison_countries
)


# Download PWT variables

data("pwt10.01")

pwt <- pwt10.01 |>
  select(
    isocode,
    country,
    year,
    rgdpo,  # Output-side real GDP at chained PPPs, million 2021 US$
    rgdpe,  # Expenditure-side real GDP at chained PPPs, million 2021 US$
    cgdpo,  # Output-side GDP at current PPPs, million 2021 US$
    cgdpe,  # Expenditure-side GDP at current PPPs, million 2021 US$
    rgdpna, # Real GDP at constant national prices, million 2021 US$
    pop     # Population, millions
  ) |>
  rename(
    iso3c = isocode,
    country_pwt = country
  ) |>
  filter(
    iso3c %in% gdp_exploration_countries,
    year >= 1980,
    year <= 2021
  ) |>
  mutate(
    pwt_rgdpo_pc  = rgdpo / pop,
    pwt_rgdpe_pc  = rgdpe / pop,
    pwt_cgdpo_pc  = cgdpo / pop,
    pwt_cgdpe_pc  = cgdpe / pop,
    pwt_rgdpna_pc = rgdpna / pop
  )


# Download WDI variables

wdi_indicators <- c(
  # per capita variables
  wdi_gdp_pc_ppp_constant = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP, constant international $
  wdi_gdp_pc_ppp_current  = "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP, current international $
  wdi_gdp_pc_constant     = "NY.GDP.PCAP.KD",    # GDP per capita, constant 2015 US$
  wdi_gdp_pc_current      = "NY.GDP.PCAP.CD",    # GDP per capita, current US$
  wdi_gdp_pc_growth       = "NY.GDP.PCAP.KD.ZG", # GDP per capita growth, annual %

  # Not per capita
  wdi_gdp_ppp_constant    = "NY.GDP.MKTP.PP.KD", # GDP, PPP, constant international $
  wdi_gdp_ppp_current     = "NY.GDP.MKTP.PP.CD", # GDP, PPP, current international $
  wdi_gdp_constant        = "NY.GDP.MKTP.KD",    # GDP, constant 2015 US$
  wdi_gdp_current         = "NY.GDP.MKTP.CD"     # GDP, current US$
)

wdi <- WDI(
  country = gdp_exploration_countries,
  indicator = wdi_indicators,
  start = 1980,
  end = 2021
) |>
  rename(country_wdi = country)


# Merge WDI and PWT, with pc calculations for aggregate values

df <- wdi |>
  left_join(pwt, by = c("iso3c", "year")) |>
  mutate(
    country = coalesce(country_wdi, country_pwt),

    region = case_when(
      iso3c %in% waemu_table1 ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
      TRUE ~ NA_character_
    ),

    # WDI aggregate GDP divided by PWT population (in millions)
    wdi_gdp_ppp_constant_pc_pwtpop = wdi_gdp_ppp_constant / (pop * 1e6),
    wdi_gdp_ppp_current_pc_pwtpop  = wdi_gdp_ppp_current  / (pop * 1e6),
    wdi_gdp_constant_pc_pwtpop     = wdi_gdp_constant     / (pop * 1e6),
    wdi_gdp_current_pc_pwtpop      = wdi_gdp_current      / (pop * 1e6)
  ) |>
  select(-country_wdi, -country_pwt)

# Save the GDP exploration raw panel
write_csv(df, file.path(csv_output_dir, "gdp_exploration_raw_panel.csv"))


# misc helper functions

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

make_safe_name <- function(source, variable, method) {
  paste(source, variable, method, sep = "_") |>
    gsub("[^A-Za-z0-9_]+", "_", x = _) |>
    tolower()
}

assign_country_group <- function(iso3c) {
  case_when(
    iso3c %in% waemu_table1 ~ "WAEMU",
    iso3c %in% caemc ~ "CAEMC",
    iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
    TRUE ~ NA_character_
  )
}


# Functions for different GDP calculation methods

# Methods:
#   pct_growth:
#     100 * (GDP_t / GDP_{t-1} - 1)
#
#   growth_factor:
#     GDP_t / GDP_{t-1}
#
#   log_growth:
#     100 * (log(GDP_t) - log(GDP_{t-1}))
#
#   avg_relative_level_change:
#     ((GDP_end - GDP_start) / GDP_start) / number_of_years
#
#   avg_relative_level_change_pct:
#     100 * ((GDP_end - GDP_start) / GDP_start) / number_of_years
#
#   cagr_factor:
#     (GDP_end / GDP_start)^(1 / number_of_years)
#
#   cagr_pct:
#     100 * ((GDP_end / GDP_start)^(1 / number_of_years) - 1)
#
#   mean_index_to_period_start:
#     Average of GDP_t / GDP_start within each period
#
#   direct:
#     Uses the variable directly. This is mainly for WDI's direct growth series.

compute_gdp_measure <- function(data, gdp_var, method) {

  data <- data |>
    arrange(iso3c, year) |>
    group_by(iso3c) |>
    mutate(
      gdp_value = .data[[gdp_var]],
      pct_growth = 100 * (gdp_value / lag(gdp_value) - 1),
      growth_factor = gdp_value / lag(gdp_value),
      log_growth = 100 * (log(gdp_value) - log(lag(gdp_value)))
    ) |>
    ungroup()

  if (method %in% c("pct_growth", "growth_factor", "log_growth")) {
    data <- data |>
      mutate(gdp_measure = .data[[method]])
  }

  if (method == "direct") {
    data <- data |>
      mutate(gdp_measure = gdp_value)
  }

  if (method == "index_change_from_1990") {
    data <- data |>
      group_by(iso3c) |>
      mutate(
        gdp_1990 = gdp_value[year == 1990][1],
        gdp_measure = 100 * (gdp_value / gdp_1990 - 1)
      ) |>
      ungroup()
  }

  if (method == "mean_index_to_period_start") {
    data <- data |>
      group_by(iso3c) |>
      mutate(
        gdp_start_1990 = gdp_value[year == 1990][1],
        gdp_start_2002 = gdp_value[year == 2002][1],
        gdp_measure = case_when(
          year >= 1990 & year <= 2001 ~ gdp_value / gdp_start_1990,
          year >= 2002 & year <= 2021 ~ gdp_value / gdp_start_2002,
          TRUE ~ NA_real_
        )
      ) |>
      ungroup()
  }

  if (method %in% c(
    "avg_relative_level_change",
    "avg_relative_level_change_pct",
    "cagr_factor",
    "cagr_pct"
  )) {

    endpoint_data <- data |>
      filter(year %in% c(1990, 2001, 2002, 2021)) |>
      select(iso3c, year, gdp_value) |>
      pivot_wider(
        names_from = year,
        values_from = gdp_value,
        names_prefix = "gdp_"
      ) |>
      mutate(
        measure_1990_2001 = case_when(
          method == "avg_relative_level_change" ~
            ((gdp_2001 - gdp_1990) / gdp_1990) / (2001 - 1990),
          method == "avg_relative_level_change_pct" ~
            100 * ((gdp_2001 - gdp_1990) / gdp_1990) / (2001 - 1990),
          method == "cagr_factor" ~
            (gdp_2001 / gdp_1990)^(1 / (2001 - 1990)),
          method == "cagr_pct" ~
            100 * ((gdp_2001 / gdp_1990)^(1 / (2001 - 1990)) - 1),
          TRUE ~ NA_real_
        ),
        measure_2002_2021 = case_when(
          method == "avg_relative_level_change" ~
            ((gdp_2021 - gdp_2002) / gdp_2002) / (2021 - 2002),
          method == "avg_relative_level_change_pct" ~
            100 * ((gdp_2021 - gdp_2002) / gdp_2002) / (2021 - 2002),
          method == "cagr_factor" ~
            (gdp_2021 / gdp_2002)^(1 / (2021 - 2002)),
          method == "cagr_pct" ~
            100 * ((gdp_2021 / gdp_2002)^(1 / (2021 - 2002)) - 1),
          TRUE ~ NA_real_
        )
      ) |>
      select(iso3c, measure_1990_2001, measure_2002_2021)

    data <- data |>
      left_join(endpoint_data, by = "iso3c") |>
      mutate(
        gdp_measure = case_when(
          year >= 1990 & year <= 2001 ~ measure_1990_2001,
          year >= 2002 & year <= 2021 ~ measure_2002_2021,
          TRUE ~ NA_real_
        )
      )
  }

  data
}


# Data completeness check function

make_gdp_checks <- function(data, gdp_var) {

  data |>
    filter(
      iso3c %in% gdp_exploration_countries,
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      gdp_value = .data[[gdp_var]],
      group = assign_country_group(iso3c)
    ) |>
    group_by(group, iso3c, country) |>
    summarise(
      n_rows = n(),
      n_missing = sum(is.na(gdp_value)),
      has_1990 = any(year == 1990 & !is.na(gdp_value)),
      has_2001 = any(year == 2001 & !is.na(gdp_value)),
      has_2002 = any(year == 2002 & !is.na(gdp_value)),
      has_2021 = any(year == 2021 & !is.na(gdp_value)),
      complete_1990_2001 = all(!is.na(gdp_value[year >= 1990 & year <= 2001])),
      complete_2002_2021 = all(!is.na(gdp_value[year >= 2002 & year <= 2021])),
      .groups = "drop"
    ) |>
    arrange(group, iso3c)
}


# Function to make combined table with vaalues

# Function to make combined table with values

make_combined_gdp_table <- function(data) {

  # Calculate full-period average: 1980-2021
  full_period_rows <- data |>
    filter(
      iso3c %in% gdp_exploration_countries,
      year >= 1980,
      year <= 2021
    ) |>
    mutate(
      group = assign_country_group(iso3c),
      period = "1980-2021"
    ) |>
    group_by(group, iso3c, country, period) |>
    summarise(
      value = safe_mean(gdp_measure),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = period,
      values_from = value
    )

  # Calculate pre- and post-period averages
  pre_post_rows <- data |>
    filter(
      iso3c %in% gdp_exploration_countries,
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      group = assign_country_group(iso3c),
      period = case_when(
        year >= 1990 & year <= 2001 ~ "1990-2001",
        year >= 2002 & year <= 2021 ~ "2002-2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period)) |>
    group_by(group, iso3c, country, period) |>
    summarise(
      value = safe_mean(gdp_measure),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = period,
      values_from = value
    )

  # Combine full period with pre/post periods
  country_rows <- pre_post_rows |>
    left_join(
      full_period_rows,
      by = c("group", "iso3c", "country")
    ) |>
    mutate(
      order = case_when(
        iso3c %in% waemu_table1 ~ match(iso3c, waemu_table1),
        iso3c %in% caemc ~ length(waemu_table1) + match(iso3c, caemc),
        iso3c %in% non_cfa_comparison_countries ~
          length(waemu_table1) + length(caemc) +
          match(iso3c, non_cfa_comparison_countries),
        TRUE ~ NA_integer_
      )
    ) |>
    arrange(order) |>
    select(group, country, `1980-2021`, `1990-2001`, `2002-2021`)

  # Group averages
  average_rows <- country_rows |>
    group_by(group) |>
    summarise(
      country = paste("Average", first(group)),
      `1980-2021` = safe_mean(`1980-2021`),
      `1990-2001` = safe_mean(`1990-2001`),
      `2002-2021` = safe_mean(`2002-2021`),
      .groups = "drop"
    )

  bind_rows(country_rows, average_rows) |>
    mutate(
      group = factor(
        group,
        levels = c("WAEMU", "CAEMC", "Non-CFA comparison")
      )
    ) |>
    arrange(group, country) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}


# Function to create charts

make_gdp_chart <- function(data, source, gdp_var, method, file_stub) {

  plot_data <- data |>
    filter(
      region %in% c("WAEMU", "CAEMC", "Non-CFA comparison"),
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      region_plot = recode(
        region,
        "Non-CFA comparison" = "Non-CFA countries"
      )
    ) |>
    group_by(region_plot, year) |>
    summarise(
      value = safe_mean(gdp_measure),
      .groups = "drop"
    )

  p <- ggplot(plot_data, aes(x = year, y = value, color = region_plot)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = 2002, linewidth = 1.1, color = "black") +
    scale_x_continuous(breaks = seq(1990, 2020, 2)) +
    labs(
      title = paste("GDP exploration:", source, gdp_var, method),
      x = NULL,
      y = method,
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(chart_output_dir, paste0("chart_", file_stub, ".png")),
    plot = p,
    width = 12,
    height = 4,
    dpi = 300
  )

  p
}


# GDP exploration functino

run_gdp_exploration <- function(data, source, gdp_var, method) {

  file_stub <- make_safe_name(source, gdp_var, method)

  message("Running GDP exploration: ", file_stub)

  if (!(gdp_var %in% names(data))) {
    stop(paste("Variable", gdp_var, "not found in df."))
  }

  checks <- make_gdp_checks(data, gdp_var)

  write_csv(
    checks,
    file.path(csv_output_dir, paste0("checks_", file_stub, ".csv"))
  )

  cat(
    kable(
      checks,
      format = "latex",
      booktabs = TRUE,
      caption = paste("GDP completeness checks:", source, gdp_var)
    ),
    file = file.path(table_output_dir, paste0("checks_", file_stub, ".tex"))
  )

  incomplete_rows <- checks |>
    filter(
      n_missing > 0 |
        !has_1990 | !has_2001 | !has_2002 | !has_2021
    )

  if (nrow(incomplete_rows) > 0) {
    message("WARNING: Some countries have incomplete GDP data. Check file: checks_", file_stub, ".csv")
  } else {
    message("Data check passed: no missing GDP values for 1990-2021 and all endpoint years present.")
  }

  processed <- compute_gdp_measure(
    data = data,
    gdp_var = gdp_var,
    method = method
  )

  table_out <- make_combined_gdp_table(processed)

  write_csv(
    table_out,
    file.path(csv_output_dir, paste0("table_", file_stub, ".csv"))
  )

  cat(
    kable(
      table_out,
      format = "latex",
      booktabs = TRUE,
      caption = paste("GDP table:", source, gdp_var, method)
    ),
    file = file.path(table_output_dir, paste0("table_", file_stub, ".tex"))
  )

  plot_out <- make_gdp_chart(
    data = processed,
    source = source,
    gdp_var = gdp_var,
    method = method,
    file_stub = file_stub
  )

  list(
    checks = checks,
    table = table_out,
    plot = plot_out
  )
}


# Define all GDP tests to do
# =====================================================
# GDP variables to test
# =====================================================

gdp_variables <- tibble::tribble(
  ~source,        ~gdp_var,
  "PWT",          "pwt_rgdpo_pc",
  "PWT",          "pwt_rgdpe_pc",
  "PWT",          "pwt_cgdpo_pc",
  "PWT",          "pwt_cgdpe_pc",
  "PWT",          "pwt_rgdpna_pc",

  "WDI",          "wdi_gdp_pc_ppp_constant",
  "WDI",          "wdi_gdp_pc_ppp_current",
  "WDI",          "wdi_gdp_pc_constant",
  "WDI",          "wdi_gdp_pc_current",

  "WDI_PWTPOP",   "wdi_gdp_ppp_constant_pc_pwtpop",
  "WDI_PWTPOP",   "wdi_gdp_ppp_current_pc_pwtpop",
  "WDI_PWTPOP",   "wdi_gdp_constant_pc_pwtpop",
  "WDI_PWTPOP",   "wdi_gdp_current_pc_pwtpop"
)

# =====================================================
# GDP calculation methods to test
# =====================================================

gdp_methods <- tibble::tibble(
  method = c(
    "pct_growth",
    "growth_factor",
    "log_growth",
    # "mean_index_to_period_start",
    "index_change_from_1990"
    # "avg_relative_level_change",
    # "avg_relative_level_change_pct",
    # "cagr_factor",
    # "cagr_pct"
  )
)

# Create all source-variable-method combinations
gdp_tests <- tidyr::crossing(
  gdp_variables,
  gdp_methods
)

# Add WDI direct growth rate separately because it is already a growth rate
gdp_tests <- bind_rows(
  gdp_tests,
  tibble::tribble(
    ~source, ~gdp_var,             ~method,
    "WDI",   "wdi_gdp_pc_growth",  "direct"
  )
)



# Run all tests

gdp_results <- vector("list", nrow(gdp_tests))


for (i in seq_len(nrow(gdp_tests))) {
  gdp_results[[i]] <- run_gdp_exploration(
    data = df,
    source = gdp_tests$source[i],
    gdp_var = gdp_tests$gdp_var[i],
    method = gdp_tests$method[i]
  )
}

names(gdp_results) <- make_safe_name(
  gdp_tests$source,
  gdp_tests$gdp_var,
  gdp_tests$method
)

#### Saved combined chart

plot_list <- lapply(gdp_results, function(x) x$plot)

# Remove any NULL plots, just in case
plot_list <- plot_list[!sapply(plot_list, is.null)]

combined_plot <- wrap_plots(
  plot_list,
  ncol = 4
) +
  plot_annotation(
    title = "GDP exploration charts"
  )

ggsave(
  filename = file.path(chart_output_dir, "combined_gdp_exploration_charts.png"),
  plot = combined_plot,
  width = 32,
  height = 4 * ceiling(length(plot_list) / 4),
  dpi = 300,
  limitsize = FALSE
)

saveRDS(
  gdp_results,
  file.path(output_dir, "gdp_exploration_results.rds")
)

print("GDP exploration complete.")



# =====================================================
# Country-specific method comparison table
# =====================================================

make_country_method_table <- function(data, tests, country_iso3c) {

  country_name <- data |>
    filter(iso3c == country_iso3c) |>
    summarise(country = first(na.omit(country))) |>
    pull(country)

  output_rows <- list()

  for (i in seq_len(nrow(tests))) {

    source_i <- tests$source[i]
    gdp_var_i <- tests$gdp_var[i]
    method_i <- tests$method[i]

    if (!(gdp_var_i %in% names(data))) {
      warning(paste("Skipping", gdp_var_i, "- variable not found in data."))
      next
    }

    processed_i <- compute_gdp_measure(
      data = data,
      gdp_var = gdp_var_i,
      method = method_i
    )

    # Full-period average: 1980-2021
    row_all <- processed_i |>
      filter(
        iso3c == country_iso3c,
        year >= 1980,
        year <= 2021
      ) |>
      summarise(
        period = "1980-2021",
        value = safe_mean(gdp_measure),
        n_obs = sum(!is.na(gdp_measure))
      )

    # Pre- and post-period averages
    row_sub <- processed_i |>
      filter(
        iso3c == country_iso3c,
        year >= 1990,
        year <= 2021
      ) |>
      mutate(
        period = case_when(
          year >= 1990 & year <= 2001 ~ "1990-2001",
          year >= 2002 & year <= 2021 ~ "2002-2021",
          TRUE ~ NA_character_
        )
      ) |>
      filter(!is.na(period)) |>
      group_by(period) |>
      summarise(
        value = safe_mean(gdp_measure),
        n_obs = sum(!is.na(gdp_measure)),
        .groups = "drop"
      )

    row_i <- bind_rows(row_all, row_sub) |>
      pivot_wider(
        names_from = period,
        values_from = c(value, n_obs)
      ) |>
      mutate(
        source = source_i,
        gdp_variable = gdp_var_i,
        method = method_i
      ) |>
      select(
        source,
        gdp_variable,
        method,
        `1980-2021` = `value_1980-2021`,
        `1990-2001` = `value_1990-2001`,
        `2002-2021` = `value_2002-2021`,
        `n 1980-2021` = `n_obs_1980-2021`,
        `n 1990-2001` = `n_obs_1990-2001`,
        `n 2002-2021` = `n_obs_2002-2021`
      )

    output_rows[[length(output_rows) + 1]] <- row_i
  }

  table_out <- bind_rows(output_rows) |>
    mutate(
      country_iso3c = country_iso3c,
      country = country_name,
      across(c(`1980-2021`, `1990-2001`, `2002-2021`), ~ round(.x, 4))
    ) |>
    select(
      country,
      method,
      `1980-2021`,
      `1990-2001`,
      `2002-2021`,
      `n 1980-2021`,
      `n 1990-2001`,
      `n 2002-2021`
    )

  table_out
}


# =====================================================
# Produce country-specific comparison tables
# =====================================================

country_to_check <- "AGO"

country_method_table <- make_country_method_table(
  data = df,
  tests = gdp_tests,
  country_iso3c = country_to_check
)

print(country_method_table)

write_csv(
  country_method_table,
  file.path(
    country_output_dir,
    paste0("country_method_table_", country_to_check, ".csv")
  )
)

cat(
  kable(
    country_method_table,
    format = "latex",
    booktabs = TRUE,
    caption = paste("GDP method comparison for", country_to_check)
  ),
  file = file.path(
    country_output_dir,
    paste0("country_method_table_", country_to_check, ".tex")
  )
)
