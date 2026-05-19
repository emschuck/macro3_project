# =====================================================
# GDP source and calculation exploration
# Downloads WDI + PWT, then tests different GDP methods
# =====================================================

library(tidyverse)
library(WDI)
library(pwt10)
library(knitr)
library(readr)

# Set output directory

output_dir <- "output/gdp_exploration"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


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
write_csv(df, file.path(output_dir, "gdp_exploration_raw_panel.csv"))


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

make_combined_gdp_table <- function(data) {

  country_rows <- data |>
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
    select(group, country, `1990-2001`, `2002-2021`)

  average_rows <- country_rows |>
    group_by(group) |>
    summarise(
      country = paste("Average", first(group)),
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
    filename = file.path(output_dir, paste0("chart_", file_stub, ".png")),
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
    file.path(output_dir, paste0("checks_", file_stub, ".csv"))
  )

  cat(
    kable(
      checks,
      format = "latex",
      booktabs = TRUE,
      caption = paste("GDP completeness checks:", source, gdp_var)
    ),
    file = file.path(output_dir, paste0("checks_", file_stub, ".tex"))
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
    file.path(output_dir, paste0("table_", file_stub, ".csv"))
  )

  cat(
    kable(
      table_out,
      format = "latex",
      booktabs = TRUE,
      caption = paste("GDP table:", source, gdp_var, method)
    ),
    file = file.path(output_dir, paste0("table_", file_stub, ".tex"))
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

gdp_tests <- tibble::tribble(
~source, ~gdp_var, ~method,

# PWT output-side real GDP per capita
"PWT", "pwt_rgdpo_pc", "pct_growth",
"PWT", "pwt_rgdpo_pc", "growth_factor",
"PWT", "pwt_rgdpo_pc", "log_growth",
"PWT", "pwt_rgdpo_pc", "mean_index_to_period_start",

# PWT expenditure-side real GDP per capita
"PWT", "pwt_rgdpe_pc", "pct_growth",
"PWT", "pwt_rgdpe_pc", "growth_factor",
"PWT", "pwt_rgdpe_pc", "mean_index_to_period_start",

# PWT current PPP output-side GDP per capita
"PWT", "pwt_cgdpo_pc", "pct_growth",
"PWT", "pwt_cgdpo_pc", "growth_factor",
"PWT", "pwt_cgdpo_pc", "mean_index_to_period_start",

# PWT constant national-price GDP per capita
"PWT", "pwt_rgdpna_pc", "pct_growth",
"PWT", "pwt_rgdpna_pc", "growth_factor",
"PWT", "pwt_rgdpna_pc", "mean_index_to_period_start",

# WDI real GDP per capita variants
"WDI", "wdi_gdp_pc_ppp_constant", "pct_growth",
"WDI", "wdi_gdp_pc_ppp_constant", "growth_factor",
"WDI", "wdi_gdp_pc_ppp_constant", "log_growth",
"WDI", "wdi_gdp_pc_ppp_constant", "mean_index_to_period_start",

"WDI", "wdi_gdp_pc_constant", "pct_growth",
"WDI", "wdi_gdp_pc_constant", "growth_factor",
"WDI", "wdi_gdp_pc_constant", "mean_index_to_period_start",

# WDI current-price variants
"WDI", "wdi_gdp_pc_ppp_current", "pct_growth",
"WDI", "wdi_gdp_pc_ppp_current", "growth_factor",
"WDI", "wdi_gdp_pc_ppp_current", "mean_index_to_period_start",

"WDI", "wdi_gdp_pc_current", "pct_growth",
"WDI", "wdi_gdp_pc_current", "growth_factor",
"WDI", "wdi_gdp_pc_current", "mean_index_to_period_start",

# WDI direct growth rate
"WDI", "wdi_gdp_pc_growth", "direct",

# WDI aggregate GDP divided by PWT population
"WDI_PWTPOP", "wdi_gdp_ppp_constant_pc_pwtpop", "pct_growth",
"WDI_PWTPOP", "wdi_gdp_ppp_constant_pc_pwtpop", "growth_factor",
"WDI_PWTPOP", "wdi_gdp_ppp_constant_pc_pwtpop", "mean_index_to_period_start",

"WDI_PWTPOP", "wdi_gdp_ppp_current_pc_pwtpop", "pct_growth",
"WDI_PWTPOP", "wdi_gdp_ppp_current_pc_pwtpop", "growth_factor",
"WDI_PWTPOP", "wdi_gdp_ppp_current_pc_pwtpop", "mean_index_to_period_start",

"WDI_PWTPOP", "wdi_gdp_constant_pc_pwtpop", "pct_growth",
"WDI_PWTPOP", "wdi_gdp_constant_pc_pwtpop", "growth_factor",
"WDI_PWTPOP", "wdi_gdp_constant_pc_pwtpop", "mean_index_to_period_start",

"WDI_PWTPOP", "wdi_gdp_current_pc_pwtpop", "pct_growth",
"WDI_PWTPOP", "wdi_gdp_current_pc_pwtpop", "growth_factor",
"WDI_PWTPOP", "wdi_gdp_current_pc_pwtpop", "mean_index_to_period_start"
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

saveRDS(
  gdp_results,
  file.path(output_dir, "gdp_exploration_results.rds")
)

print("GDP exploration complete.")