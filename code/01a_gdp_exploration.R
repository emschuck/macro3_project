# =====================================================
# GDP source and calculation exploration
# Downloads WDI + PWT, then tests different GDP methods
# Produces composite regional and country-level charts
# =====================================================

library(tidyverse)
library(WDI)
library(pwt10)
library(knitr)
library(readr)
library(patchwork)

# =====================================================
# 1. Output directories
# =====================================================

output_dir <- "output/gdp_exploration"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

csv_output_dir <- file.path(output_dir, "csvs")
dir.create(csv_output_dir, recursive = TRUE, showWarnings = FALSE)

chart_output_dir <- file.path(output_dir, "charts")
dir.create(chart_output_dir, recursive = TRUE, showWarnings = FALSE)

country_chart_output_dir <- file.path(chart_output_dir, "countries")
dir.create(country_chart_output_dir, recursive = TRUE, showWarnings = FALSE)


# =====================================================
# 2. Country groups
# =====================================================

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


# =====================================================
# 3. Download PWT variables
# =====================================================

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


# =====================================================
# 4. Download WDI variables
# =====================================================

wdi_indicators <- c(
  # Per capita variables
  wdi_gdp_pc_ppp_constant = "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP, constant international $
  wdi_gdp_pc_ppp_current = "NY.GDP.PCAP.PP.CD", # GDP per capita, PPP, current international $
  wdi_gdp_pc_constant = "NY.GDP.PCAP.KD", # GDP per capita, constant 2015 US$
  wdi_gdp_pc_current = "NY.GDP.PCAP.CD", # GDP per capita, current US$
  wdi_gdp_pc_growth = "NY.GDP.PCAP.KD.ZG", # GDP per capita growth, annual %

  # Aggregate GDP variables
  wdi_gdp_ppp_constant = "NY.GDP.MKTP.PP.KD", # GDP, PPP, constant international $
  wdi_gdp_ppp_current = "NY.GDP.MKTP.PP.CD", # GDP, PPP, current international $
  wdi_gdp_constant = "NY.GDP.MKTP.KD", # GDP, constant 2015 US$
  wdi_gdp_current = "NY.GDP.MKTP.CD" # GDP, current US$
)

wdi <- WDI(
  country = gdp_exploration_countries,
  indicator = wdi_indicators,
  start = 1980,
  end = 2021
) |>
  rename(country_wdi = country)


# =====================================================
# 5. Merge WDI and PWT
# =====================================================

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

    # WDI aggregate GDP divided by PWT population.
    # PWT population is in millions, so multiply by 1e6.
    wdi_gdp_ppp_constant_pc_pwtpop = wdi_gdp_ppp_constant / (pop * 1e6),
    wdi_gdp_ppp_current_pc_pwtpop  = wdi_gdp_ppp_current  / (pop * 1e6),
    wdi_gdp_constant_pc_pwtpop     = wdi_gdp_constant     / (pop * 1e6),
    wdi_gdp_current_pc_pwtpop      = wdi_gdp_current      / (pop * 1e6)
  ) |>
  select(-country_wdi, -country_pwt)

write_csv(
  df,
  file.path(csv_output_dir, "gdp_exploration_raw_panel.csv")
)


# =====================================================
# 6. Helper functions
# =====================================================

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

make_safe_name <- function(source, variable, method) {
  out <- paste(source, variable, method, sep = "_")
  out <- gsub("[^A-Za-z0-9_]+", "_", out)
  tolower(out)
}

assign_country_group <- function(iso3c) {
  case_when(
    iso3c %in% waemu_table1 ~ "WAEMU",
    iso3c %in% caemc ~ "CAEMC",
    iso3c %in% non_cfa_comparison_countries ~ "Non-CFA comparison",
    TRUE ~ NA_character_
  )
}


# =====================================================
# 7. GDP calculation function
# =====================================================

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


# =====================================================
# 8. Completeness check function
# =====================================================

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


# =====================================================
# 9. Combined table function
# =====================================================

make_combined_gdp_table <- function(data) {

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
    select(group, iso3c, country, `1980-2021`, `1990-2001`, `2002-2021`)

  average_rows <- country_rows |>
    group_by(group) |>
    summarise(
      iso3c = NA_character_,
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


# =====================================================
# 10. Plot-data functions
# =====================================================

make_region_plot_data <- function(data, source, gdp_var, method, file_stub, test_order) {

  data |>
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
    ) |>
    mutate(
      source = source,
      gdp_variable = gdp_var,
      method = method,
      file_stub = file_stub,
      test_order = test_order,
      test_label = paste(source, gdp_var, method, sep = "\n")
    )
}

make_country_plot_data <- function(data, source, gdp_var, method, file_stub, test_order) {

  data |>
    filter(
      iso3c %in% gdp_exploration_countries,
      year >= 1990,
      year <= 2021
    ) |>
    transmute(
      iso3c,
      country,
      region,
      year,
      value = gdp_measure,
      source = source,
      gdp_variable = gdp_var,
      method = method,
      file_stub = file_stub,
      test_order = test_order,
      test_label = paste(source, gdp_var, method, sep = "\n")
    )
}


# =====================================================
# 11. Main exploration function
# =====================================================

run_gdp_exploration <- function(data, source, gdp_var, method, test_order) {

  file_stub <- make_safe_name(source, gdp_var, method)

  message("Running GDP exploration: ", file_stub)

  if (!(gdp_var %in% names(data))) {
    stop(paste("Variable", gdp_var, "not found in df."))
  }

  checks <- make_gdp_checks(data, gdp_var) |>
    mutate(
      source = source,
      gdp_variable = gdp_var,
      method = method,
      file_stub = file_stub,
      test_order = test_order
    )

  incomplete_rows <- checks |>
    filter(
      n_missing > 0 |
        !has_1990 | !has_2001 | !has_2002 | !has_2021
    )

  if (nrow(incomplete_rows) > 0) {
    message("WARNING: Some countries have incomplete GDP data for ", file_stub)
  } else {
    message("Data check passed for ", file_stub)
  }

  processed <- compute_gdp_measure(
    data = data,
    gdp_var = gdp_var,
    method = method
  )

  table_out <- make_combined_gdp_table(processed) |>
    mutate(
      source = source,
      gdp_variable = gdp_var,
      method = method,
      file_stub = file_stub,
      test_order = test_order
    ) |>
    select(
      source,
      gdp_variable,
      method,
      file_stub,
      test_order,
      everything()
    )

  region_plot_data <- make_region_plot_data(
    data = processed,
    source = source,
    gdp_var = gdp_var,
    method = method,
    file_stub = file_stub,
    test_order = test_order
  )

  country_plot_data <- make_country_plot_data(
    data = processed,
    source = source,
    gdp_var = gdp_var,
    method = method,
    file_stub = file_stub,
    test_order = test_order
  )

  list(
    checks = checks,
    table = table_out,
    region_plot_data = region_plot_data,
    country_plot_data = country_plot_data
  )
}


# =====================================================
# 12. Define all GDP tests
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

gdp_methods <- tibble::tibble(
  method = c(
    "pct_growth",
    "growth_factor",
    "log_growth",
    "index_change_from_1990"
  )
)

gdp_tests <- tidyr::crossing(
  gdp_variables,
  gdp_methods
)

gdp_tests <- bind_rows(
  gdp_tests,
  tibble::tribble(
    ~source, ~gdp_var,             ~method,
    "WDI",   "wdi_gdp_pc_growth",  "direct"
  )
) |>
  mutate(test_order = row_number())


# =====================================================
# 13. Run all tests
# =====================================================

gdp_results <- vector("list", nrow(gdp_tests))

for (i in seq_len(nrow(gdp_tests))) {
  gdp_results[[i]] <- run_gdp_exploration(
    data = df,
    source = gdp_tests$source[i],
    gdp_var = gdp_tests$gdp_var[i],
    method = gdp_tests$method[i],
    test_order = gdp_tests$test_order[i]
  )
}

names(gdp_results) <- make_safe_name(
  gdp_tests$source,
  gdp_tests$gdp_var,
  gdp_tests$method
)


# =====================================================
# 14. Combine outputs
# =====================================================

all_checks <- bind_rows(
  lapply(gdp_results, function(x) x$checks)
)

all_tables <- bind_rows(
  lapply(gdp_results, function(x) x$table)
)

all_region_plot_data <- bind_rows(
  lapply(gdp_results, function(x) x$region_plot_data)
)

all_country_plot_data <- bind_rows(
  lapply(gdp_results, function(x) x$country_plot_data)
)

write_csv(
  all_checks,
  file.path(csv_output_dir, "gdp_checks_all_tests.csv")
)

write_csv(
  all_tables,
  file.path(csv_output_dir, "gdp_tables_all_tests.csv")
)

write_csv(
  all_region_plot_data,
  file.path(csv_output_dir, "gdp_region_plot_data_all_tests.csv")
)

write_csv(
  all_country_plot_data,
  file.path(csv_output_dir, "gdp_country_plot_data_all_tests.csv")
)

saveRDS(
  gdp_results,
  file.path(output_dir, "gdp_exploration_results.rds")
)


# =====================================================
# 15. Composite regional multi-chart
# =====================================================

all_region_plot_data <- all_region_plot_data |>
  mutate(
    test_label = factor(
      test_label,
      levels = all_region_plot_data |>
        arrange(test_order) |>
        distinct(test_label) |>
        pull(test_label)
    )
  )

p_region_composite <- ggplot(
  all_region_plot_data,
  aes(x = year, y = value, color = region_plot)
) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = 2002, linewidth = 0.7, color = "black") +
  scale_x_continuous(breaks = seq(1990, 2020, 10)) +
  facet_wrap(~ test_label, scales = "free_y", ncol = 4) +
  labs(
    title = "GDP exploration charts: regional averages",
    subtitle = "WAEMU, CAEMC, and non-CFA comparison countries",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 7)
  )

ggsave(
  filename = file.path(chart_output_dir, "combined_gdp_exploration_charts_regions.png"),
  plot = p_region_composite,
  width = 32,
  height = 4 * ceiling(length(unique(all_region_plot_data$test_label)) / 4),
  dpi = 300,
  limitsize = FALSE
)

# =====================================================
# 16. Composite country multi-charts
# =====================================================

all_country_plot_data <- all_country_plot_data |>
  mutate(
    test_label = factor(
      test_label,
      levels = all_country_plot_data |>
        arrange(test_order) |>
        distinct(test_label) |>
        pull(test_label)
    )
  )

country_list <- all_country_plot_data |>
  distinct(iso3c, country, region) |>
  filter(!is.na(iso3c)) |>
  arrange(region, country)

for (i in seq_len(nrow(country_list))) {

  country_code_i <- country_list$iso3c[i]
  country_name_i <- country_list$country[i]
  region_i <- country_list$region[i]

  country_data_i <- all_country_plot_data |>
    filter(iso3c == country_code_i) |>
    mutate(
      value = as.numeric(value)
    ) |>
    filter(
      year >= 1990,
      year <= 2021,
      is.finite(value)
    )

  if (nrow(country_data_i) == 0) {
    message("Skipping empty country chart for ", country_code_i)
    next
  }

  p_country_i <- ggplot(
    country_data_i,
    aes(x = year, y = value, group = test_label)
  ) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 0.6, na.rm = TRUE) +
    geom_vline(xintercept = 2002, linewidth = 0.7, color = "black") +
    scale_x_continuous(breaks = seq(1990, 2020, 10)) +
    facet_wrap(~ test_label, scales = "free_y", ncol = 4) +
    labs(
      title = paste0("GDP exploration charts: ", country_name_i, " (", country_code_i, ")"),
      subtitle = paste0("Country group: ", region_i),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 7)
    )

  ggsave(
    filename = file.path(
      country_chart_output_dir,
      paste0("combined_gdp_exploration_charts_", country_code_i, ".png")
    ),
    plot = p_country_i,
    width = 32,
    height = 4 * ceiling(length(unique(country_data_i$test_label)) / 4),
    dpi = 300,
    limitsize = FALSE
  )
}


# =====================================================
# 17. Optional: country-specific method table function
# Manual use only - does not automatically output files.
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
      source,
      gdp_variable,
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

print("GDP exploration complete.")