
#### ========================================================================###
#### ======================== PROJECT SETUP =================================###
#### ========================================================================###

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


## Safe mean function wit hnas
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Load processed data
df <- readRDS("data/processed/processed_panel.rds")

#### ========================================================================###
#### ==================== == INFLATION TABLES ===============================###
#### ========================================================================###

# Helper function to replicate inflation Tables 1 and 2
make_inflation_table <- function(
  data,
  country_order,
  average_label = "Average inflation"
) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1980,
      year <= 2021
    ) |>
    mutate(
      table_period = case_when(
        year >= 1980 & year <= 2001 ~ "1980--2001",
        year >= 2002 & year <= 2021 ~ "2002--2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(table_period)) |>
    group_by(iso3c, country, table_period) |>
    summarise(
      inflation_avg = mean(inflation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = table_period,
      values_from = inflation_avg
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(country, `1980--2001`, `2002--2021`)

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `1980--2001` = mean(`1980--2001`, na.rm = TRUE),
      `2002--2021` = mean(`2002--2021`, na.rm = TRUE)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
}

# Table 1, Panel A: WAEMU
table1_waemu <- make_inflation_table(
  df,
  country_order = waemu_table1,
  average_label = "Average inflation for the WAEMU zone"
)

# Additional WAEMU average excluding Guinea-Bissau
waemu_avg_excl_gnb <- table1_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average inflation for the WAEMU zone"
  )) |>
  summarise(
    country = "Average inflation without Guinea-Bissau",
    `1980--2001` = mean(`1980--2001`, na.rm = TRUE),
    `2002--2021` = mean(`2002--2021`, na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

table1_waemu <- bind_rows(table1_waemu, waemu_avg_excl_gnb)

# Table 1, Panel B: CAEMC
table1_caemc <- make_inflation_table(
  df,
  country_order = caemc,
  average_label = "Average inflation for the CAEMC zone"
)

# Table 2: Non-CFA comparison countries
table2_non_cfa_inflation <- make_inflation_table(
  df,
  country_order = non_cfa_comparison_countries,
  average_label = "Average inflation"
)

# Print tables
#View(table1_waemu)
#View(table1_caemc)
#View(table2_non_cfa_inflation)

# Save LaTeX outputs
cat(
  kable(
    table1_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "CFA franc zone countries annual inflation rate: WAEMU"
  ),
  file = "output/tables/table1_panelA_waemu_inflation.tex"
)

cat(
  kable(
    table1_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CFA franc zone countries annual inflation rate: CAEMC"
  ),
  file = "output/tables/table1_panelB_caemc_inflation.tex"
)

cat(
  kable(
    table2_non_cfa_inflation,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA franc zone countries mean annual inflation rate"
  ),
  file = "output/tables/table2_non_cfa_inflation.tex"
)


# -----------------------------
# Regional inflation summary
# -----------------------------

inflation_region <- df |>
  filter(region %in% c("WAEMU", "CAEMC", "Donor pool", "Non-CFA comparison")) |>
  group_by(region, period) |>
  summarise(
    inflation_avg = mean(inflation, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = period,
    values_from = inflation_avg
  ) |>
  select(region, pre, post) |>
  mutate(
    pre = round(pre, 2),
    post = round(post, 2)
  )

#View(inflation_region)

#### ========================================================================###
#### ====================== GDP GROWTH GRAPH ================================###
#### ========================================================================###

# GDP per capita growth graph: WAEMU, CAEMC, non-CFA countries

df <- df |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(growth = 100 * (gdp_pc / lag(gdp_pc) - 1)) |>
  ungroup()

df_growth <- df |>
  filter(region %in% c("WAEMU", "CAEMC", "Non-CFA comparison")) |>
  mutate(region_plot = recode(region,
                              "Non-CFA comparison" = "Non-CFA countries")) |>
  group_by(region_plot, year) |>
  summarise(growth = mean(growth, na.rm = TRUE), .groups = "drop") |>
  filter(year >= 1990, year <= 2021)

p_gdp_growth <- ggplot(df_growth, aes(year, growth, color = region_plot)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linewidth = 1.1, color = "black") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  scale_y_continuous(breaks = seq(-25, 25, 5), limits = c(-25, 25)) +
  labs(
    title = "Evolution of GDP per capita growth",
    x = NULL,
    y = "Growth rate (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_gdp_growth)

ggsave(
  "output/figures/gdp_pc_growth_regions.png",
  p_gdp_growth,
  width = 12,
  height = 4,
  dpi = 300
)










#### ========================================================================###
#### ========================== GDP TABLES ===============================###
#### ========================================================================###

# =====================================================
# GDP per capita growth appendix tables
# Comparison PWT GDP per capita (gdp_pc) and WDI GDP per capita (gdp_pc_wdi)
# =====================================================


# Compute annual GDP per capita growth for both variables
df_gdp_growth <- df |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    growth_gdp_pc = 100 * (gdp_pc / lag(gdp_pc) - 1),
    growth_gdp_pc_wdi = 100 * (gdp_pc_wdi / lag(gdp_pc_wdi) - 1)
  ) |>
  ungroup()

# Helper function for GDP growth tables
make_gdp_growth_table <- function(data, country_order, average_label) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
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
    group_by(iso3c, country, period) |>
    summarise(
      gdp_pc_growth = safe_mean(growth_gdp_pc),
      gdp_pc_wdi_growth = safe_mean(growth_gdp_pc_wdi),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = period,
      values_from = c(gdp_pc_growth, gdp_pc_wdi_growth)
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(
      country,
      `gdp_pc 1990-2001` = `gdp_pc_growth_1990-2001`,
      `gdp_pc 2002-2021` = `gdp_pc_growth_2002-2021`,
      `gdp_pc_wdi 1990-2001` = `gdp_pc_wdi_growth_1990-2001`,
      `gdp_pc_wdi 2002-2021` = `gdp_pc_wdi_growth_2002-2021`
    )

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
      `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
      `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
      `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# Appendix 3, Panel A: WAEMU
gdp_growth_waemu <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = waemu_table1,
  average_label = "Average for the WAEMU area"
)

# Additional WAEMU average excluding Guinea-Bissau
gdp_growth_waemu_excl_gnb <- gdp_growth_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average for the WAEMU area"
  )) |>
  summarise(
    country = "Average without Guinea-Bissau",
    `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
    `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
    `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
    `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

gdp_growth_waemu <- bind_rows(gdp_growth_waemu, gdp_growth_waemu_excl_gnb)

# Appendix 3, Panel B: CAEMC
gdp_growth_caemc <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = caemc,
  average_label = "Average for the CAEMC zone"
)

# Appendix 4: Non-CFA comparison countries
gdp_growth_non_cfa <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = non_cfa_comparison_countries,
  average_label = "Average"
)

# Print tables
print(gdp_growth_waemu)
print(gdp_growth_caemc)
print(gdp_growth_non_cfa)

# Save LaTeX outputs
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  kable(
    gdp_growth_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "WAEMU countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelA_waemu_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CAEMC countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelB_caemc_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_non_cfa,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix4_non_cfa_gdp_growth.tex"
)





