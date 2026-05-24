library(dplyr)
library(knitr)

# Load shared constants
source("code/00_constants.R")

# Loading the data
df_synth <- readRDS("data/processed/scm_export_wide.rds")

# List of treated and synthetic countries
synthetic_countries <- paste0("SYN_", treated_countries)

avg_countries <- c(treated_countries, synthetic_countries)

# Keep only the treated and synthetic countries
df2 <- df_synth |>
  filter(
    iso3c %in% avg_countries,
    year >= 1980,
    year <= 2019
  ) |>
  select(
    iso3c, country, year,
    gdp_selected,
    agriculture, #industry, 
    govt_share, invest_share,
    oda_share, fdi, labour, polity2
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

# Make the table for the actual and synthetic data
avg_df <- df2 |>
  group_by(iso3c, country) |>
  summarise(
    avg_agriculture = mean(agriculture, na.rm = TRUE),
    # avg_industry = mean(industry, na.rm = TRUE),
    avg_government = mean(govt_share, na.rm = TRUE),
    avg_oda = mean(oda_share, na.rm = TRUE),
    avg_fdi = mean(fdi, na.rm = TRUE),
    avg_labour = mean(labour, na.rm = TRUE),
    avg_institution = mean(polity2, na.rm = TRUE),
    .groups = "drop"
  )

# Order rows so each actual treated country is followed by its synthetic control
ordering_df <- tibble(
  iso3c = rep(treated_countries, each = 2),
  iso3c_ordered = as.vector(rbind(
    treated_countries,
    paste0("SYN_", treated_countries)
  )),
  order = seq_along(as.vector(rbind(
    treated_countries,
    paste0("SYN_", treated_countries)
  )))
) |>
  select(iso3c = iso3c_ordered, order)

ordered_avg_df <- avg_df |>
  left_join(ordering_df, by = "iso3c") |>
  arrange(order) |>
  select(-iso3c, -order)

View(ordered_avg_df)

# Save the output
cat(
  kable(
    ordered_avg_df,
    format = "latex",
    booktabs = TRUE,
    caption = "The average of the predictors of per-capita GDP for franc-CFA zone countries"
  ),
  file = "output/tables/table_averages_CFAzone.tex"
)
