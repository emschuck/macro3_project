# OLS regression table for CFA franc-zone countries

# =====================================================
# Load packages
# =====================================================

library(dplyr)
library(stargazer)
library(tidyr)
library(stringr)
library(lmtest)
library(huxtable)
library(knitr)

# =====================================================
# Load shared constants
# =====================================================

constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
  stop("Constants file not found: ", constants_file)
}

source(constants_file)

required_constants <- c(
  "treated_countries",
  "country_names"
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


# =====================================================
# Load data
# =====================================================

df <- readRDS("data/processed/processed_panel_imputed.rds")

if (!(gdp_var %in% names(df))) {
  stop("The selected GDP variable is not present in df: ", gdp_var)
}

df <- df |>
  mutate(
    gdp_selected = .data[[gdp_var]]
  )

# =====================================================
# Prepare OLS dataset
# =====================================================

ols_df <- df |>
  filter(
    iso3c %in% treated_countries,
    year >= 1980,
    year <= 2019
  ) |>
  select(
    iso3c,
    country,
    year,
    gdp_selected,
    agriculture,
    # industry,
    govt_share,
    invest_share,
    oda_share,
    fdi,
    labour,
    polity2
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

# =====================================================
# Estimate one OLS regression for each treated country
# =====================================================

ols_formula <- gdp_selected ~
  agriculture  + govt_share +
  invest_share + oda_share + fdi + labour

ols_models <- lapply(treated_countries, function(country_code) {
  country_data <- ols_df |>
    filter(iso3c == country_code)

  lm(ols_formula, data = country_data)
})

model_names <- vapply(
  treated_countries,
  get_country_name,
  character(1)
)

names(ols_models) <- model_names

# =====================================================
# Summarise the results in one table
# =====================================================

table_ols <- do.call(
  huxtable::huxreg,
  c(
    unname(ols_models),
    list(
      error_format = "({p.value})",
      error_pos = "below",
      statistics = c(
        N = "nobs",
        "R2 adj." = "adj.r.squared"
      )
    )
  )
)

# Rename model columns after creating the huxtable.
# The first column contains coefficient/statistic names, 
# so only rename model columns.
colnames(table_ols)[2:ncol(table_ols)] <- model_names

View(table_ols)

# =====================================================
# Save the output
# =====================================================

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  huxtable::to_latex(table_ols),
  file = "output/tables/table_ols_regression_gdp.tex"
)

print("Complete")