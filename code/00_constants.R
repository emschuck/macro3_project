# =====================================================
# Shared constants for replication project
# =====================================================

# GDP outcome used across scripts
gdp_var <- "wdi_gdp_pc_current"
gdp_selected_var <- "gdp_selected"
gdp_var_label <- "WDI GDP per capita (current USD)"

# gdp_var_file_stub <- gsub("[^A-Za-z0-9]+", "_", gdp_var)

# Main sample periods
pre_period <- 1980:2001
post_period <- 2002:2019
plot_period <- 1980:2019
treatment_year <- 2002

# Treated countries
treated_countries <- c(
  "BEN", "BFA", "CIV", "MLI", "NER", "SEN",
  "TGO", "CMR", "CAF", "TCD", "COG", "GAB", "GNQ"
)

waemu <- c("BEN", "BFA", "CIV", "MLI", "NER", "SEN", "TGO")
waemu_table1 <- c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO")
caemc <- c("CMR", "CAF", "TCD", "COG", "GNQ", "GAB")

# =====================================================
# Donor country lists
# =====================================================

# Original donor pool from the paper
donor_countries_original <- c(
  "BGD", # Bangladesh
  "BRB", # Barbados
  "BTN", # Bhutan
  "BOL", # Bolivia
  "BWA", # Botswana
  "CPV", # Cabo Verde
  "DMA", # Dominica
  "ECU", # Ecuador
  "SWZ", # Eswatini
  "GRD", # Grenada
  "LAO", # Lao PDR
  "LSO", # Lesotho
  "MUS", # Mauritius
  "MAR", # Morocco
  "NAM", # Namibia
  "OMN", # Oman
  "PAN", # Panama
  "KNA", # St. Kitts and Nevis
  "LCA", # St. Lucia
  "SYC"  # Seychelles
)

# Updated donor pool used for robustness checks
donor_countries_updated <- c(
  "BGD", # Bangladesh
  "BRB", # Barbados
  "BTN", # Bhutan
  "BOL", # Bolivia
  "BWA", # Botswana
  "CPV", # Cabo Verde
  "DMA", # Dominica
  "ECU", # Ecuador
  "SWZ", # Eswatini
  "GRD", # Grenada
  "LAO", # Lao PDR
  "LSO", # Lesotho
  "MUS", # Mauritius
  "MAR", # Morocco
  "NAM", # Namibia
  "KNA", # St. Kitts and Nevis
  "LCA", # St. Lucia
  "SYC", # Seychelles

  # Additional donor countries
  "TKM", # Turkmenistan
  "GUY", # Guyana
  "IRQ"  # Iraq
)

# Choose which donor pool the script should use
donor_pool_version <- "updated"

donor_countries <- switch(
  donor_pool_version,
  "original" = donor_countries_original,
  "updated" = donor_countries_updated,
  stop("donor_pool_version must be either 'original' or 'updated'.")
)

scm_countries <- unique(c(treated_countries, donor_countries))

# Non-CFA comparison countries
non_cfa_comparison_countries <- c(
  "AGO", "BDI", "COD", "ETH", "GMB", "GHA", "GIN", "KEN",
  "MDG", "MWI", "NGA", "STP", "SLE", "SDN", "TZA", "UGA",
  "ZMB", "ZWE"
)

# SCM predictors
scm_predictors <- c(
  "agriculture",
  "industry",
  "govt_share",
  "invest_share",
  "oda_share",
  "fdi",
  "labour",
  "polity2"
)

special_var <- "gdp_selected"
special_years <- c(1980, 1990, 1995, 2001)

# Country names
country_names <- tibble::tribble(
  ~iso3c, ~country,
  "BEN", "Benin",
  "BFA", "Burkina Faso",
  "CIV", "Côte d'Ivoire",
  "GNB", "Guinea-Bissau",
  "MLI", "Mali",
  "NER", "Niger",
  "SEN", "Senegal",
  "TGO", "Togo",

  "CMR", "Cameroon",
  "CAF", "Central African Republic",
  "TCD", "Chad",
  "COG", "Republic of Congo",
  "GNQ", "Equatorial Guinea",
  "GAB", "Gabon",

  "BGD", "Bangladesh",
  "BRB", "Barbados",
  "BTN", "Bhutan",
  "BOL", "Bolivia",
  "BWA", "Botswana",
  "CPV", "Cabo Verde",
  "DMA", "Dominica",
  "ECU", "Ecuador",
  "SWZ", "Eswatini",
  "GRD", "Grenada",
  "LAO", "Lao PDR",
  "LSO", "Lesotho",
  "MUS", "Mauritius",
  "MAR", "Morocco",
  "NAM", "Namibia",
  "OMN", "Oman",
  "PAN", "Panama",
  "KNA", "St. Kitts and Nevis",
  "LCA", "St. Lucia",
  "SYC", "Seychelles",

  "BRN", "Brunei Darussalam",
  "MAC", "Macao",
  "NPL", "Nepal",
  "LBY", "Libya",
  "TKM", "Turkmenistan",
  "ERI", "Eritrea",
  "GUY", "Guyana",
  "IRQ", "Iraq",
  "QAT", "Qatar",
  "DJI", "Djibouti",
  "SLV", "El Salvador",
  "JOR", "Jordan",
  "BZD", "Belize",

  "AGO", "Angola",
  "BDI", "Burundi",
  "COD", "Congo, Dem. Rep.",
  "ETH", "Ethiopia",
  "GMB", "Gambia, The",
  "GHA", "Ghana",
  "GIN", "Guinea",
  "KEN", "Kenya",
  "MDG", "Madagascar",
  "MWI", "Malawi",
  "NGA", "Nigeria",
  "STP", "Sao Tome and Principe",
  "SLE", "Sierra Leone",
  "SDN", "Sudan",
  "TZA", "Tanzania",
  "UGA", "Uganda",
  "ZMB", "Zambia",
  "ZWE", "Zimbabwe"
) |>
  dplyr::distinct(iso3c, .keep_all = TRUE)

# Helper functions
get_country_name <- function(code) {
  out <- country_names |>
    dplyr::filter(iso3c == code) |>
    dplyr::pull(country)

  if (length(out) == 0) code else out[1]
}

make_donor_labels <- function(donor_codes = donor_countries) {
  country_names |>
    dplyr::filter(iso3c %in% donor_codes) |>
    dplyr::mutate(
      order = match(iso3c, donor_codes),
      unit.names = iso3c,
      donor_label = country
    ) |>
    dplyr::arrange(order) |>
    dplyr::select(unit.names, donor_label)
}

make_treated_labels <- function(treated_codes = treated_countries) {
  country_names |>
    dplyr::filter(iso3c %in% treated_codes) |>
    dplyr::mutate(
      order = match(iso3c, treated_codes),
      treated_label = country
    ) |>
    dplyr::arrange(order) |>
    dplyr::select(iso3c, treated_label)
}

# =====================================================
# Extension
# =====================================================


# Periods for the trade-extension SCM analysis
extension_pre_period <- 1981:2001
extension_post_period <- 2002:2019
extension_plot_period <- 1981:2019

# Trade variables used as SCM outcomes
extension_trade_outcomes <- c(
  "XEU",
  "MEU",
  "trade_openness",
  "exports_gdp",
  "imports_gdp"
)

extension_trade_outcomes_chart <- c(
  "trade_openness",
  "exports_gdp",
  "imports_gdp"
)

# Labels for charts and tables
extension_trade_outcome_labels <- c(
  XEU = "Exports to EU (% of total exports)",
  MEU = "Imports from EU (% of total imports)",
  trade_openness = "Trade openness (% of GDP)",
  exports_gdp = "Exports of goods and services (% of GDP)",
  imports_gdp = "Imports of goods and services (% of GDP)"
)

# Predictors used in the extension SCM
extension_scm_predictors <- c(
  "agriculture",
  "govt_share",
  "invest_share",
  "oda_share",
  "fdi",
  "labour",
  "polity2"
)

# Main output folder for extension figures and tables.
extension_output_dir <- "output/extension_trade"

# Folder for processed extension RDS objects.
extension_processed_dir <- "data/processed/extension_trade"


extension_special_years <- c(1981, 1990, 1995, 2001)

