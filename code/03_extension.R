library(WDI)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Trade indicators from WDI : 
trade_indicators <- c(
  trade_openness = "NE.TRD.GNFS.ZS",
  exports_gdp    = "NE.EXP.GNFS.ZS",
  imports_gdp    = "NE.IMP.GNFS.ZS")

waemu <- c("BEN", "BFA", "CIV", "MLI", "NER", "SEN", "TGO")
caemc <- c("CMR", "CAF", "TCD", "COG", "GNQ", "GAB")
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

countries <- unique(c(waemu, caemc, non_cfa_comparison_countries))

# Downloading trade data : 

trade_raw <- WDI(
  country = countries,
  indicator = c(
    trade_openness = "NE.TRD.GNFS.ZS",
    exports_gdp    = "NE.EXP.GNFS.ZS",
    imports_gdp    = "NE.IMP.GNFS.ZS"
  ),
  start = 1980,
  end   = 2019
) |>
  select(-country) |>
  distinct(iso3c, year, .keep_all = TRUE)

#Checking the missing values : 
missing_check <- trade_raw |>
  group_by(iso3c) |>
  summarise(
    n_years         = n(),
    miss_trade      = sum(is.na(trade_openness)),
    miss_exports    = sum(is.na(exports_gdp)),
    miss_imports    = sum(is.na(imports_gdp)),
    # Pre-treatment period specifically (what matters for SCM)
    miss_trade_pre  = sum(is.na(trade_openness[year <= 2001])),
    miss_exports_pre= sum(is.na(exports_gdp[year <= 2001])),
    miss_imports_pre= sum(is.na(imports_gdp[year <= 2001])),
    .groups = "drop"
  ) |>
  mutate(
    group = case_when(
      iso3c %in% waemu  ~ "WAEMU",
      iso3c %in% caemc  ~ "CAEMC",
      TRUE              ~ "Donor"
    )
  ) |>
  arrange(group, desc(miss_trade))

print(missing_check, n = Inf)

#Creation of a data frame in order to do some plots : 
trade_grouped <- trade_raw |>
  filter(iso3c %in% c(waemu, caemc,non_cfa_comparison_countries )) |>
  mutate(
    group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% non_cfa_comparison_countries ~ "Non CFA Countries"
    )
  ) |>
  group_by(group, year) |>
  summarise(
    trade_openness = mean(trade_openness, na.rm = TRUE),
    imports_gdp    = mean(imports_gdp,    na.rm = TRUE),
    exports_gdp    = mean(exports_gdp,    na.rm = TRUE),
    .groups = "drop"
  )
#Graphics on trade openness, imports, exports for each group of countries : 
trade_grouped |>
  ggplot(aes(x = year, y = trade_openness, color = group)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linetype = "dashed") +
  labs(
    title = "Trade openness (% of GDP) — group averages",
    subtitle = "Dashed line = 2002",
    x = NULL, y = "Trade openness (% of GDP)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

trade_grouped |>
  ggplot(aes(x = year, y = imports_gdp, color = group)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linetype = "dashed") +
  labs(
    title = "Imports (% of GDP) — group averages",
    subtitle = "Dashed line = 2002",
    x = NULL, y = "Imports (% of GDP)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

trade_grouped |>
  ggplot(aes(x = year, y = exports_gdp, color = group)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2002, linetype = "dashed") +
  labs(
    title = "Exports (% of GDP) — group averages",
    subtitle = "Dashed line = 2002",
    x = NULL, y = "Exports (% of GDP)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")



# Load processed data
df <- readRDS("data/processed/processed_panel_imputed.rds")

# Country lists for treated and control

treated_countries <- c(
  "BEN", # Benin
  "BFA", # Burkina Faso
  "CIV", # Côte d'Ivoire
  "MLI", # Mali
  "NER", # Niger
  "SEN", # Senegal
  "TGO", # Togo
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "TCD", # Chad
  "COG", # Republic of Congo
  "GAB", # Gabon
  "GNQ"  # Equatorial Guinea
)

donor_countries <- c(
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



# Country names for charts
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
)

# Function to get country name form list
get_country_name <- function(code) {
  country_names |>
    filter(iso3c == code) |>
    pull(country)
}



#Clean the trade variables

#Importing data for treated and donor countries : 

scm_countries <- c(treated_countries, donor_countries)

trade_scm <- trade_raw <- WDI(
  country = countries_scm,
  indicator = c(
    trade_openness = "NE.TRD.GNFS.ZS",
    exports_gdp    = "NE.EXP.GNFS.ZS",
    imports_gdp    = "NE.IMP.GNFS.ZS"
  ),
  start = 1980,
  end   = 2019
) |>
  select(-country) |>
  distinct(iso3c, year, .keep_all = TRUE)


missing_check_scm <- trade_scm |>
  group_by(iso3c) |>
  summarise(
    n_years         = n(),
    miss_trade      = sum(is.na(trade_openness)),
    miss_exports    = sum(is.na(exports_gdp)),
    miss_imports    = sum(is.na(imports_gdp)),
    # Pre-treatment period specifically (what matters for SCM)
    miss_trade_pre  = sum(is.na(trade_openness[year <= 2001])),
    miss_exports_pre= sum(is.na(exports_gdp[year <= 2001])),
    miss_imports_pre= sum(is.na(imports_gdp[year <= 2001])),
    .groups = "drop"
  ) |>
  mutate(
    group = case_when(
      iso3c %in% waemu  ~ "WAEMU",
      iso3c %in% caemc  ~ "CAEMC",
      TRUE              ~ "Donor"
    )
  ) |>
  arrange(group, desc(miss_trade))

print(missing_check, n = Inf)



missing_check_scm |>
  filter(group %in% c("WAEMU", "CAEMC")) |>
  select(iso3c, group, miss_trade_pre, miss_exports_pre, miss_imports_pre) |>
  print()

missing_check_scm |>
  filter(group == "Donor") |>
  select(iso3c, miss_trade_pre, miss_exports_pre, miss_imports_pre) |>
  print()


#Results : problem --> GNQ (Equatorial Guinea)
# in donor countries --> 8/9 countries with missing data, like BRB (Barbados)  




