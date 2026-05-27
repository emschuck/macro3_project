# 00_WDI_data_download.R

#### ========================================================================###
#### ======================== 1. PROJECT SETUP ==============================###
#### ========================================================================###

# Load the packages required for data download, cleaning, analysis, and output.
library(pwt10)
library(WDI)
library(tidyverse)
library(readxl)
library(countrycode)

# Create raw-data folder before saving WDI output.
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Load shared country lists and labels.
constants_file <- "code/00_constants.R"

if (!file.exists(constants_file)) {
  stop("Constants file not found: ", constants_file)
}

source(constants_file)

# Check that the constants file has the objects needed by this script.
required_constants <- c(
  "waemu_table1",
  "caemc",
  "donor_countries_original",
  "donor_countries_updated",
  "non_cfa_comparison_countries"
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

# Helper function used in some diagnostic or summary code.
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

#### ========================================================================###
#### ======================== 2. COUNTRY SAMPLE =============================###
#### ========================================================================###

# Build the WDI download sample from the shared constants file.
# This uses both donor lists so that the saved WDI file contains all countries
# needed for either the original donor-pool specification or the updated one.
countries <- unique(c(
  waemu_table1,
  caemc,
  donor_countries_original,
  donor_countries_updated,
  non_cfa_comparison_countries
))

# Basic country-code checks.
if (any(is.na(countries)) || any(countries == "")) {
  stop("The constructed country list contains missing or blank ISO3 codes.")
}

duplicate_countries <- countries[duplicated(countries)]

if (length(duplicate_countries) > 0) {
  warning(
    "Duplicate country codes were found before applying unique(): ",
    paste(unique(duplicate_countries), collapse = ", ")
  )
}

cat("Number of countries requested from WDI:", length(countries), "\n")
cat("Countries requested from WDI:\n")
print(sort(countries))

#### ========================================================================###
#### ======================== 3. WDI INDICATORS =============================###
#### ========================================================================###

# WDI indicator list.
# These are downloaded once and then used by the data-processing script.
indicators <- c(
  # Sectoral structure
  agriculture = "NV.AGR.TOTL.ZS",
  # Agriculture, forestry, and fishing, value added (% of GDP)

  agriculture_alt = "NV.AGR.TOTL.CD",
  # Agriculture, forestry, and fishing, value added (current US$)

  industry = "NV.IND.TOTL.ZS",
  # Industry including construction, value added (% of GDP)

  industry_alt = "NV.IND.TOTL.CD",
  # Industry including construction, value added (current US$)

  # Capital flows and expenditure shares
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  # Foreign direct investment, net inflows (% of GDP)

  govt_share_alt = "NE.CON.GOVT.ZS",
  # General government final consumption expenditure (% of GDP)

  invest_share_alt = "NE.GDI.FTOT.ZS",
  # Gross fixed capital formation (% of GDP)

  # Inflation
  alt_inflation = "FP.CPI.TOTL.ZG",
  # Inflation, consumer prices (annual %), not currently used in main tables

  inflation = "NY.GDP.DEFL.KD.ZG",
  # Inflation, GDP deflator (annual %)

  # Official development assistance
  oda = "DT.ODA.ODAT.GD.ZS",
  # Net official development assistance received (% of GNI)

  oda_alt = "DT.ODA.ODAT.CD",
  # Net ODA received (current US$)

  # GDP per capita variables
  wdi_gdp_pc_ppp_constant = "NY.GDP.PCAP.PP.KD",
  # GDP per capita, PPP, constant international $

  wdi_gdp_pc_ppp_current = "NY.GDP.PCAP.PP.CD",
  # GDP per capita, PPP, current international $

  wdi_gdp_pc_constant = "NY.GDP.PCAP.KD",
  # GDP per capita, constant 2015 US$

  wdi_gdp_pc_current = "NY.GDP.PCAP.CD",
  # GDP per capita, current US$

  wdi_gdp_pc_growth = "NY.GDP.PCAP.KD.ZG",
  # GDP per capita growth, annual %

  # Aggregate GDP variables
  wdi_gdp_ppp_constant = "NY.GDP.MKTP.PP.KD",
  # GDP, PPP, constant international $

  wdi_gdp_ppp_current = "NY.GDP.MKTP.PP.CD",
  # GDP, PPP, current international $

  wdi_gdp_constant = "NY.GDP.MKTP.KD",
  # GDP, constant 2015 US$

  wdi_gdp_current = "NY.GDP.MKTP.CD",
  # GDP, current US$

  # Trade-extension variables from WDI
  trade_openness = "NE.TRD.GNFS.ZS",
  exports_gdp = "NE.EXP.GNFS.ZS",
  imports_gdp = "NE.IMP.GNFS.ZS"
)

#### ========================================================================###
#### ======================== 4. DOWNLOAD WDI DATA ==========================###
#### ========================================================================###

# Download annual WDI data for all selected countries and years.
# This can take some time because it requests many indicators for many countries.
wdi <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2021
) |>
  select(-country) |>
  distinct(iso3c, year, .keep_all = TRUE)

# Check whether all requested countries appear in the downloaded WDI data.
missing_wdi_countries <- setdiff(countries, unique(wdi$iso3c))

if (length(missing_wdi_countries) > 0) {
  warning(
    "These requested countries did not appear in the WDI download: ",
    paste(missing_wdi_countries, collapse = ", ")
  )
}

cat("Number of countries returned by WDI:", length(unique(wdi$iso3c)), "\n")
cat("Number of country-year rows returned by WDI:", nrow(wdi), "\n")

#### ========================================================================###
#### ======================== 5. SAVE RAW WDI DATA ==========================###
#### ========================================================================###

saveRDS(wdi, "data/raw/wdi.rds")

print("WDI Data Download Complete")