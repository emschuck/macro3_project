# 00_WDI_data_download.R


# Load the packages required for data download, cleaning, analysis, and output.
# Note: tidyverse already includes dplyr, ggplot2, tidyr, readr, etc., but the
# individual library calls are left explicit so readers can see core dependencies.
library(pwt10)
library(WDI)
library(tidyverse)
library(readxl)
library(countrycode)


safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}
#### ======================================================================###
countries <- c(
    "BEN", # Benin
    "BFA", # Burkina Faso
    "CIV", # Côte d'Ivoire
    "GNB", # Guinea-Bissau
    "MLI", # Mali
    "NER", # Niger
    "SEN", # Senegal
    "TGO",
    "CMR", # Cameroon
    "CAF", # Central African Republic
    "TCD", # Chad
    "COG", # Republic of Congo
    "GNQ", # Equatorial Guinea
    "GAB",
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
    "SYC", # Seychelles
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
    "ZWE", # Zimbabwe
    "BRN", #"Brunei",
    "MAC", #"Macao",
    "NPL", #"Nepal",
    "LBY", #"Libya",
    "TKM", #"Turkmenistan",
    "ERI", #"Eritrea",
    "GUY", #"Guyana",
    "IRQ", # Iraq
    "QAT", # Qatar
    "DJI", # Djibouti
    "SLV",  # El Salvador
    "JOR", # Jordan
    "BZD"  # Belize
)

indicators <- c(
  agriculture = "NV.AGR.TOTL.ZS", # NV.AGR.TOTL.CD for levels
  # Agriculture, forestry, and fishing, value added (% of GDP)
  agriculture_alt = "NV.AGR.TOTL.CD", # NV.AGR.TOTL.CD for levels
  # Agriculture, forestry, and fishing, value added (current US$)
#   ag_alt_2 = "NP.AGR.TOTL.CN", # Not available 
#   ag_alt_3 = "NA.GDP.AGR.CR", # Not available 
  industry = "NV.IND.TOTL.ZS", # NV.IND.MANF.ZS for manufacturing only
  # Industry including construction, value added (% of GDP)
  industry_alt = "NV.IND.TOTL.CD",
  # Industry including construction, value added (level)
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  # Foreign direct investment, net inflows (% of GDP)
  govt_share_alt = "NE.CON.GOVT.ZS",
  # General government final consumption expenditure (% of GDP)
  invest_share_alt = "NE.GDI.FTOT.ZS",
  # Gross fixed capital formation (% of GDP)=
#   fdi_alt = "BN.KLT.DINV.CD.DRS",  # Not available 
  # Foreign direct investment, net inflows (current USD)
  alt_inflation = "FP.CPI.TOTL.ZG",
  # Inflation, consumer prices (annual %)( not currently used)
  inflation = "NY.GDP.DEFL.KD.ZG",
  # Inflation, GDP deflator (annual %)
  oda = "DT.ODA.ODAT.GD.ZS",
  # Net official development assistance received (% of GNI)
  oda_alt = "DT.ODA.ODAT.CD",
  # Net ODA received (current USD)


#   # GDP VARIABLES
#   gdp_pc_wdi_alt = "NY.GDP.PCAP.CD",
#   # GDP per capita (current US$)
#   gdp_pc_wdi = "NY.GDP.PCAP.PP.KD", # GDP, PPP (constant 2021 international $)
#   gdp_wdi = "NY.GDP.MKTP.PP.KD", # GDP, PPP (constant 2021 international $)
#   gdp_pc_growth_wdi = "NY.GDP.PCAP.KD.ZG",
#   gdp_wdi_current = "NY.GDP.MKTP.CD",
#   # GDP growth rate, PPP (constant 2021 international $)
#   # real gdp pc
#   # gdp_pc_wdi = "NY.GDP.PCAP.PP.CD",
#   # GDP per capita, PPP (current international $)


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


# Download annual WDI data for all selected countries and years.
# may take some time
wdi <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2021
) |>
  select(-country) |>
  distinct(iso3c, year, .keep_all = TRUE)

saveRDS(wdi, "data/raw/wdi.rds")

print("Complete")