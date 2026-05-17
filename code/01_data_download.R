# International Macroeconomics Project

# Script 01     : Data download
# Authors       : Elizabeth Schuck, Callista Lodzinski, Mathilde Muller
# Date          : April 2026

# This script carries out the data acquisition, cleaning and preprocessing
#   1. Loading packages, verifying file structure
#   2. Read and inspect data from project files (Polity, ...), 
#            WDI API, Penn World Tables
#   3. Data cleaning
#   4. Preliminary data exploration
#   6. 


####=========================================================================###
####========================= 1. PROJECT SETUP ==============================###
####=========================================================================###

# Setup code copied from tutorial 1 R file (Author: Juan Pablo Ugarte Checura)

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

library(tidyverse)
library(readxl)
library(countrycode)

# Create output directories
dir.create("./output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("./output/figures",  recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)



####=========================================================================###
####=========================== 2. LOAD DATA ================================###
####=========================================================================###

### Penn World Tables ###
# Load and inspect Penn World Tables
data("pwt10.01")

pwt <- pwt10.01 |>
  select(
    isocode, country, year,
    rgdpo,      # Output-side real GDP at chained PPPs (mil. 2021 US$)
    pop,        # Population (millions)
    emp,        # Number of persons engaged (millions)
    csh_g,      # Share of government consumption at current PPPs
    csh_i,      # Share of gross capital formation at current PPPs
    hc          # Human capital index
  ) |>
  mutate(
    gdp_pc = rgdpo / pop,          # Real GDP per capita, output-side PPP
    labour = 100 * emp / pop,      # Employment as % of population
    govt_share = csh_g,            # Government consumption share
    invest_share = csh_i           # Investment / capital formation share
  )

# Match country code label
pwt <- pwt |>
  rename(iso3c = isocode)


### WDI ###
# WDI indicators used for variables not taken from PWT

indicators <- c(
  agriculture = "NV.AGR.TOTL.ZS",
  # Agriculture, forestry, and fishing, value added (% of GDP)
  industry = "NV.IND.TOTL.ZS",
  # Industry, including construction, value added (% of GDP)
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  # Foreign direct investment, net inflows (% of GDP)
  inflation = "FP.CPI.TOTL.ZG",
  # Inflation, consumer prices (annual %)
  oda = "DT.ODA.ODAT.GD.ZS"
  # Net official development assistance received (% of GNI)
)

# Download WDI data
wdi <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2019 # could pick more recent year? 
)

# Merge WDI with PWT 
df <- left_join(wdi, pwt, by = c("iso3c", "year")) |>
  select(-country.y) |>
  rename(country = country.x)

# Check for missing values after merge
summary(is.na(df$gdp_pc)) # FALSE 520 
summary(is.na(df$labour))

# Inspect
summary(df)
View(df)

# Define Regions
waemu <- c(
  "BEN", # Benin
  "BFA", # Burkina Faso
  "CIV", # Ivory Coast
  "MLI", # Mali
  "NER", # Niger
  "SEN", # Senegal
  "TGO"  # Togo
)

caemc <- c(
  "CMR", # Cameroon
  "CAF", # Central African Republic
  "COG", # Republic of Congo
  "GAB", # Gabon
  "GNQ", # Equatorial guinea
  "TCD" # Chad
)

donor_countries <- c(
  "BGD",  # Bangladesh
  "BRB",  # Barbados
  "BTN",  # Bhutan
  "BOL",  # Bolivia
  "BWA",  # Botswana
  "CPV",  # Cabo Verde
  "DMA",  # Dominica
  "ECU",  # Ecuador
  "SWZ",  # Eswatini
  "GRD",  # Grenada
  "LAO",  # Lao PDR
  "LSO",  # Lesotho
  "MUS",  # Mauritius
  "MAR",  # Morocco
  "NAM",  # Namibia
  "OMN",  # Oman
  "PAN",  # Panama
  "KNA",  # St. Kitts and Nevis
  "LCA",  # St. Lucia
  "SYC"   # Seychelles
)

# Table 2 non-CFA countries
non_cfa_comparison_countries <- c(
  "AGO",  # Angola
  "BDI",  # Burundi
  "COD",  # Congo, Dem. Rep.
  "ETH",  # Ethiopia
  "GMB",  # Gambia, The
  "GHA",  # Ghana
  "GIN",  # Guinea
  "KEN",  # Kenya
  "MDG",  # Madagascar
  "MWI",  # Malawi
  "NGA",  # Nigeria
  "STP",  # Sao Tome and Principe
  "SLE",  # Sierra Leone
  "SDN",  # Sudan
  "TZA",  # Tanzania
  "UGA",  # Uganda
  "ZMB",  # Zambia
  "ZWE"   # Zimbabwe
)


df <- df %>%
  mutate(
    region = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      iso3c %in% caemc ~ "donor_countries",
      iso3c %in% caemc ~ "non_cfa_comparison_countries",
    ),
    period = ifelse(year <= 2001, "pre", "post")
  )



# Check distribution
table(df$region)
table(df$period)


# =====================================================
# 5. Inflation validation (country level)
# =====================================================

inflation_table <- df |>
  group_by(country, period) |> # country.x 
  summarise(inflation_avg = mean(inflation, na.rm = TRUE)) |>
  pivot_wider(names_from = period, values_from = inflation_avg) |>
  select(country, pre, post) |>
  mutate(
    pre = round(pre, 2),
    post = round(post, 2)
  )

print(inflation_table)


# =====================================================
# 6. Inflation validation (regional averages)
# =====================================================

inflation_region <- df |>
  group_by(region, period) |>
  summarise(inflation_avg = mean(inflation, na.rm = TRUE)) |>
  pivot_wider(names_from = period, values_from = inflation_avg) |>
  select(region, pre, post) |>
  mutate(
    pre = round(pre, 2),
    post = round(post, 2)
  )

print(inflation_region)


# =====================================================
# 7. Additional data checks
# =====================================================

# Missing values per variable
df %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Check duplicates
df %>%
  count(iso3c, year) %>%
  filter(n > 1)

# Check extreme inflation values
df %>%
  summarise(
    min_inflation = min(inflation, na.rm = TRUE),
    max_inflation = max(inflation, na.rm = TRUE)
  )

#Temporary conclusion : Some values are quite far from the paper's one, not so sure about the explanation

# =====================================================
# 8. GDP growth graph
# =====================================================

df<-df %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(growth = (gdp_pc / lag(gdp_pc) - 1) * 100)

library(ggplot2)

df_growth <- df %>% group_by(region,year) %>% summarize(growth=mean(growth,na.rm=TRUE))

ggplot(df_growth, aes(x = year, y = growth, color = region)) +
   geom_line(size = 1) + geom_vline(xintercept=2002,linetype ="dashed",color="black")+
    labs(
       title = "Evolution of GDP per capita growth",
         x = "Year",
         y = "Growth rate (%)"
       ) 
  +   theme_minimal()


# =====================================================
# 8. Load Polity data (institutions)
# =====================================================

# Polity score:
# -10 = full autocracy
# +10 = full democracy

polity <- read_excel("../data/raw/polity5/p5v2018.xlsx")

polity_clean <- polity %>%
  select(country, year, polity2) %>%
  mutate(
    iso3c = countrycode(country,
                        origin = "country.name",
                        destination = "iso3c")
  ) %>%
  filter(iso3c %in% countries,
         year >= 1980, year <= 2019)


# =====================================================
# 9. Merge Polity data with WDI dataset
# =====================================================

df <- df %>%
  left_join(polity_clean, by = c("iso3c", "year"))

# Check polity values
summary(df$polity2)
