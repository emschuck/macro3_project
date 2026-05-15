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

# Create output directories
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)



####=========================================================================###
####=========================== 2. LOAD DATA ================================###
####=========================================================================###


### Penn World Tables ###

# Load and inspect Penn World Tables
data("pwt10.01")
View(pwt10.01)


### WDI ###

# Finding relevant indicators for each variable
View(WDIsearch("development assistance"))

# GDP per capita
#   NY.GDP.PCAP.PP.KD   -- GDP per capita, PPP (constant 2021 int. $)

# Gross Fixed Capital Formation (expenditure, used for investment metric)
#   NE.GDI.FTOT.ZS  -- Gross fixed capital formation (% of GDP)

# Govt expenditure
#   NE.CON.GOVT.ZS
#       -- General government final consumption expenditure (% of GDP)
#   NE.CON.GOVT.CD
#       -- General government final consumption expenditure (current US$)

# Official Development Assistance
#   DT.ODA.ALLD.CD
#       -- Net official development assistance and official aid received
#           (current US$)
#   DT.ODA.ALLD.GI.ZS
#   Net official development assistance received (% of gross capital formation)

# Foreign Direct Investment
#   BN.KLT.DINV.CD.ZS -- Foreign direct investment (% of GDP)

# Employment Rate
# SL.UEM.TOTL.NE.ZS
#       -- Unemployment, total (% of total labor force) (national estimate)
# SL.UEM.TOTL.ZS
#       -- Unemployment, total (% of total labor force) (modeled ILO estimate)

# Agriculture share of GDP ***
# NV.AGR.TOTL.ZS
#   Agriculture, forestry, and fishing, value added (% of GDP)

# Industry share of GDP ***
#   NV.IND.TOTL.ZS
#   Industry (including construction), value added (% of GDP)

# Inflation
#   FP.CPI.TOTL.ZG
#   Inflation, consumer prices (annual %)

# Institution quality


# Political regime characteristics



  # =====================================================
# Objective:
# - Load WDI data
# - Clean dataset
# - Validate inflation (pre vs post 2002)
# - Load and merge Polity data (institutions)
# =====================================================


# =====================================================
# 1. Load libraries
# =====================================================
library(WDI)
library(tidyverse)
library(readxl)
library(countrycode)


# =====================================================
# 2. Define countries and variables
# =====================================================

# CFA countries (treated countries)
countries <- c("BEN","BFA","CIV","MLI","NER","SEN","TGO",
               "CMR","CAF","TCD","COG","GAB","GNQ")

# WDI indicators (consistent with paper)
indicators <- c(
  gdp_pc = "NY.GDP.PCAP.KD",
  agriculture = "NV.AGR.TOTL.ZS",
  industry = "NV.IND.TOTL.ZS",
  investment = "NE.GDI.TOTL.ZS",
  government = "NE.CON.GOVT.ZS",
  fdi = "BX.KLT.DINV.WD.GD.ZS",
  inflation = "FP.CPI.TOTL.ZG",
  oda = "DT.ODA.ODAT.GD.ZS"
)


# =====================================================
# 3. Download WDI data
# =====================================================

df <- WDI(
  country = countries,
  indicator = indicators,
  start = 1980,
  end = 2019
)

# Quick overview
summary(df)


# =====================================================
# 4. Define regions and periods
# =====================================================

# Regions
waemu <- c("BEN","BFA","CIV","MLI","NER","SEN","TGO")
caemc <- c("CMR","CAF","TCD","COG","GAB","GNQ")

df <- df %>%
  mutate(
    region = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC"
    ),
    period = ifelse(year <= 2001, "pre", "post")
  )

# Check distribution
table(df$region)
table(df$period)


# =====================================================
# 5. Inflation validation (country level)
# =====================================================

inflation_table <- df %>%
  group_by(country, period) %>%
  summarise(inflation_avg = mean(inflation, na.rm = TRUE)) %>%
  pivot_wider(names_from = period, values_from = inflation_avg) %>%
  select(country, pre, post) %>%
  mutate(
    pre = round(pre, 2),
    post = round(post, 2)
  )

print(inflation_table)


# =====================================================
# 6. Inflation validation (regional averages)
# =====================================================

inflation_region <- df %>%
  group_by(region, period) %>%
  summarise(inflation_avg = mean(inflation, na.rm = TRUE)) %>%
  pivot_wider(names_from = period, values_from = inflation_avg) %>%
  select(region, pre, post) %>%
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

polity <- read_excel("data/raw/polity5/p5v2018.xlsx")

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
