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
View(WDIsearch("agricultural gdp"))

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

# Foreign Direct Investment
#   BN.KLT.DINV.CD.ZS -- Foreign direct investment (% of GDP)

# Employment Rate
# SL.UEM.TOTL.NE.ZS
#       -- Unemployment, total (% of total labor force) (national estimate)
# SL.UEM.TOTL.ZS
#       -- Unemployment, total (% of total labor force) (modeled ILO estimate)

# Agriculture share of GDP

# Industry share of GDP
# Inflation

# Institution quality
# Political regime characteristics
