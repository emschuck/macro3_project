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
library(stringr)
library(lmtest)


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

scm_countries <- c(treated_countries, donor_countries)

#keep only the treated countries and the donors

ols_df <- df |>
  filter(
    iso3c %in% scm_countries,
    year >= 1980,
    year <= 2019
  ) |>
  select(
    iso3c, country, year,
    gdp_pc_current,
    agriculture, industry, govt_share, invest_share,
    oda_share, fdi, labour, polity2
  ) |>
  distinct(iso3c, year, .keep_all = TRUE)

#Benin - BEN
df_BN <- ols_df %>%
  filter(iso3c=="BEN")

ols_BN <- lm(gdp_pc_current ~ 
               agriculture + industry + govt_share +
               invest_share + oda_share + fdi + labour, 
             data = df_BN)

#Burkina Faso - BFA
df_BFA <- ols_df %>%
  filter(iso3c=="BFA")

ols_BFA <- lm(gdp_pc_current ~ 
               agriculture + industry + govt_share +
               invest_share + oda_share + fdi + labour, 
             data = df_BFA)

#Ivory Coast - CIV
df_CIV <- ols_df %>%
  filter(iso3c=="CIV")

ols_CIV <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_CIV)

#Mali - MLI
df_MLI <- ols_df %>%
  filter(iso3c=="MLI")

ols_MLI <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_MLI)

#Niger - NER
df_NER <- ols_df %>%
  filter(iso3c=="NER")

ols_NER <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_NER)

#Senegal - SEN
df_SEN <- ols_df %>%
  filter(iso3c=="SEN")

ols_SEN <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_SEN)

#Togo - TGO
df_TGO <- ols_df %>%
  filter(iso3c=="TGO")

ols_TGO <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_TGO)

#Cameroon - CMR
df_CMR <- ols_df %>%
  filter(iso3c=="CMR")

ols_CMR <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_CMR)

#Central African Republic
df_CAF <- ols_df %>%
  filter(iso3c=="CAF")

ols_CAF <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_CAF)

#Chad - TCD
df_TCD <- ols_df %>%
  filter(iso3c=="TCD")

ols_TCD <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_TCD)

#Republic of Congo - COG
df_COG <- ols_df %>%
  filter(iso3c=="COG")

ols_COG <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_COG)

#Gabon - GAB
df_GAB <- ols_df %>%
  filter(iso3c=="GAB")

ols_GAB <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_GAB)

#Equatorial Guinea - GNQ
df_GNQ <- ols_df %>%
  filter(iso3c=="GNQ")

ols_GNQ <- lm(gdp_pc_current ~ 
                agriculture + industry + govt_share +
                invest_share + oda_share + fdi + labour, 
              data = df_GNQ)
