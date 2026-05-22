#Library
library(dplyr)
library(stargazer)
library(tidyr)
library(stringr)
library(lmtest)
library(huxtable)

#Load the data
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

#keep only the treated countries 
ols_df <- df |>
  filter(
    iso3c %in% treated_countries,
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


#OLS Regression for each country

#Benin - BEN
df_BEN <- ols_df %>%
  filter(iso3c=="BEN")

ols_BEN <- lm(gdp_pc_current ~ 
               agriculture + industry + govt_share +
               invest_share + oda_share + fdi + labour, 
             data = df_BEN)

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

#Summarize the results in one table
table_ols <- huxreg(ols_BEN, ols_BFA, ols_CAF, ols_CIV, ols_CMR, ols_COG, ols_GAB, ols_GNQ, ols_MLI, ols_NER, ols_SEN,        error_format = "({p.value})",
       error_pos = "below",
       statistics = c(N = "nobs", "R2 adj." = "adj.r.squared"))

View(table_ols)

#Save the output
cat(
  kable(
    table_ols,
    format = "latex",
    booktabs = TRUE,
    caption = "OLS estimates of real GDP per capita parameter of CFA Franc-zone countries"
  ),
  file = "output/tables/table_ols_regression_gdp.tex"
)