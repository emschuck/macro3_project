library(dplyr)

#Loading the data
df_synth <- readRDS("data/processed/scm_export_wide.rds")


# List of treated and synthetic countries
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

synthetic_countries <- c(
  "SYN_BEN", # Benin
  "SYN_BFA", # Burkina Faso
  "SYN_CIV", # Côte d'Ivoire
  "SYN_MLI", # Mali
  "SYN_NER", # Niger
  "SYN_SEN", # Senegal
  "SYN_TGO", # Togo
  "SYN_CMR", # Cameroon
  "SYN_CAF", # Central African Republic
  "SYN_TCD", # Chad
  "SYN_COG", # Republic of Congo
  "SYN_GAB", # Gabon
  "SYN_GNQ"  # Equatorial Guinea
)

avg_countries <- c(treated_countries, synthetic_countries)

#keep only the treated countries
df2 <- df_synth |>
  filter(
    iso3c %in% avg_countries,
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

#make the table for the actual data
avg_df<- df2 %>%
  group_by(country) %>%
  summarise(avg_agriculture=mean(agriculture), 
            avg_industry=mean(industry), 
            avg_government=mean(govt_share),
            avg_oda = mean(oda_share), 
            avg_fdi = mean(fdi), 
            avg_labour = mean(labour), 
            avg_institution = mean(polity2))
View(avg_df)
