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
  country = scm_countries,
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


# Changing of trade variables --> Now focusing on the share of exports goi,g to EU (IMF data) 

library(readr)
library(ggplot2)
library(fixest)

#Data set with treated and donor countries : 

dot_raw <- read_csv("data/raw/imf_trade_dots.csv")


dot_clean <- dot_raw |>
  rename(
    iso3c     = COUNTRY.ID,
    indicator = INDICATOR.ID,
    partner   = COUNTERPART_COUNTRY.ID,
    year      = TIME_PERIOD,
    value     = OBS_VALUE
  ) |>
  select(iso3c, indicator, partner, year, value) |>
  mutate(year  = as.integer(year),
         value = as.numeric(value)) |>
  filter(iso3c %in% scm_countries)

#Construction of a clean data set where we compute the the share of exports and imports relying on EU 
dot_shares <- dot_clean |>
  pivot_wider(names_from = c(indicator, partner), values_from = value) |>
  rename(
    exports_world = XG_FOB_USD_G001,
    imports_world = MG_CIF_USD_G001,
    exports_eu    = XG_FOB_USD_G163,
    imports_eu    = MG_CIF_USD_G163
  ) |>
  mutate(
    XEU = (exports_eu / exports_world) * 100,  # export share to EU (%)
    MEU = (imports_eu / imports_world) * 100   # import share from EU (%)
  )

View(dot_shares)

#The data frame is built such that when a value is missing for a year, the year does not even appear. Therefore we need to verify all years appear : 
dot_balanced <- dot_shares |>
  complete(iso3c, year = 1980:2019)

#Function to check how many values are missing : 
missing_check_dot <- dot_balanced |>
  group_by(iso3c) |>
  summarise(
    n_years      = n(),
    miss_XEU     = sum(is.na(XEU)),
    miss_MEU     = sum(is.na(MEU)),
    miss_XEU_pre = sum(is.na(XEU[year <= 2001])),
    miss_MEU_pre = sum(is.na(MEU[year <= 2001])),
    .groups = "drop"
  ) |>
  mutate(
    group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      TRUE             ~ "Donor"
    )
  ) |>
  arrange(group, desc(miss_XEU_pre))

print(missing_check_dot, n = Inf)

#Creation of a data frame to generate plots : 
dot_grouped <- dot_balanced |>
  mutate(
    group = case_when(
      iso3c %in% waemu ~ "WAEMU",
      iso3c %in% caemc ~ "CAEMC",
      TRUE             ~ "Donor"
    )
  ) |>
  group_by(group, year) |>
  summarise(
    XEU_mean = mean(XEU, na.rm = TRUE),
    MEU_mean = mean(MEU, na.rm = TRUE),
    .groups = "drop"
  )

# Graph on the share of imports coming from Euro area : 

ggplot(dot_grouped, aes(x = year, y = MEU_mean, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2003, y = max(dot_grouped$MEU_mean, na.rm=TRUE),
           label = "CFA reform", hjust = 0, size = 3.5, color = "gray40") +
  labs(
    title    = "Import share to EU by group",
    subtitle = "% of total imports, annual average within group",
    x = NULL, y = "% of total imports",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

#Graph on the evolution of the share of exports going to Euro area : 
ggplot(dot_grouped, aes(x = year, y = XEU_mean, color = group)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2003, y = max(dot_grouped$XEU_mean, na.rm=TRUE),
           label = "CFA reform", hjust = 0, size = 3.5, color = "gray40") +
  labs(
    title    = "Export share to EU by group",
    subtitle = "% of total exports, annual average within group",
    x = NULL, y = "% of total exports",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

#Evolution of the share of exports going to Euro area by groups (WAEMU/CAEMC/Donor countries) :
ggplot(dot_balanced |> mutate(group = case_when(
  iso3c %in% waemu ~ "WAEMU",
  iso3c %in% caemc ~ "CAEMC",
  TRUE             ~ "Donor")),
  aes(x = year, y = XEU, group = iso3c, color = group)) +
  geom_line(alpha = 0.4, linewidth = 0.6) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "gray40") +
  facet_wrap(~group) +
  labs(title = "Export share to EU — individual countries",
       x = NULL, y = "% of total exports", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

#Regressions (DID and TWFE) : 

# Treated = WAEMU + CAEMC (peg switched to euro in 2002)

dot_balanced_reg <- dot_balanced |>
  mutate(
    treated   = iso3c %in% c(waemu, caemc),
    post      = year >= 2002,
    treated_post = treated * post
  )

did_simple <- lm(XEU ~ treated + post + treated_post, data = dot_balanced_reg)
summary(did_simple)

#Note on DID : Do not control for country specific or time-invariant trends and the R²=0.27 is low (lots of varaition not explained by the model) 

#Two-way fixed effects : 

twfe <- feols(XEU ~ treated_post | iso3c + year, 
              data = dot_balanced_reg, 
              cluster = ~iso3c)
summary(twfe)

#After checking for pre trends, the parallel pre trend assumption is not veriefied, especially for 1980-1990 period. 
#Dropping these 10 years of obsevations and running TWFE : 

# Restrict to post-1990
twfe_1990 <- feols(XEU ~ i(event_time, treated, ref = -1) | iso3c + year,
                         data = dot_balanced_reg |>
                           filter(year >= 1990,
                           ),
                         cluster = ~iso3c)

iplot(twfe_1990,
      main = "Event study: 1990-2019",
      xlab = "Years relative to treatment",
      ylab = "Estimated effect (pp)")


#TWFE results : 
twfe_final <- feols(XEU ~ treated_post | iso3c + year,
                    data = dot_balanced_reg |> filter(year >= 1990),
                    cluster = ~iso3c)

summary(twfe_final)

#testing the 2 groups separately: 

dot_balanced_reg <- dot_balanced_reg |>
  mutate(
    waemu_post = iso3c %in% waemu & year >= 2002,
    caemc_post = iso3c %in% caemc & year >= 2002
  )

twfe_split_1990 <- feols(XEU ~ waemu_post + caemc_post | iso3c + year,
                         data = dot_balanced_reg |> filter(year >= 1990),
                         cluster = ~iso3c)

summary(twfe_split_1990)

#Clean table of TWFE coefficeints per country : 
library(modelsummary)
treated_countries <- c(waemu, caemc)

# Create a list of models, one per treated country
models_by_country <- lapply(treated_countries, function(ctry) {
  df <- dot_balanced_reg |>
    filter(year >= 1990) |>
    filter(iso3c %in% donor_countries | iso3c == ctry) |>
    mutate(treated_post = iso3c == ctry & year >= 2002)
  
  feols(XEU ~ treated_post | iso3c + year,
        data = df,
        cluster = ~iso3c)
})

names(models_by_country) <- treated_countries
modelsummary(
  models_by_country,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c("treated_postTRUE" = "Treated x Post"),
  gof_map  = c("nobs", "adj.r.squared"),
  title    = "TWFE estimates by treated country — EU export share (1990–2019)",
  notes    = "Country and year fixed effects included. Standard errors clustered at country level. Donor group serves as control for each specification."
)


#Back to cleaning the data to merge later :
summary(dot_balanced[, c("XEU", "MEU")])


dot_balanced |>
  group_by(iso3c) |>
  summarise(
    miss_XEU = sum(is.na(XEU)),
    miss_MEU = sum(is.na(MEU)),
    pct_miss_XEU = round(mean(is.na(XEU)) * 100, 1),
    pct_miss_MEU = round(mean(is.na(MEU)) * 100, 1)
  ) |>
  filter(miss_XEU > 0 | miss_MEU > 0) |>
  arrange(desc(pct_miss_XEU))

bad_coverage <- dot_balanced |>
  filter(year <= 2001) |>
  group_by(iso3c) |>
  summarise(miss_pre = sum(is.na(XEU))) |>
  filter(miss_pre > 3) |>
  pull(iso3c)

library(zoo)

dot_clean_final <- dot_balanced |>
  group_by(iso3c) |>
  arrange(year) |>
  mutate(
    XEU = na.approx(XEU, na.rm = FALSE, maxgap = 2),
    MEU = na.approx(MEU, na.rm = FALSE, maxgap = 2)
  ) |>
  ungroup()

dot_final <- dot_clean_final |>
  filter(!iso3c %in% bad_coverage) |>
  select(iso3c, year, XEU, MEU) |>
  arrange(iso3c, year)

#Saving the clean data set : 
saveRDS(dot_final, "data/processed/trade_dots_clean.rds")

#Merging ? 

