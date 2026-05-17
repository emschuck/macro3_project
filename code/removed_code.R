
# GDP per capita
#   NY.GDP.PCAP.PP.KD   -- GDP per capita, PPP (constant 2021 int. $)
#   NY.GDP.MKTP.CD -- GDP (current US$)
#   NY.GDP.PCAP.CD  -- GDP per capita (current US$)

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






### CHECKING TABLE 1 difference
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}
table1_waemu_1980_2021_check <- df |>
  filter(
    iso3c %in% waemu_table1,
    year >= 1980,
    year <= 2021
  ) |>
  group_by(iso3c, country) |>
  summarise(
    `1980-2021` = safe_mean(inflation),
    `2002-2021` = safe_mean(inflation[year >= 2002 & year <= 2021]),
    .groups = "drop"
  ) |>
  mutate(order = match(iso3c, waemu_table1)) |>
  arrange(order) |>
  select(country, `1980-2021`, `2002-2021`) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

waemu_avg <- table1_waemu_1980_2021_check |>
  summarise(
    country = "Average inflation for the WAEMU zone",
    `1980-2021` = safe_mean(`1980-2021`),
    `2002-2021` = safe_mean(`2002-2021`)
  )

waemu_avg_excl_gnb <- table1_waemu_1980_2021_check |>
  filter(country != "Guinea-Bissau") |>
  summarise(
    country = "Average inflation without Guinea-Bissau",
    `1980-2021` = safe_mean(`1980-2021`),
    `2002-2021` = safe_mean(`2002-2021`)
  )

table1_waemu_1980_2021_check <- bind_rows(
  table1_waemu_1980_2021_check,
  waemu_avg,
  waemu_avg_excl_gnb
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(table1_waemu_1980_2021_check)
View(table1_waemu_1980_2021_check)
