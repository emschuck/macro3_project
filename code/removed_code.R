
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




====================

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


#============================



## Smoothed graph 



### Testing smoothed graph to match paper
p_gdp_growth <- ggplot(df_growth, aes(year, growth, color = region_plot)) +
  geom_line(linewidth = 0.4, alpha = 0.35) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 0.9) +
  geom_vline(xintercept = 2002, linewidth = 1.1, color = "black") +
  scale_x_continuous(breaks = seq(1990, 2020, 2)) +
  scale_y_continuous(breaks = seq(-25, 25, 5)) +
  coord_cartesian(ylim = c(-25, 25)) +
  labs(
    title = "Evolution of GDP per capita growth",
    x = NULL,
    y = "Growth rate (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_gdp_growth)

ggsave(
  "output/figures/gdp_pc_growth_regions_smoothed.png",
  p_gdp_growth,
  width = 12,
  height = 4,
  dpi = 300
)





####### Growth table instead of growth factor


# =====================================================
# GDP per capita growth appendix tables
# Compare PWT GDP per capita (gdp_pc) and WDI GDP per capita (gdp_pc_wdi)
# =====================================================
safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Compute annual GDP per capita growth for both variables
df_gdp_growth <- df |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(
    growth_gdp_pc = 100 * (gdp_pc / lag(gdp_pc) - 1),
    growth_gdp_pc_wdi = 100 * (gdp_pc_wdi / lag(gdp_pc_wdi) - 1)
  ) |>
  ungroup()

# Helper function for GDP growth tables
make_gdp_growth_table <- function(data, country_order, average_label) {
  country_rows <- data |>
    filter(
      iso3c %in% country_order,
      year >= 1990,
      year <= 2021
    ) |>
    mutate(
      period = case_when(
        year >= 1990 & year <= 2001 ~ "1990-2001",
        year >= 2002 & year <= 2021 ~ "2002-2021",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(period)) |>
    group_by(iso3c, country, period) |>
    summarise(
      gdp_pc_growth = safe_mean(growth_gdp_pc),
      gdp_pc_wdi_growth = safe_mean(growth_gdp_pc_wdi),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = period,
      values_from = c(gdp_pc_growth, gdp_pc_wdi_growth)
    ) |>
    mutate(order = match(iso3c, country_order)) |>
    arrange(order) |>
    select(
      country,
      `gdp_pc 1990-2001` = `gdp_pc_growth_1990-2001`,
      `gdp_pc 2002-2021` = `gdp_pc_growth_2002-2021`,
      `gdp_pc_wdi 1990-2001` = `gdp_pc_wdi_growth_1990-2001`,
      `gdp_pc_wdi 2002-2021` = `gdp_pc_wdi_growth_2002-2021`
    )

  average_row <- country_rows |>
    summarise(
      country = average_label,
      `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
      `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
      `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
      `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
    )

  bind_rows(country_rows, average_row) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}

# Appendix 3, Panel A: WAEMU
gdp_growth_waemu <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = waemu_table1,
  average_label = "Average for the WAEMU area"
)

# Additional WAEMU average excluding Guinea-Bissau
gdp_growth_waemu_excl_gnb <- gdp_growth_waemu |>
  filter(!country %in% c(
    "Guinea-Bissau",
    "Average for the WAEMU area"
  )) |>
  summarise(
    country = "Average without Guinea-Bissau",
    `gdp_pc 1990-2001` = safe_mean(`gdp_pc 1990-2001`),
    `gdp_pc 2002-2021` = safe_mean(`gdp_pc 2002-2021`),
    `gdp_pc_wdi 1990-2001` = safe_mean(`gdp_pc_wdi 1990-2001`),
    `gdp_pc_wdi 2002-2021` = safe_mean(`gdp_pc_wdi 2002-2021`)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

gdp_growth_waemu <- bind_rows(gdp_growth_waemu, gdp_growth_waemu_excl_gnb)

# Appendix 3, Panel B: CAEMC
gdp_growth_caemc <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = caemc,
  average_label = "Average for the CAEMC zone"
)

# Appendix 4: Non-CFA comparison countries
gdp_growth_non_cfa <- make_gdp_growth_table(
  df_gdp_growth,
  country_order = non_cfa_comparison_countries,
  average_label = "Average"
)

# Print tables
print(gdp_growth_waemu)
print(gdp_growth_caemc)
print(gdp_growth_non_cfa)

# Save LaTeX outputs
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

cat(
  kable(
    gdp_growth_waemu,
    format = "latex",
    booktabs = TRUE,
    caption = "WAEMU countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelA_waemu_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_caemc,
    format = "latex",
    booktabs = TRUE,
    caption = "CAEMC countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix3_panelB_caemc_gdp_growth.tex"
)

cat(
  kable(
    gdp_growth_non_cfa,
    format = "latex",
    booktabs = TRUE,
    caption = "Non-CFA countries' mean annual GDP per capita growth: PWT and WDI GDP variables"
  ),
  file = "output/tables/appendix4_non_cfa_gdp_growth.tex"
)
