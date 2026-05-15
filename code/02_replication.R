# Prepare the data

countries_sc <- c("BDG","BRB","BTN", "BOL","BWA","CPC", "DMA", "ECU", "SWZ", "GRD", "LAO",
                  "LSO", "MUS", "MAR", "NAM", "OMN", "PAN", "KNA", "LCA", "SYC")

df_sc <- WDI(
  country = countries_sc,
  indicator = indicators,
  start = 1980,
  end = 2019
)