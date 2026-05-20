# Macro 3 Modelling Diary
## Callista Lodzinski (CL), Mathilde Muller (MM), and Elizabeth Schuck (ES)

[comment]: <> (Use CTRL+SHIFT+V to view rendered markdown in VS Code)

* 23-04-2026 : Data sourced from Polity5. Most recent file has data only from 2018. May be worth exploring other sources for similar data, for comparison points
* 16-05-2026 : Initial investigation shows some differences in inflation rate from those shown in the table -- needs investigation into reasons
* 17-05-2026 : Initially using NY.GDP.PCAP.KD = constant-price GDP per capita, but appendix lists Output-side real GDP at current $. Swapping to using NY.GDP.PCAP.CD  -- GDP per capita (current US$).
* 17-05-2026 : Penn world tables have more reliable data for output-side gdp, and include population variables -- testing using this instead of WDI variables.
* 17-05-2026 : Using PWT for employment, government consumption share, and investment share instead of WDI 
* 17-05-2026 : Trying to match inflation is giving different numbers. Likely partially from WDI data revisions, but also missing some values for Guinea, SL, and Zimbabwe. Testing GDP deflator inflation values (NY.GDP.DEFL.87.ZG, NY.GDP.DEFL.KD.ZG) matches for table 1B, but still not accurate for table 1a.
* 17-05-2026 : Testing for table 1A if it is 1980-2021 values (as written in the report) not 1980-2001, but this does not seem to be the case -- much lower values
* 17-05-2026 : GDP growth rate giving negative values and different scale. Investigating WDI variables options and PWT variable options, no improvement.
* 18-05-2026 : Full test suite of all GDP variables available, including manual calculation from PWT population and WDI values -- none match. 
* 19-05-2026 : The GDP table in the report has a mistake - both columns have the same values. Priortitising getting values close to the chart, and working from there
* 20-05-2026 : Data availability problems: Laos and St Lucia missing agriculture and industry data. Duplications for CPV and SWZ. For cemac countries, CAR and GNQ missing ag and industry data. Starting to solve data problems for donor countries, so complete analysis can be completed for WAEMU countries.
* 20-05-2026 : Ag. data for Laos key block: missing WDI data until 1989, but high weights on Laos for many countries in method. Found a paper with estimates of Laos ag. GDP back to 1970s -- seems to support a backwards linear extrapolation of existing data
* 20-05-2026 : Barbados, Namibia, Oman, Seychelles, St kitts and Nevis, all missing some ODA data. Forwards and backwards extrapolation reasonable in some cases, but some (BRB, OMN, KNA) missing substantial share. 
* 20-05-2026 : 

