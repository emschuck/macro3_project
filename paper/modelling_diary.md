# Macro 3 Modelling Diary
## Callista Lodzinski (CL), Mathilde Muller (MM), and Elizabeth Schuck (ES)

[comment]: <> (Use CTRL+SHIFT+V to view rendered markdown in VS Code)

* 23-04-2026 [ES]: Data sourced from Polity5. Most recent file has data only from 2018. May be worth exploring other sources for similar data, for comparison points
* 16-05-2026 [ES]: Initial investigation shows some differences in inflation rate from those shown in the table -- needs investigation into reasons
* 17-05-2026 [ES]: Initially using NY.GDP.PCAP.KD = constant-price GDP per capita, but appendix lists Output-side real GDP at current $. Swapping to using NY.GDP.PCAP.CD  -- GDP per capita (current US$).
* 17-05-2026 [ES]: Penn world tables have more reliable data for output-side gdp, and include population variables -- testing using this instead of WDI variables.
* 17-05-2026 [ES]: Using PWT for employment, government consumption share, and investment share instead of WDI 
* 17-05-2026 [ES]: Trying to match inflation is giving different numbers. Likely partially from WDI data revisions, but also missing some values for Guinea, SL, and Zimbabwe. Testing GDP deflator inflation values (NY.GDP.DEFL.87.ZG, NY.GDP.DEFL.KD.ZG) matches for table 1B, but still not accurate for table 1a.
* 17-05-2026 [ES]: Testing for table 1A if it is 1980-2021 values (as written in the report) not 1980-2001, but this does not seem to be the case -- much lower values
* 17-05-2026 [ES]: GDP growth rate giving negative values and different scale. Investigating WDI variables options and PWT variable options, no improvement.
* 18-05-2026 [ES]: Full test suite of all GDP variables available, including manual calculation from PWT population and WDI values -- none match. 
* 19-05-2026 [ES]: The GDP table in the report has a mistake - both columns have the same values. Priortitising getting values close to the chart, and working from there
* 20-05-2026 [ES]: Data availability problems: Laos and St Lucia missing agriculture and industry data. Duplications for CPV and SWZ. For cemac countries, CAR and GNQ missing ag and industry data. Starting to solve data problems for donor countries, so complete analysis can be completed for WAEMU countries.
* 20-05-2026 [ES]: Ag. data for Laos key block: missing WDI data until 1989, but high weights on Laos for many countries in method. Found a paper with estimates of Laos ag. GDP back to 1970s -- seems to support a backwards linear extrapolation of existing data
* 20-05-2026 [ES]: Barbados, Namibia, Oman, Seychelles, St kitts and Nevis, all missing some ODA data. Forwards and backwards extrapolation reasonable in some cases, but some (BRB, OMN, KNA) missing substantial share. 
* 20-05-2026 [ES]: Agricultural data taken from FAOSTAT to fill misisng gaps. Fits well with existing data - choosing not to completely replace dataset to try to keep some alignment with the paper
* 20-05-2026 [ES]: Set up extrapolation/interpolation methods for missing data. Filling zeros for Polity5 data, (treats countries with missing data as neutral). Filling zeros for FDI, as missing values are all at the start of history, and first few existing values are around zero. Filling with value for closest year in the case of large missing periods. May not be accurate, but unlikely to warp results in the way linear extrapolation might. For variables with only a few missing values, linear interpolation or extrapolation (forwards/backwards), or for larger missing value periods with supporting info (eg a paper that shows the trend is rougly linear over this period) -- chosing linear method instead of patchworking too many datasets, as the authors made no mention of other sources. 
* 23-05-2026 [ES]: Testing with PWT gdp variables returns similar results, but does not require as much data filling. Using rgdpo variable for current testing
* 23-05-2026 [CL]: Downloading trade variables from IMF. Computing the share of exports giong to the Euro Area for each country. Exploring the trade trends via through graphics. Running regressions like DID and TWFE, while checking some assumptions (parallel trends...)
* 24-05-2026 [ES]: Testing validity of adding other donor countries. Looking at results when including non-fixed peg countries in Africa (more comparable than elsewhere) shifts almost all weight onto these countries. Gives nice results, but not a suitable set for SCM if all weights end there. 
* 24-05-2026 [ES]: Testing with added special variable years: 1985, 1990. Some slight improvement in outcome, may keep -- justified by poor data for some input variables. Keeping only 1990 to avoid overfitting
* 24-05-2026 [ES]: Testing without industry variable, as it is causing poor fit for countries with missing data (CAF, Chad)
* 24-05-2026 [ES]: Out of GDP variables, wdi_gdp_pc_current is closest fit to decribed variables in report, and also has minimal missing data

