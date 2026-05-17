# Macro 3 Modelling Diary
## Callista Lodzinski (CL), Mathilde Muller (MM), and Elizabeth Schuck (ES)

[comment]: <> (Use CTRL+SHIFT+V to view rendered markdown in VS Code)

* 23-04-2026 : Data sourced from Polity5. Most recent file has data only from 2018. May be worth exploring other sources for similar data, for comparison points
* 16-05-2026 : Initial investigation shows some differences in inflation rate from those shown in the table -- needs investigation into reasons
* 17-05-2026 : Initially using NY.GDP.PCAP.KD = constant-price GDP per capita, but appendix lists Output-side real GDP at current $. Swapping to using NY.GDP.PCAP.CD  -- GDP per capita (current US$).
* 17-05-2026 : Penn world tables have more reliable data for output-side gdp, and include population variables -- testing using this instead of WDI variables.
* 17-05-2026 : Using PWT for employment, government consumption share, and investment share instead of WDI 
