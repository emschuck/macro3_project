# macro3_project

## Authors
* Elizabeth Schuck
* Mathilde Muller
* Callista Lodzinski

## Project overview

This project replicates and extends Diallo and Ba (2024), which studies whether the CFA franc zone countries experienced an economic effect from the shift in the CFA franc anchor from the French franc to the euro in 2002.

### Running the project

Running the file `results_exploration_dashboard.R` will create a dashboard to inspect the values and results of this analysis.

To execute the whole analysis and generate all outputs, from the project root, run the following command:

```r
source("code/pipeline.R")
```

The code takes a long time to run, due to the many (many, many) placebo tests. The placebo test portion of the replication file can be toggled on or off in the first lines of the script. The extension placebo script takes upwards of an hour to run. 

Individual scripts can also be run separately, as the outputs from previous runs have been included in the repository. 



Creates a Shiny dashboard used to inspect results, compare imputed and unimputed data, and visualise country-level paths during the analysis.

## Repository structure

```text
macro3_project/
├── code/
├── data/
├── output/
│   ├── extension_trade/
│   ├── figures/
│   └── tables/
├── paper/
├── reading/ 
├── README.md
└── .gitignore
```

### Files in raw data folder

* `p5v2018.xlsx` — Polity5 annual data.
* `p5v2018d.xls` — Polity5 data by date.
* IMF DOTS raw trade data used for the trade extension.

### Polity5

The Polity5 data was downloaded from:

https://www.systemicpeace.org/inscrdata.html

Downloaded on: **2026-04-23**

Contents: Polity5 Project, Political Regime Characteristics and Transitions, 1800–2018. This is annual, cross-national, time-series data coding democratic and autocratic patterns of authority and regime changes for independent countries with population greater than 500,000 in 2018.

## Code files

The project is organised as a staged R pipeline.

### `00_constants.R`
Defines constants for the project (e.g. selected variables, country lists) for consistency across scripts

### `01_WDI_data_download.R`

Downloads WDI variables used in the replication and extension.

### `01_data_download.R`

Combines data from WDI, PWT, Polity5, and FAOSTAT. Constructs the main country-year panel, handles missing values using documented rules, and saves both unimputed and imputed processed panels. Also produces replicated descriptive inflation tables for the report.

### `02_replication.R`

Runs the main GDP synthetic control replication. Estimates one synthetic control for each treated CFA country, produces actual-versus-synthetic GDP paths, GDP gap plots, donor-weight tables, predictor-balance tables, and placebo-test outputs.

### `02_Average_replication.R`

Produces descriptive average tables used in the replication section and appendix.

### `02_OLS_replication.R`

Runs auxiliary OLS-style checks related to the original paper’s appendix analysis.

### `03_extension_implementation.R`

Implements the trade extension using synthetic control methods. Outcomes include EU export share, EU import share, trade openness, exports/GDP, and imports/GDP.

### `03_extension_placebo.R`

Runs placebo tests for the trade extension.


### `pipeline.R`

Runs the main project scripts in sequence.




## Outputs
Outputs are saved in the outputs folder, separated into tables, figures, and ouptuts related the extension in three separate folders. Tables are saved as .tex files an csvs.

## Paper folder
Contains the latex report (and associated files) as well as the modelling diary.

