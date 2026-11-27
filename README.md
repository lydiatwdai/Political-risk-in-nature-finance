# Country risk and finance for ecosystem-based adaptation: cross-country analysis

Code and data for the cross-country part of the study *Country risk or risk assessment? What limits finance for
ecosystem-based adaptation*. The analysis relates three finance flows (Public Finance, Philanthropy Finance and
VCM Finance, i.e. retired nature-based carbon credits) to six country risk indicators in 130 low- and
middle-income countries. The results are associations only.

Interview data are not part of this repository.

## Data that are not included

ACLED and GEMS variables are not redistributed here:

- **ACLED** (columns starting `acled_`): the end user licence agreement does not allow the data to be republished.
  Download from https://acleddata.com under your own account.
- **GEMS** (columns starting `gems_`): European Investment Bank copyright, and no licence permitting redistribution
  was found. Download from https://www.gemsriskdatabase.org.

To run the full analysis, create `data/restricted/restricted_variables.csv` with one row per country, a column `iso3`
and the columns marked `in_public_dataset = no` in `data/data_dictionary.csv` that the scripts use:
`acled_danger_value_2025`, `acled_index_ranking_2025`, `acled_deadliness_value_2025`, `acled_fragmentation_value_2025`,
`acled_diffusion_value_2025`, `gems_sovereign_default_rate`, `gems_private_avg_default_rate`,
`gems_public_avg_default_rate`. Do not commit this file.

## Reproduce the results

Run from the repository root:

```bash
LC_ALL=en_US.UTF-8 Rscript run_all.R
```

Without the restricted file, this runs the simple regressions and Figures 3 and 4. With it, it runs everything.
The tables and figures in `output/` come from the full run.

Packages (tested with R 4.5.1):

```r
install.packages(c("readxl", "ggplot2", "gridExtra", "scales", "ggrepel", "ragg"))
```

`openxlsx` is needed only for the scripts in `data_preparation/`.

## Contents

| Path | Content |
|---|---|
| `data/MASTER_LMIC_incl_Ethiopia.xlsx` | Public part of the analysis dataset, one row per country (sheet `LMIC only`) |
| `data/data_dictionary.csv` | Definition, source, year, unit, role and licence of every variable |
| `R/00_setup.R` | Data loading, variable preparation and helper functions |
| `R/01_multiple_regression.R` | Models 1 and 2; Supplementary Tables 5, 6, 9, 10; Supplementary Figures 1, 2 |
| `R/02_supplementary_tables.R` | Supplementary Tables 6, 7, 8; Supplementary Figure 3 |
| `R/03_simple_regression.R` | Simple regressions on Political Stability; Supplementary Table 4 |
| `R/04_public_finance_figures.R` | Figures 3 and 4; income-group regressions |
| `R/05_transformation_check.R` | Choice of outcome transformation |
| `output/` | Tables and figures produced by the full run |
| `data_preparation/` | Provenance scripts that added columns to the dataset (see below) |

## Data preparation

The dataset was assembled from the public sources listed in `data/data_dictionary.csv`. Only part of that
assembly is scripted: adding population, land area, GDP per capita and Ethiopia. The other columns, including the
three finance flows and the risk indicators, were compiled from the source files before these scripts, and no build
script for that step is included. The scripts in `data_preparation/` need the original source files and are not run
by `run_all.R`.

## Data sources and licences

Each column remains under its provider's terms. Please cite the providers.

| Data | Terms |
|---|---|
| OECD (2026). Climate-related development finance, recipient perspective, 2024. https://www.oecd.org/en/data/dashboards/development-finance-for-climate.html | Reuse permitted with citation: https://www.oecd.org/en/about/terms-conditions.html |
| OECD (2026). Private philanthropy for development, 2020-2023. https://www.oecd.org/en/publications/private-philanthropy-for-development-third-edition_98e676c0-en.html | As above |
| Berkeley Voluntary Registry Offsets Database v2026-04. https://gspp.berkeley.edu/berkeley-carbon-trading-project/offsets-database | CC BY 4.0 |
| World Bank: Worldwide Governance Indicators, World Development Indicators, country income groups. https://data.worldbank.org | CC BY 4.0: https://data.worldbank.org/summary-terms-of-use |
| United Nations, World Population Prospects 2024. https://population.un.org/wpp/ | CC BY 3.0 IGO |
| Naturebase (2025). https://naturebase.org (column `ncs_potential_tco2e`) | CC BY-NC-SA 4.0: non-commercial use, with attribution, shared under the same licence |

The values were filtered, aggregated or averaged by the authors as described in the data dictionary. The providers
do not endorse this work.
