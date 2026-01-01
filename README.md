# AFCON Homogenisation (EDA)

## Objective
This project performs an exploratory analysis of Africa Cup of Nations (AFCON) matches, including both finals and qualification games, to study whether competitive balance has changed over time (match tightness, scoring trends, and home advantage).

## Data
Source repository:
https://github.com/martj42/international_results

Direct CSV used in this project:
https://raw.githubusercontent.com/martj42/international_results/master/results.csv

We filter matches whose `tournament` is either:
- African Cup of Nations
- African Cup of Nations qualification

## Project structure
- `R/`
  - `01_extract_afcon.R` : downloads the raw CSV, filters AFCON + qualifiers, creates derived variables, and saves a clean dataset
- `data/`
  - `afcon_results.csv` : generated clean dataset (created by the script)
- `figures/`
  - exported plots (optional)

## Requirements
R (>= 4.1) with the packages:
- readr
- dplyr
- stringr
- lubridate

## How to run
From the repository root:

1. Run the extraction script:
   - `Rscript R/01_extract_afcon.R`

2. The cleaned dataset will be created at:
   - `data/afcon_results.csv`

## Notes on interpretation
- `home_team` does not always imply a true home game. The variable `neutral` must be used when analyzing home advantage.
- Finals and qualification matches can have different competitive levels; analyses should compare them separately when relevant.
