This R project contains the code used to create data, figures, and tables for the 2022 Bluefish Research Track Assessment Ecosystem and Socioeconomic Profile document. Figures and tables were compiled with the `_targets.R` script using the {targets} R package. This script calculates most indicators from their source data. This R project is intended to allow a user to recreate the 2022 Bluefish Ecosystem and Socioeconomic Profile data and analyses; however, due to local differences in R packages, file paths, and R versions, it is likely that a user will need to complete (hopefully minimal) troubleshooting in order to successfully execute the entire codebase. We hope that the documention here, in the `_targets.R` file, in the `data-raw/scripts` folder, and in the `R` folder functions will help users complete any needed troubleshooting.

Some calculations are dependent on files that exist outside of this repository:
- Relative condition calculations are dependent on length-weight data housed (at the time of the 2022 RT assessment) at `github.com/samtruesdell/bluefishLifeHistory`
- Temperature indicators (based on the Physical Sciences Laboratory OISST dataset) were imported as .csvs from `github.com/kimberly-bastille/ESPplayground`; temperature indicator creation scripts are also housed at `github.com/kimberly-bastille/ESPplayground` (see `https://github.com/kimberly-bastille/ESPplayground/blob/main/.github/workflows/calculate_temperature_indicators.yml` and `https://github.com/kimberly-bastille/ESPplayground/blob/main/.github/workflows/calculate_temperature_july.yml`)
- VAST center of gravity indicators were calculated separately and .csvs are read in. VAST scripts, data, and figures have been copied into the `data-raw/VAST` folder, but relative file paths in the code have not been updated.

Folders and files in this repository:
- `_targets` folder: Automatically created and updated by the {targets} package.
- `data`: Stored R package data. Automatically created and updated by the {usethis} package.
- `data-raw` folder:
  - `old` folder: Data and scripts not used in the final ESP.
    - `2009-2019 Rec Scores_ME_MS_031522 cw.xlsx`: Community reliance and engagement data provided by Changhua Weng.
    - `american-fisheries-society.csl`: Citation Style Language file for formatting references.
    - `bluefish ESP figure information.csv`: Caption and alt text information for the images in the ESP document (captions may have been edited in the full document after initial compiling with this spreadsheet).
    - `bluefish_references.csv`: References.
    - `catch_wave.RDS`: MRIP catch, intermediate file.                           
    - `ChesMMAP Table.xlsx`: ChesMMAP bluefish diet information, not used in report but preserved for potential future use.          
    - `Conn YOY.csv`: Conn young of year index, saved from Working Group's google drive for correlation analyses.
    - `effort.RDS`: MRIP effort, intermediate file.
    - `indicator_key.csv`: Order information to link figures and captions/alt text.
    - `NEAMAP Table.xlsx`: NEAMAP bluefish diet information, not used in report but preserved for potential future use.     
    - `references.bib`: References.
    - `references.txt`: References.                                            
    - `SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL_2020Constantdols.csv`: Commercial socioeconomic indicators provided by Samantha Werner.
    - `SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL_2020Constantdols.xls`: Commercial socioeconomic indicators provided by Samantha Werner.
    - `Standardized_CPUE_StatexWave.csv`: Standardized bluefish CPUE data provided by Katie Drew.
    - `temperature_indicators.csv`: Temperature indicator data calculated at `github.com/kimberly-bastille/ESPplayground`
    - `temperature_july_bfstrata.csv`: Temperature indicator data calculated `at github.com/kimberly-bastille/ESPplayground`
    - `Unstdz_CPUE_StatexWave.csv``: Unstandardized bluefish CPUE data provided by Katie Drew.
    - `uwnd.10m.mon.mean.nc`: Monthly wind data downloaded from the Physical Sciences Laboratory.
    - `vwnd.10m.mon.mean.nc`: Monthly wind data downloaded from the Physical Sciences Laboratory.
  - `scripts` folder:
    - `create_mrip_keys.R`: Create keys data objects to translate MRIP abbreviations.
    - `create_references.R`: Compile references from a spreadsheet into a .bib file
    - `Econ_Indicator_CODE_github.R`: Create commercial indicators from database queries.
    - `pull_MRIP_data.R`: Pull MRIP catch and effort data from noaa.gov.
    - `pull_nmfs_bottom_trawl.R`: Pull NMFS bottom trawl data from Oracle database.
  - `MRIP_data` folder: MRIP catch and effort data downloaded from noaa.gov.
  - `MRIP_directed_trips` folder: MRIP directed trips data downloaded from noaa.gov.
  - `VAST` folder: Copies of VAST scripts, data, and figures. Importantly, relative file paths in the code have not been updated.
- `docs` folder: 
  - `indicator_figs.Rmd` and `indicator_figs.docx`: Rmarkdown file creates single Word document containing ESP data visualizations.
  - `length-fig-child.Rmd`: Child document supporting `indicator_figs.Rmd`.
  - `fig-child.Rmd`: Child document supporting `indicator_figs.Rmd`.
  - `references.Rmd` and `references.html`: Rmarkdown file that compiles references into bibliography format.
  - `presentation figures.R`: Script to create data visualizations used in the presentation to the peer review panel.
  - `preliminary docs` folder: Non-final documents and data visualizations.
- `images` folder: Images provided for the ESP report, and images created for the peer review presentation.
- `man` folder: Automatically created and updated by the {usethis} package.
- `R` folder: Functions created and used for data processing, analysis, and visualization.

The only file not included in this repo is `data-raw/SMA2017_Bratio.xlsx`, which was kindly provided by ICCAT and therefore we do not wish to be a secondary distributor of this file. 
