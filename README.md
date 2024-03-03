## Scenario-based health impact projections of the war in Gaza (2023-2024)
## Repository of analysis scripts and analysis datasets

Contributors: Francesco Checchi, Zhixi Chen, Zeina Jamaluddine, Greg Barnsley, Sebastian Funk, Pratik Gupte, Tak Igusa.

Funding: UK Humanitarian Innovation Hub

### General information
Welcome to this repository. Every sub-folder on this directory corresponds to a cause-specific module of our report and corresponding Methods Annex, published at www.gaza-projections.org . The sub-folder `gaza_overall` contains code and data required to do final analysis of all the cause-specific modules together.

We will be adding more variable dictionaries and descriptions of code and datasets. Please bear with us while we assemble all of this information.

All of the data contained in this repository are in the public domain, or we have received permission to publish them. The study has received ethics approval from the London School of Hygiene and Tropical Medicine and the Johns Hopkins University Bloomberg School of Public Health.

To replicate the analysis, please download the entire repository and keep the folder structure as it is. Each folder has an inputs-code-outputs structure. All input files are contained in the `/inputs` sub-folder, while all the code is in the `/code` sub-folder and all outputs (data files, tables, graphs) are saved to `/outputs`. Analyses in R can be re-run from the `gaza_[xxx]_00_master_script.R` code, which will load packages and source all other scripts. The directory for reading and outputting files is set automatically when `gaza_[xxx]_00_master_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). We recommend to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source.

### Traumatic injuries (folder 'gaza_injuries')
(please check back for more content)

### Malnutrition (folder 'gaza_nutrition')
The input files are as follows:
- `gaza_nutrition_parameters.xlsx` contains several worksheets (tabs), all of which but `lists` are read into the analysis. Worksheet `general` allows the user to set parameters for the nutrition analysis. Worksheet `scenarios` contains assumptions made for each scenario concerning daily caloric intake from humanitarian aid during months 1-3 and 4-6 of the projection period, by scenario and as a ratio relative to the target daily intake (see worksheet `general`). Worksheet `non-aid` contains assumptions by month (from 7 october 2023 to the end of the projection period) and scenario regarding the proportion of the pre-war caloric intake that would have been met by existing food stocks or the agriculture/livestock sector. Lastly, worksheet `breastfeeding` contains pre-crisis and crisis period estimate of the proportion of infants exclusively breastfeeding during the first 6 months of life, for other crisis-affected settings in the Middle East.
- 
