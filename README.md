## Scenario-based health impact projections of the war in Gaza (2023-2024)
## Repository of analysis scripts and analysis datasets

Contributors: Francesco Checchi, Zhixi Chen, Zeina Jamaluddine, Greg Barnsley, Sebastian Funk, Pratik Gupte, Tak Igusa.

Funding: UK Humanitarian Innovation Hub

### General information
Welcome to this repository. Every sub-folder on this directory corresponds to a cause-specific module of our report and corresponding Methods Annex, published at www.gaza-projections.org . The sub-folder `gaza_overall` contains code and data required to do final analysis of all the cause-specific modules together.

We will be adding more variable dictionaries and descriptions of code and datasets. Please bear with us while we assemble all of this information.

All of the data contained in this repository are in the public domain, or we have received permission to publish them. The study has received ethics approval from the London School of Hygiene and Tropical Medicine and the Johns Hopkins University Bloomberg School of Public Health.

To replicate the analysis, please download the entire repository and keep the folder structure as it is. Each folder has an inputs-code-outputs structure. All input files are contained in the `/inputs` sub-folder, while all the code is in the `/code` sub-folder and all outputs (data files, tables, graphs) are saved to `/outputs`. Analyses in R can be re-run from `gaza_[xxx]/code/00_master_script.R`, which will load packages and source all other scripts for a given cause-specific module. The directory for reading and outputting files is set automatically when `00_master_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). We recommend to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source.

### Traumatic injuries (folder 'gaza_injuries')
(please check back for more content)

### Malnutrition (folder 'gaza_nutrition')
#### Input files
- `gaza_nutrition_parameters.xlsx` contains several worksheets (tabs), all of which but `lists` are read into the analysis. Worksheet `general` allows the user to set parameters for the nutrition analysis. Worksheet `scenarios` contains assumptions made for each scenario concerning daily caloric intake from humanitarian aid during months 1-3 and 4-6 of the projection period, by scenario and as a ratio relative to the target daily intake (see worksheet `general`). Worksheet `non-aid` contains assumptions by month (from 7 october 2023 to the end of the projection period) and scenario regarding the proportion of the pre-war caloric intake that would have been met by existing food stocks or the agriculture/livestock sector. Lastly, worksheet `breastfeeding` contains pre-crisis and crisis period estimate of the proportion of infants exclusively breastfeeding during the first 6 months of life, for other crisis-affected settings in the Middle East.
- `gaza_food_trucks.csv` contains the reported number of food trucks reaching any open crossing into Gaza, per day.
- `gaza_survey2020_kcal_bmi_agg` contains aggregate (by age, sex) data from a 2020 survey of adults aged 40+ years in Gaza. For each age-sex stratum the dataset reports mean values of the weight, height and daily Kcal intake. The `svy_wt` column contains the relative proportion of survey participants in each age-sex stratum, and is used when averaging results to all age groups.
- `gm_anthro_2019_agg.rds` contains aggregate (by age in months, sex, height and weight) anthropometric data from systematic growth monitoring of children aged 6-59 months old in Gaza. The data were collected in 2019. The `wt` variable is the proportion of children within the age-sex-height-weight-stratum, and is used when averaging GAM and SAM prevalences for the entire population aged 6-59 momths old.

#### Analysis scripts
- `00_master_script.R` installs/loads packages, sets colour palettes, initialises random numbers, recognises the local directory and calls all the other scripts;
- '01_specify_wt_functions.R` specifies functions needed to model adult weight loss;
- `02_read_prepare_inputs.R` reads data and parameters and prepares them for further analysis;
- `03_estimate_food_aid.R` applies assumptions and uncertainty ranges to the data on food trucks so as to estimate/project caloric intake to date/over the projection period, by scenario. This is done through a simulation; each simulation run is outputted, and graphs are generated.
- `04_estimate_wt_loss.R` applies the estimated/projected nutrient intake to a 2020 survey of adult anthropometry and food intake. A mechanistic model is used to estimate/project percent weight loss. The analysis occurs in a simulation to account for parameter uncertainty; each simulation run is outputted, and graphs are generated.
- `05_project_gam_sam.R` applies the estimated/projected adult weight loss to pre-war anthropometric data from children 6-59 months old, and computed globald and severe acute malnutrition prevalence. This is also done in a simulation. Graphs and tables are outputted.
- `06_project_breastfeeding.R` projects the reduction in exclusive breastfeeding based on the pre-war level in Gaza and observed reductions in crisis-affected settings in the region.
