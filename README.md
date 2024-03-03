## Scenario-based health impact projections of the war in Gaza (2023-2024)
## Repository of analysis scripts and analysis datasets

Contributors: Francesco Checchi, Zhixi Chen, Zeina Jamaluddine, Greg Barnsley, Sebastian Funk, Pratik Gupte, Tak Igusa.

Funding: UK Humanitarian Innovation Hub


### General information
Welcome to this repository. Every sub-folder on this directory corresponds to a cause-specific module of our report and corresponding Methods Annex, published at www.gaza-projections.org . The sub-folder `gaza_overall` contains code and data required to do final analysis of all the cause-specific modules together.

All of the data contained in this repository are in the public domain, or we have received permission to publish them. The study has received ethics approval from the London School of Hygiene and Tropical Medicine and the Johns Hopkins University Bloomberg School of Public Health.

To replicate the analysis, please download the entire repository and keep the folder structure as it is. Each folder has an inputs-code-outputs structure. All input files are contained in the `/inputs` sub-folder, while all the code is in the `/code` sub-folder and all outputs (data files, tables, graphs) are saved to `/outputs`. Analyses in R can be re-run from `gaza_[xxx]/code/00_master_script.R`, which will load packages and source all other scripts for a given cause-specific module. The directory for reading and outputting files is set automatically when `00_master_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). We recommend to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source. R package `epidemics`, needed for the infectious diseases analysis, will likely require installing Rtools43 (https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html) first. This rather large bundle can be uninstalled once `epidemics` is successfully installed and compiled.

***

### Traumatic injuries (folder `gaza_injuries`)
#### Input files
- `gaza_injuries_data.xlsx` contains all the data needed for this module, including daily numbers of deaths and injuries reported by the MoH and UNRWA (worksheet `daily`), line lists of injured persons as provided by the MoH (`moh_list`), extent of internal displacement in shelters (`idps`) and data on deaths and injuries overall and due to unexploded ordnance/mines during the 2014 war (`ordnance`). Each worksheet is one dataset and the `dictionary` worksheet (not needed for analysis) describes each variable.
- `gaza_injuries_parameters.xlsx` contains parameters needed for the analysis, which can be modified here by the user (worksheet `general`). Worksheet `surv` contains meta-data from a large cohort of injuries, where variable `day` is the number of days since the injury and `p_d` is the proportion of cases who died during the interval since the previous time point.
- `gaza_noninjury_to_date.xlsx` has been generated manually by binding together deaths due to causes other than traumatic injury, as estimated/projected in all the other cause-specific modules. This is needed here to subtract a non-injury death level from projections based on MoH data.
- `out_adjustment_factors.csv` is an output of the `gaza_overall` code and is needed here to adjust final injury estimates for the probability of dying from competing causes. See Methods Annex.

#### Analysis scripts
- `00_master_script.R` installs/loads packages, sets colour palettes, initialises random numbers, recognises the local directory and calls all the other scripts;
- `01_read_prepare_data.R` reads data and parameters and prepares them for further analysis;
- `02_prepare_simulations.R` visualises MoH and UNRWA data; fits count models of deaths and injuries, for both the escalation and status quo scenarios; fits a model of the deaths reporting fraction; figures out the age-sex distribution of injury deaths; prepares a template timeline/cohort for further analysis steps; and works out the proportion of injury deaths who die immediately; graphs are produced and outputs saved for next steps;
- `03_estimate_cf_scenario.R` implements a simulation to project injury deaths and injuries in the ceasefire scenario, including deaths due to injuries sustained previously during the war and due to unexploded ordnance;
- `04_estimate_sq_es_scenarios.R` implements a simulation to project injury deaths and injuries in the status quo and escalation scenarios;
- `05_analyse_visualise.R` collects outputs of the above simulations, computes means and uncertainty intervals and tabulates/graphs the findings.

***

### Malnutrition (folder `gaza_nutrition`)
#### Input files
- `gaza_nutrition_parameters.xlsx` contains several worksheets (tabs), all of which but `lists` are read into the analysis. Worksheet `general` allows the user to set parameters for the nutrition analysis. Worksheet `scenarios` contains assumptions made for each scenario concerning daily caloric intake from humanitarian aid during months 1-3 and 4-6 of the projection period, by scenario and as a ratio relative to the target daily intake (see worksheet `general`). Worksheet `non-aid` contains assumptions by month (from 7 october 2023 to the end of the projection period) and scenario regarding the proportion of the pre-war caloric intake that would have been met by existing food stocks or the agriculture/livestock sector. Lastly, worksheet `breastfeeding` contains pre-crisis and crisis period estimate of the proportion of infants exclusively breastfeeding during the first 6 months of life, for other crisis-affected settings in the Middle East.
- `gaza_food_trucks.csv` contains the reported number of food trucks reaching any open crossing into Gaza, per day.
- `gaza_survey2020_kcal_bmi_agg` contains aggregate (by age, sex) data from a 2020 survey of adults aged 40+ years in Gaza. For each age-sex stratum the dataset reports mean values of the weight, height and daily Kcal intake. The `svy_wt` column contains the relative proportion of survey participants in each age-sex stratum, and is used when averaging results to all age groups.
- `gm_anthro_2019_agg.rds` contains aggregate (by age in months, sex, height and weight) anthropometric data from systematic growth monitoring of children aged 6-59 months old in Gaza. The data were collected in 2019. The `wt` variable is the proportion of children within the age-sex-height-weight-stratum, and is used when averaging GAM and SAM prevalences for the entire population aged 6-59 momths old.
- `gaza_nutrition_starvation_lit.xlsx` contains metadata extracted from old studies of people exposed to starvation conditions. The main outcome of interest is weight loss. This dataset is read and analysed by the archived R script `02_fit_wt_loss_model.R` (see below), but is not used for the analysis.

#### Analysis scripts
- `00_master_script.R` installs/loads packages, sets colour palettes, initialises random numbers, recognises the local directory and calls all the other scripts;
- `01_specify_wt_functions.R` specifies functions needed to model adult weight loss;
- `02_read_prepare_inputs.R` reads data and parameters and prepares them for further analysis;
- `03_estimate_food_aid.R` applies assumptions and uncertainty ranges to the data on food trucks so as to estimate/project caloric intake to date/over the projection period, by scenario. This is done through a simulation; each simulation run is outputted, and graphs are generated.
- `04_estimate_wt_loss.R` applies the estimated/projected nutrient intake to a 2020 survey of adult anthropometry and food intake. A mechanistic model is used to estimate/project percent weight loss. The analysis occurs in a simulation to account for parameter uncertainty; each simulation run is outputted, and graphs are generated.
- `05_project_gam_sam.R` applies the estimated/projected adult weight loss to pre-war anthropometric data from children 6-59 months old, and computed globald and severe acute malnutrition prevalence. This is also done in a simulation. Graphs and tables are outputted.
- `06_project_breastfeeding.R` projects the reduction in exclusive breastfeeding based on the pre-war level in Gaza and observed reductions in crisis-affected settings in the region.
- The `archive` sub-folder includes the scripts (i) `99_read_prepare_nonpublic_data.R` so users can see how we managed datasets that are not released publicly in their original form. This script cannot be implemented as the datasets it relies on are not made public; and (ii) `02_fit_wt_loss_model.R`, which fits a model to old studies of weight loss among people exposed to starvation (see above, `gaza_nutrition_starvation_lit.xlsx`): this model has not been used in the analysis.

***

### Infectious diseases (folder `gaza_infections`)
#### Input files
- `gaza_infections_parameters.xlsx` contains various worksheets with parameters for the analysis, which the user should modify here if needed: `general` contains various general parameters; `immunity_assumptions` contains assumptions made regarding baseline susceptibility to infection and disease for various diseases not included in Gaza's routine vaccination schedule; `epidemic_parameters` contains ranges, relative values and age-specific values for different epidemic-prone infectious disease parameters including the basic reproduction number, the case-fatality ratio, the duration of the infectious and pre-infectious periods, and the proportion of infectious that become symptomatic; `endemic_parameters` contains assumed values of age-specific mortality, relative share of infectious disease mortality, and seasonality for various endemic infections; `list_diseases` is a table of all infectious diseases considered; `other_lists` is not used.
- `gaza_infections_endemic_data.xlsx` contains data on age-specific and proportional mortality due to endemic infections between 2016 and 2022, with assumptions for 2023-2024; the `dictionary` tab describes variables.
- `gaza_infections_see_data.xlsx` contains responses by an expert eliciation panels to questions related to the likely values of infection-related parameters in Gaza. The `dictionary` tab describes variables, and `lists` is not used.
- `see_distributions.rds` is produced during the analysis itself and used in subsequent steps, and contains empirical distributions of parameters, derived from expert elicitation.
- `out_adjustment_factors.csv` is an output of the `gaza_overall` code and is needed here to adjust final injury estimates for the probability of dying from competing causes. See Methods Annex.
- `fit_model_endemic_deaths.rds` is a model of annual endemic infectious deaths, fit during the analysis itself and used in subsequent steps.
- `digaale_pop.rds` and `digaale_svy.rds` are data from a social mixing survey done among IDPs in Somaliland, used as assumption of the social contact structure for Gaza. These datasets are downloaded by the code from their source online, if not already in the folder.
- the sub-folder `immunity_projections` contains estimates/projections of various immune/susceptible classes, by disease, period and scenario, as produced by the immunity-tracking model. In practice only two of the datasets are used in subsequent steps.

#### Analysis scripts
- the sub-folders `immunity_projections_model` and `immunity_projections_scripts` forward to repositories for an R package (`IVODE`) and R scripts developed for this project to track immunity/susceptibility status for diseases included in Gaza's routine vaccination programme. Please see the Readme sections for those repositories. This can be viewed as a sub-module of the infectious diseases module.
- `00_master_script.R` installs/loads packages, sets colour palettes, initialises random numbers, recognises the local directory and calls all the other scripts;
- `01_specify_functions.R` defines functions used to compute scores and weights for expert elicitation data, to implement SEIR models of each epidemic pathogen, and to distribute social mixing data into finer age groups;
- `02_read_prepare_data.R` reads and prepares datasets; analyses and graphs expert elicitation data; prepares compartments and values for SEIR models, including social mixing data.
- `03_prepare_simulations.R` prepares all the random simulations for the SEIR / epidemic projections;
- `04_run_simulations.R` runs simulations for epidemic-prone and endemic infections, and collects outputs;
- `05_analyse_and_visualise.R` analyses outputs, producing graphs and tables.
- `04b_run_simulations_to_date.R` is not needed for the analysis, but produces estimates of endemic infection deaths for the war period to date.

***

### Maternal and neonatal health problems (folder `gaza_mnh`)
#### Input files
- `gaza_mnh_list_outputs.xlsx` is the output of the LiST model as applied in this project, and is used to produce graphs and tables in R. The file contains four worksheets. Worksheets `maternal`, `neonatal` and `stillbirths` contain LiST-projected maternal deaths, neonatal deaths and stillbirths (excluding those due to traumatic injury), by scenario and period (months 1-3, months 4-6 in the projection period); the columns `mean`, `lci` and `uci` contain point estimates and 95% uncertainty intervals, while column `d_crisis_excess` indicates whether the value refers to the counterfactual baseline ('d_baseline'), the projection under scenario assumptions ('d_crisis'), or excess deaths ('d_crisis' - 'd_baseline'). Worksheet `cum` contains excess maternal deaths, neonatal deaths and stillbirths by month, including pre-war, the period to date and the three scenarios: only the point estimate is included.
- `gaza_MNH-LiST_pre war_indicators.xlsx` is a collection of pre-war indicators / data relevant to the MNH module, only included for reference.
- `gaza_MNH_LiST_final_reduction_2024.xlsx` contains the scenario-specific assumptions fed into the LiST model. For more details on how LiST was used, please see the Methods Annex or write to info@gaza-projections.org .

#### Analysis scripts
- `01_visualise_outputs.R` creates graphs based on `gaza_mnh_list_outputs.xlsx`. Note that the MNH analysis was done using the LiST model, which does not run on R software.

***

### Non-communicable diseases (folder `gaza_NCDs`)
#### Input files
(To be added)

#### Analysis scripts
(To be added)

***

### Overall analysis (folder `gaza_overall`)
#### Input files
- `gaza_overall_parameters.xlsx` contains key dates (`general` tab), age- and sex-specific population (`pop` tab) and the list of infectious diseases analysed (`list_diseases` tab).
- `gaza_overall_data.xlsx` contains projected deaths by cause (specific disease for infections and NCDs), scenario, subperiod (months 1-3 and 4-6 of the projection period) and age. Means, medians and lower/upper bounds of the uncertainty interval are provided; 'd_base_[xxx]' means counterfactual baseline estimates, 'd_crisis_[xxx]' projections based on the scenario assumptions, and 'd_excess_[xxx] the difference between 'd_crisis' and 'd_base'. At present this dataset is assembled semi-manually from cause-specific module outputs.

#### Analysis scripts
- `00_master_script.R` installs/loads packages, sets colour palettes, initialises random numbers, recognises the local directory and calls all the other scripts;
- `01_read_prepare_data.R` reads parameters and aggregated output from the cause-specific modules; and solves discrepancies in age and sex categories;
- `02_analyse_data.R` computes and applies adjustment factors for the probability of deaths from concurrent causes, and generates tables and graphs of all-cause mortality. The output `out_adjustment_factors.csv` is then manually copy-pasted into the inputs sub-folders of cause-specific modules, as needed.
