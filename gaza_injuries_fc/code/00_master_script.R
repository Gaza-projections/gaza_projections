#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO READ DATASETS AND PARAMETERS, AND CALL OTHER SCRIPTS  ---- ##
#...............................................................................


#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  pacman::p_load(
    actuar,      # To provide additional survival distributions
    betareg,     # To fit beta regression
    boot,        # To get the inverse logit
    fitdistrplus,# To fit survival functions
    flexsurv,    # To fit survival curves
    flextable,   # To write tables in .docx format
    ggplot2,     # Data visualization
    ggpubr,      # Arranging multiple plots into a single plot
    gratia,      # Simulate from a MGCV GAM model
    gtools,      # Assist various programming tasks
    lubridate,   # Makes it easier to work with dates and times
    MASS,        # Fit negative binomial model
    mgcv,        # Fit generalised additive models
    readxl,      # Read Excel files
    reshape2,    # For converting between wide and long data structure
    scales,      # Scaling and formatting data for visualizations
    tidyverse,   # Tidyverse suite of packages
    viridis,     # Colour palettes    
    zoo)         # For computing running means

  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/code", "", dir_path)
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
          
      # specific palette for the pre-war period, crisis to date period and 
        # three scenarios
      periods <-c("pre-war", "to date", "ceasefire", "status quo", "escalation")
      
      palette_periods <- c("azure4", palette_gen[c(2, 12, 8, 4)])
      names(palette_periods) <- periods
      show_col(palette_periods)

#...............................................................................  
### Sourcing other scripts
#...............................................................................

  #...................................
  ## Read and prepare datasets and parameters
  source(paste(dir_path, "code/01_read_prepare_data.R", sep =""))

  #...................................
  ## Prepare simulations
  source(paste(dir_path, "code/02_prepare_simulations.R", sep =""))

  #...................................
  ## Implement estimation for the ceasefire scenario
  source(paste(dir_path, "code/03_estimate_cf_scenario.R", sep =""))
 
  #...................................
  ## Implement estimation for the status quo and escalation scenarios
  source(paste(dir_path, "code/04_estimate_sq_es_scenarios.R", sep =""))

  #...................................
  ## Analyse and visualise data
  source(paste(dir_path, "code/05_analyse_visualise.R", sep =""))

      
      
      
      
#...............................................................................  
### ENDS
#...............................................................................
     