#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO LOAD PACKAGES AND SOURCE OTHER ANALYSIS SCRIPTS  ------ ##
#...............................................................................

                          # Francesco Checchi, Zeina Jamaluddine (January 2024)

#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  pacman::p_load(
    anthro,      # To compute anthropometric scores
    betareg,     # To fit beta regression
    flextable,   # To write tables in .docx format
    gamlss,      # To fit generalised additive models
    ggplot2,     # Data visualization
    ggpubr,      # Arranging multiple plots into a single plot
    glmmTMB,     # For fitting generalised linear mixed models
    gtools,      # Assist various programming tasks
    haven,       # Read Stata datasets
    lubridate,   # Makes it easier to work with dates and times
    MASS,        # For various statistical functions
    mgcv,        # To fit GAM models    
    parameters,  # Extract model fit parameters
    ranger,      # Random forest fitting
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
    
    # # Colour-blind palette for graphing
    # palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    #   "#0072B2", "#D55E00", "#CC79A7")
    palette_cb <- viridis(16)
    show_col(palette_cb)


#...............................................................................
### Sourcing dependent scripts
#...............................................................................
    
  #...................................      
  ## Read and prepare inputs
  source(paste(dir_path, "code/", "01_read_prepare_inputs.r", sep="") )
    
  # #...................................      
  # ## Fit model of weight loss as a function of caloric intake deficit
  # source(paste(dir_path, "code/", "02_fit_wt_loss_model.r", sep="") )
 
  #...................................      
  ## Specify functions for weight change model
  source(paste(dir_path, "code/", "02_specify_wt_functions.r", sep="") )
    
  #...................................      
  ## Estimate caloric intake from food aid from crisis start to date
  source(paste(dir_path, "code/", "03_estimate_food_aid.r", sep="") )
    
  #...................................      
  ## Estimate caloric intake deficit and weight loss among adults
  source(paste(dir_path, "code/", "04_estimate_wt_loss.r", sep="") )
    
  #...................................      
  ## Estimate prevalence of acute malnutrition among children
  source(paste(dir_path, "code/", "05_project_gam_sam.r", sep="") )
    
  #...................................      
  ## Project prevalence of exclusive breastfeeding
  source(paste(dir_path, "code/", "06_project_breastfeeding.r", sep="") )
    
    
#...............................................................................  
### ENDS
#...............................................................................
     