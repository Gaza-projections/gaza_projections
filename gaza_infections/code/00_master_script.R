#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO READ DATASETS AND PARAMETERS, AND CALL OTHER SCRIPTS  ---- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 

#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  pacman::p_load(
    flextable,   # To write tables in .docx format
    ggplot2,     # Data visualization
    ggpubr,      # Arranging multiple plots into a single plot
    glmmTMB,     # For fitting generalised linear mixed models
    gtools,      # Assist various programming tasks
    lubridate,   # Makes it easier to work with dates and times
    MASS,        # For various statistical functions
    paletteer,   # Nice colour palettes
    pak,         # Needed to install epidemics
    readxl,      # Read Excel files
    reshape2,    # For converting between wide and long data structure
    scales,      # Scaling and formatting data for visualizations
    socialmixr,   # Social mixing matrices for epidemic models
    tidyverse,   # Tidyverse suite of packages
    viridis,     # Colour palettes    
    zoo)         # For computing running means

    # Install or load R packages that don't seem to work with the above
    if(!require("epidemics", character.only = TRUE)) 
      {pak::pak("epiverse-trace/epidemics")}
    library("epidemics")
    
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
### Sourcing other scripts
#...............................................................................

  #...................................      
  ## Source analysis functions
  source(paste(dir_path, "code/01_specify_functions.R", sep =""))

  #...................................      
  ## Read and prepare datasets and parameters
  source(paste(dir_path, "code/02_read_prepare_data.R", sep =""))

  #...................................      
  ## Prepare simulations
  source(paste(dir_path, "code/03_prepare_simulations.R", sep =""))
        


#...............................................................................  
### ENDS
#...............................................................................
     