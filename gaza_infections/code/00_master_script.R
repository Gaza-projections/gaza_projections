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
    pak,         # Needed to install epiparameter
    readxl,      # Read Excel files
    reshape2,    # For converting between wide and long data structure
    scales,      # Scaling and formatting data for visualizations
    socialmixr,   # Social mixing matrices for epidemic models
    tidyverse,   # Tidyverse suite of packages
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
    
    # Colour-blind palette for graphing
    palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
      "#0072B2", "#D55E00", "#CC79A7")
    show_col(palette_cb)


#...............................................................................  
### Sourcing functions
#...............................................................................

source(paste(dir_path, "code/00_specify_functions.R", sep =""))

    
        
#...............................................................................  
### Reading in required parameters and data
#...............................................................................

  #...................................      
  ## Read parameters from main parameters file

    # Identify file name
    filename <- paste(dir_path, 'inputs/', "gaza_infections_parameters.xlsx", sep="")
    
    # Read general parameters
    gen_pars <- data.frame(readxl::read_excel(filename, sheet = "general"))

    # Read list of diseases
    diseases <- data.frame(readxl::read_excel(filename, 
      sheet = "list_diseases"))

    # Read immunity assumptions for diseases where these are not modelled
    immunity_assumptions <- data.frame(readxl::read_excel(filename, 
      sheet = "immunity_assumptions"))
    
    # Read non-local epidemic parameters needed for SEIR models
    epidemic_pars <- data.frame(readxl::read_excel(filename, 
      sheet = "epidemic_parameters"))

    # Read data on past epidemiology
    past_epi <- data.frame(readxl::read_excel(filename, 
      sheet = "past_epidemiology"))
 
  #...................................      
  ## Read in or set other parameters

    # Identify start and end dates of projection period
    date_start <- dmy(gen_pars[which(gen_pars$parameter == "date_start"), 
      "value_gen"])
    date_end <- dmy(gen_pars[which(gen_pars$parameter == "date_end"), 
      "value_gen"])
    
    # Identify scenarios
    scenarios <- c("central", "worst", "best")
    
    # Identify age groups
    ages <- grep("value_a", colnames(gen_pars), value = TRUE)
    ages <- gsub("value_a", "", ages)

    # Identify population per age group
    pop <- gen_pars[which(gen_pars$parameter == "pop"), 
      grep("value_a", colnames(gen_pars))]
    pop <- unlist(as.vector(pop))
    names(pop) <- gsub("value_a", "", names(pop))
    
    # Identify epidemic-prone diseases
    diseases_epid <- unique(diseases[which(diseases$category == "epidemic"), 
      "disease"])
           
    
######TO DELETE################################################    
  #...................................      
  ## Create dummy input datasets (just for developing code)
    # Proportion unprotected from infection and severe disease 
      # - from immunity model
    s <- data.frame(month = 1:6)
    s[, ages] <- 0
    s_modelled <- list()
    for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
      for (j in scenarios) {
        
        # if scenario is optimistic
        if (j == "best") {
          x <- s
          x[, ages] <- 0.20
          s_modelled[[i]][[j]] <- x
        }  
        
        # if scenario is central
        if (j == "central") {
          x <- s
          x[, ages] <- 0.40
          s_modelled[[i]][[j]] <- x
        }  
        
        # if scenario is pessimistic
        if (j == "worst") {
          x <- s
          x[, ages] <- 0.60
          s_modelled[[i]][[j]] <- x
        }  
          
      }
    }  

    # Proportion unprotected from infection but protected against severe disease 
      # - from immunity model
    vd <- data.frame(month = 1:6)
    vd[, ages] <- 0  
    vd_modelled <- list()
    for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
      for (j in scenarios) {
        
        # if scenario is optimistic
        if (j == "best") {
          x <- vd
          x[, ages] <- 0.40
          vd_modelled[[i]][[j]] <- x
        }  
        
        # if scenario is central
        if (j == "central") {
          x <- vd
          x[, ages] <- 0.30
          vd_modelled[[i]][[j]] <- x
        }  
        
        # if scenario is pessimistic
        if (j == "worst") {
          x <- vd
          x[, ages] <- 0.20
          vd_modelled[[i]][[j]] <- x
        }  
          
      }
    }  

    # Dummy empirical distributions of R0, by period
      # dummy empirical distributions of R0 for measles, by period
      r0_measles <- data.frame(r0 = seq(5, 15, 0.5))
      r0_measles$p <- dnorm(r0_measles$r0, 10, 2)
      r0_measles$p_cum <- cumsum(r0_measles$p) / sum(r0_measles$p)
  
      # dummy empirical distributions of R0 for cholera, by period
      r0_cholera <- data.frame(r0 = seq(4, 10, 0.5))
      r0_cholera$p <- dnorm(r0_cholera$r0, 6, 1)
      r0_cholera$p_cum <- cumsum(r0_cholera$p) / sum(r0_cholera$p)
      
      # overall
      
    
    
#################################################################
        
  #...................................      
  ## Populate proportions susceptible to infection values
    # Generate structure of each disease-scenario dataframe
    s <- data.frame(month = 1:6)
    s[, ages] <- 0
    
    # Nested list that will hold all of them
    s_list <- list()
    
    # For each disease...
    for (i in diseases$disease) {
      
      # if disease immunity is modelled, grab values from model output..
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          s_list[[i]][[j]] <- s_modelled[[i]][[j]] + vd_modelled[[i]][[j]]
        }
      }
      
      # if instead disease immunity is assumed, single value applies to all
        # scenarios and months...
      if (i %in% 
          diseases[which(diseases$immunity_source == "assumed"), "disease"]) {
        
        # grab values from assumptions table
        x <- immunity_assumptions[which(immunity_assumptions$parameter == "s" &
          immunity_assumptions$disease == i), 
          grep("value_a", colnames(immunity_assumptions))]
        
        # assign them to each scenario and month
        for (k in 1: nrow(s)) {
          s[k, ages] <- x
        }
        for (j in scenarios) {
          s_list[[i]][[j]] <- s
        }
      }
    }  
    
    
  #...................................      
  ## Populate proportions not protected from infection but 
    # (vaccine-)protected from severe disease
    
    # Generate structure of each disease-scenario dataframe
    vd <- data.frame(month = 1:6)
    vd[, ages] <- 0
    
    # Nested list that will hold all of them
    vd_list <- list()
    
    # For each disease...
    for (i in diseases$disease) {
      
      # if disease immunity is modelled, grab values from model output..
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          vd_list[[i]][[j]] <- vd_modelled[[i]][[j]]
        }
      }
      
      # if instead disease immunity is assumed, single value of 0 applies to all
        # scenarios and months...
      if (i %in% 
          diseases[which(diseases$immunity_source == "assumed"), "disease"]) {
        for (j in scenarios) {
          vd_list[[i]][[j]] <- vd
        }
      }
    }  
    
    
        
  #...................................      
  ## Read in and prepare social mixing (contact) matrix  
    
    # Try loading contact data from Digaale IDP camp, Somaliland (2019)
    digaale <- tryCatch(suppressWarnings(read_rds(paste(dir_path, 'inputs/', 
      "digaale_svy.rds", sep=""))), error = function (e) {"file not found"})
    
    # If the data are not found, download them and save them
    if ("file not found" %in% digaale) {
      digaale <- 
        socialmixr::get_survey("https://zenodo.org/doi/10.5281/zenodo.5226280")
      write_rds(digaale, paste(dir_path, 'inputs/', "digaale_svy.rds", sep=""))
    }

    # Try loading sample age distribution data from Digaale survey
    digaale_pop <- tryCatch(suppressWarnings(read_rds(paste(dir_path, 'inputs/', 
      "digaale_pop.rds", sep=""))), error = function (e) {"file not found"})
    
    # If the data are not found, download them and save them
    if ("file not found" %in% digaale_pop) {
      digaale_pop <- 
        read.csv("https://zenodo.org/records/7071876/files/espicc_somaliland_digaale_survey_population.csv?download=1")
      write_rds(digaale_pop, paste(dir_path, 'inputs/', 
        "digaale_pop.rds", sep=""))
    }
    
    # Prepare contact data: see socialmixr guidance - need to weight for
      # day of the week and also sampling weights in the Digaale survey
    contact_data <- socialmixr::contact_matrix(
      digaale,
      survey.pop = digaale_pop,
      age.limits = digaale_pop$lower.age.limit,
      symmetric = TRUE,
      weigh.dayofweek = TRUE,
      weights = "sample_weight",
      per.capita = TRUE
    )

    # Visualise contact matrix and save plot
    df <- melt(contact_data[["matrix"]], 
      varnames = c("age_group_contactee", "age_group_contacter"), 
      value.name = "contacts")
    df$labels <- round(df$contacts, 2)
    df$age_group_contactee <- levels(df$age_group_contacter)
    ggplot(df, aes(x = age_group_contactee, y = age_group_contacter, 
      fill = contacts) ) +
      geom_tile(colour = "grey20") +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_fill_gradient(low = "white", high = palette_cb[7]) +
      geom_text(aes(label = labels), color = "grey20", size = 4) +
      scale_x_discrete("age group of contactee (years)", expand = c(0,0)) +
      scale_y_discrete("age group of contacter (years)", expand = c(0,0)) +
      labs(fill = "contacts per day")
    ggsave(paste(dir_path, 'outputs/', "social_mixing_matrix.png", sep=""),
      units = "cm", dpi = "print", width = 15, height = 10)
  
    # Grab contact matrix
    contact_matrix <- t(contact_data[["matrix"]])
    colnames(contact_matrix) <- rownames(contact_matrix)
      input_sum <- sum(contact_matrix)  
    
    # Distribute contact matrix into desired (finer) age groups
    contact_matrix <- f_target()
      # check that matrix sum is still the same
      sum(contact_matrix) == input_sum
      
      
  #...................................      
  ## Prepare objects for SEIR models, for any epidemic disease
    # and scenario
    
    # Initialise a generic timeline
    timeline <- data.frame(date = as_date(date_start:date_end), 
      time = 0:(date_end - date_start), time_epid = 0, 
      subperiod = "subperiod1")
    timeline$subperiod <- ifelse(timeline$time > mean(timeline$time), 
      "subperiod2", "subperiod1")
      
      # identify start and end times of each subperiod
      time_periods <- c(min(timeline$time), 
        max(timeline[which(timeline$subperiod == "subperiod1"), "time"]),
        max(timeline$time)
      )
      names(time_periods) <- c("start", "end_subperiod1", "end_subperiod2")
      
    # Prepare the demography vector (age-specific population)
    demography_vector <- pop
    names(demography_vector) <- rownames(contact_matrix)

    # Initialise dataframe of model compartment sizes, by age (as proportions)
      # compliant with 'epidemics' package
    initial_conditions <- matrix(0, nrow = length(ages), ncol = 5,
      dimnames = list(rownames(contact_matrix), c("S", "E", "I", "R", "V")))
    initial_conditions <- as.data.frame.matrix(initial_conditions)
        
    # Initialise dataframe of proportions of each age group who are
        # protected against severe disease (Vd), out of all who are susceptible
        # to infection (Vd + S): from immunity analysis
    initial_prop_vd <- data.frame(age_group = rownames(contact_matrix), S = NA, 
      Vd = NA, prop_vd = NA)  


#...............................................................................  
### ENDS
#...............................................................................
     