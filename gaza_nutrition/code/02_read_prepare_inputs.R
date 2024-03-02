#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO READ DATASETS AND PARAMETERS AND MANAGE DATASETS  ----- ##
#...............................................................................




#...............................................................................  
### Reading in datasets
#...............................................................................

  #...................................      
  ## Read data on [tr]ucks arriving into Gaza

    # Identify file name
    filename <- paste(dir_path, 'inputs/', "gaza_calories.dta", sep="")
    
    # Read dataframe
    df_tr <- data.frame(haven::read_dta(filename))
    
  #...................................      
  ## Read aggregate data from a 2020 survey of NCDs,
    # containing [ad]ult BMI and diet intake

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_survey2020_kcal_bmi_agg.csv", sep="")
    
    # Read dataframe
    df_ad <- read.csv(filename)
    
    
  #...................................      
  ## Read aggregate data from 2019 [g]rowth [m]onitoring of children in Gaza
  
    df_gm <- read_rds(
      paste(dir_path, "inputs/", "gm_anthro_2019_agg.rds", sep=""))

    
#...............................................................................  
### Reading in and/or setting parameters
#...............................................................................

  #...................................      
  ## Read parameters from main parameters file

    # Identify file name
    filename <- paste(dir_path, 'inputs/', "gaza_nutrition_parameters.xlsx", 
      sep="")
    
    # Read general parameters
    gen_pars <- data.frame(readxl::read_excel(filename, sheet = "general"))

    # Read scenario values
    scenario_pars<-data.frame(readxl::read_excel(filename, sheet = "scenarios"))
    
    # Read intake to date values
    non_aid_pars <- data.frame(readxl::read_excel(filename, sheet = "non_aid"))

    # Read baseline and crisis prevalences of exclusive breastfeeding
    bf <- data.frame(readxl::read_excel(filename, sheet = "breastfeeding"))
    
  #...................................      
  ## Read in or set other parameters

    # Prepare dataframe of runs and corresponding random values between [0, 1]
      # number of runs
      x <- as.integer(gen_pars[which(gen_pars$parameter == "runs"),"value_gen"])
    
      # dataframe of runs, sorted ascendingly
      runs <- data.frame(run = 1:x, rx = sort(runif(x)) )
      
    # Identify start and end dates of crisis and projection periods
      # crisis start
      date_crisis <- dmy(gen_pars[which(gen_pars$parameter == "date_crisis"), 
        "value_gen"])
      
      # start of projection period
      date_start <- dmy(gen_pars[which(gen_pars$parameter == "date_start"), 
        "value_gen"])
      
      # start of subperiod 2
      date_mid <- dmy(gen_pars[which(gen_pars$parameter == "date_mid"), 
        "value_gen"])      
      
      # end of projection period
      date_end <- dmy(gen_pars[which(gen_pars$parameter == "date_end"), 
        "value_gen"])
    
    # Identify number of months to date
    months_todate <- as.integer((date_start - date_crisis) / 30.41)
    
    # Identify subperiods
    subperiods <- unique(scenario_pars$period)
    names(subperiods) <- paste("subperiod", 1:length(subperiods), sep = "")
    
    # Identify scenarios
    scenarios <- c("ceasefire", "status quo", "escalation")
    
    # Identify age groups
    ages <- grep("value_a", colnames(gen_pars), value = TRUE)
    ages <- gsub("value_a", "", ages)
    
    # Identify target (recommended) caloric intake per day
      # generic
      intake_target <- as.integer(gen_pars[
        which(gen_pars$parameter == "intake_target"),"value_gen"])
    
      # adjusted for Gaza
      intake_target_adj <- as.integer(gen_pars[
        which(gen_pars$parameter == "intake_target_adj"),"value_gen"])
      
    # Identify total population of Gaza
    pop <- as.integer(gen_pars[
      which(gen_pars$parameter == "pop"),"value_gen"])
    
    # Identify pre-war prevalence of exclusive breastfeeding
    exclusive_bf <- as.numeric(gen_pars[
      which(gen_pars$parameter == "exclusive_bf"),"value_gen"])
    
    
#...............................................................................  
### Preparing datasets for analysis
#...............................................................................
    
  #...................................      
  ## Prepare the 2023- trucking dataset
    
    # Select and rename relevant columns
    df_tr <- df_tr[, c("date", "total_truck", "food_truck")]
    colnames(df_tr) <- c("date", "n_trucks", "n_trucks_food")
    
    # Fix date formats
    df_tr$date <- dmy(df_tr$date)
    

#...............................................................................  
### ENDS
#...............................................................................
     