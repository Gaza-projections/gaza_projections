#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO READ DATASETS AND PARAMETERS AND MANAGE DATASETS  ----- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Reading in datasets
#...............................................................................

  #...................................      
  ## Read data on older [w]eigh[t] loss studies

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "Literature on calorie reduction_Extraction 01-15.xlsx", sep="")
    
    # Read dataframe
    df_wt <- data.frame(readxl::read_excel(filename, sheet = "for_r"))

  #...................................      
  ## Read data on [tr]ucks arriving into Gaza

    # Identify file name
    filename <- paste(dir_path, 'inputs/', "gaza_calories.dta", sep="")
    
    # Read dataframe
    df_tr <- data.frame(haven::read_dta(filename))
    
  #...................................      
  ## Read data from a 2020 survey of NCDs, containing [ad]ult BMI and diet intake

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_survey2020_kcal_bmi_wt_ht_wc.dta", sep="")
    
    # Read dataframe
    df_ad <- data.frame(haven::read_dta(filename))
    
    
  #...................................      
  ## Read data from 2019 [g]rowth [m]onitoring of children in Gaza
  
    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_GM_UNRWA_reduced_2019_last.dta", sep="")
    
    # Read dataframe
    df_gm <- data.frame(haven::read_dta(filename))
    

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
    todate_pars <- data.frame(readxl::read_excel(filename, sheet = "to_date"))

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
    months_todate <- max(todate_pars$month)
    
    # Identify subperiods
    subperiods <- unique(scenario_pars$period)
    names(subperiods) <- paste("subperiod", 1:length(subperiods), sep = "")
    
    # Identify scenarios
    scenarios <- unique(scenario_pars$scenario)
    
    # Identify age groups
    ages <- grep("value_a", colnames(gen_pars), value = TRUE)
    ages <- gsub("value_a", "", ages)
    
    # Identify target (recommended) caloric intake per day
    intake_target <- as.integer(gen_pars[
      which(gen_pars$parameter == "intake_target"),"value_gen"])
    
    # Identify total population of Gaza
    pop <- as.integer(gen_pars[
      which(gen_pars$parameter == "pop"),"value_gen"])
    
    
#...............................................................................  
### Preparing datasets for analysis
#...............................................................................
    
  #...................................      
  ## Prepare the dataset of old weight loss studies
    
    # Create a weight
    df_wt$wt <- df_wt$sampsi * 100 / sum(df_wt$sampsi, na.rm = TRUE)

    # Identify special diets
    df_wt$special <- ifelse(df_wt$notes %in% c("intermittent", "NPLC", 
      "high protein group", "NPNC", "HPLC"), TRUE, FALSE)
    
    # Make study ID a string
    df_wt$study_id <- as.character(df_wt$study_id)

    # Monthly weight loss rates
    df_wt$percent_wt_loss_mth <- df_wt$percent_wt_loss / df_wt$duration
    df_wt$percent_fat_loss_mth <- df_wt$percent_fat_loss / df_wt$duration
    df_wt$percent_nonfat_loss_mth <- df_wt$percent_nonfat_loss / df_wt$duration
    
    # Log weight loss rate
    df_wt$percent_wt_loss_log <- log(df_wt$percent_wt_loss + 0.0001)
    df_wt$percent_wt_loss_mth_log <- log(df_wt$percent_wt_loss_mth)

    # Select data for model fitting
    x <- c("study_id", "authors", "percent_wt_loss", "percent_wt_loss_log", 
      "percent_wt_loss_mth", "percent_wt_loss_mth_log",
      "intake_reduction", "bmi_baseline", "duration", "age", "wt", 
      "special")
    df_wt_m <- df_wt[complete.cases(df_wt[, x]), x]
#    df_wt_m <- subset(df_wt_m, special == FALSE)

    
  #...................................      
  ## Prepare the 2023- trucking dataset
    
    # Select and rename relevant columns
    df_tr <- df_tr[, c("date", "total_truck", "food_truck")]
    colnames(df_tr) <- c("date", "n_trucks", "n_trucks_food")
    
    # Fix date formats
    df_tr$date <- dmy(df_tr$date)
    
  #...................................      
  ## Prepare the 2020 adult NCD survey dataset
    
    # Select necessary variables
    df_ad <- df_ad[, c("DEM04", "DEM06", "HT_avg", "WT_avg", "BMI", "energy")]

    # Rename variables
    colnames(df_ad) <- c("gender", "age", "height", "weight", "bmi_baseline",
      "intake_baseline")
    
    # Check missingness and delete missing records
    prop.table(table(complete.cases(df_ad)))
    df_ad <- df_ad[complete.cases(df_ad), ]    
        
    # Add age categories
    df_ad$age_cat <- cut(df_ad$age, breaks = c(40, 50, 60, 70, 120), 
      include.lowest = TRUE, right = FALSE)
    
    # Recode gender
    df_ad$gender <- ifelse(df_ad$gender == 1, "m", "f")
    
  #...................................      
  ## Prepare the 2019 growth monitoring child dataset
  
    # Keep and rename needed columns
    df_gm <- df_gm[, c("sex", "age_m", "wt", "ht")]
    colnames(df_gm) <- c("sex", "age_mths", "weight", "height")
    
    # Check for missingness and remove missing observations
    prop.table(table(complete.cases(df_gm)))
    df_gm <- df_gm[complete.cases(df_gm), ]
    
    # Check for ages outside limits
    range(df_gm$age_mths) # none, all OK
    
    # Recode gender
    df_gm$sex <- ifelse(df_gm$sex == 1, "m", "f")

#...............................................................................  
### ENDS
#...............................................................................
     