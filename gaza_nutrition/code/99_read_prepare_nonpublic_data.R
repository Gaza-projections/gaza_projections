#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO READ AND PREPARE DATASETS THAT WILL NOT BE PUBLIC  ----- ##
#...............................................................................


#...............................................................................  
### Converting the 2019 growth monitoring individual dataset into an aggregate
#...............................................................................

  #...................................      
  ## Read data from 2019 [g]rowth [m]onitoring of children in Gaza
  
    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_GM_UNRWA_reduced_2019_last.dta", sep="")
    
    # Read dataframe
    df_gm <- data.frame(haven::read_dta(filename))
  

  #...................................      
  ## Clean the individual dataset
  
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


  #...................................      
  ## Aggregate the dataset by age, gender, height and weight
    
    # Generate weight variable
    df_gm$wt <- 1
    
    # Generate integer age in months
    df_gm$age <- round(df_gm$age_mths, digits = 0)
    
    # Aggregate
    df_gm_agg <- aggregate(list(wt = df_gm$wt), 
      by = df_gm[, c("sex", "age", "weight", "height")], FUN = sum)
  
    # Compute weight variable and save
    df_gm_agg$wt <- df_gm_agg$wt / sum(df_gm_agg$wt)
    write_rds(df_gm_agg,
      paste(dir_path, "inputs/", "gm_anthro_2019_agg.rds", sep=""))

    
#...............................................................................  
### Preparing the 2020 adult NCD survey dataset
#...............................................................................

  #...................................      
  ## Read and recode variables

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_survey2020_kcal_bmi_wt_ht_wc.dta", sep="")
  
    # Read dataframe
    df_ad <- data.frame(haven::read_dta(filename))
    
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
      
      # unfactor age category
      df_ad$age_cat <- as.character(df_ad$age_cat)

    # Recode gender
    df_ad$gender <- ifelse(df_ad$gender == 1, "m", "f")

  # #...................................      
  # ## Add variables to survey dataset
  # 
  #   # Estimate baseline fat mass in adults surveyed
  #   df_ad$f_start <- apply(df_ad, 1, f_fat)
  # 
  #   # Resting metabolic rate per day
  #   df_ad$rmr <- NA
  #   
  #   # Weight per day of adults (set to starting value at the beginning)
  #   df_ad$wt_now <- df_ad$weight
  #   
  #   # Change in intake per day
  #   df_ad$change_intake <- NA
  #   
    
  #...................................      
  ## Aggregate dataset: means by age category, gender
    
    # Generate a weight for each individual (1/n), used later for averaging
    df_ad$svy_wt <- 1/nrow(df_ad)
    
    # Aggregate all variables by age category and gender (means)
    df_ad_agg <- aggregate(df_ad[, c("age", "weight", "height",
      "intake_baseline")], by = df_ad[, c("gender", "age_cat")], FUN = mean)
    
    # Add category weights
    x <- aggregate(list(svy_wt = df_ad$svy_wt), 
      by = df_ad[, c("gender", "age_cat")], FUN = sum)
    df_ad_agg <- merge(df_ad_agg, x, by = c("age_cat", "gender"), all.x = TRUE)
    
    # Add other needed variables
      # estimate baseline fat mass in adults surveyed
      df_ad_agg$f_start <- apply(df_ad_agg, 1, f_fat)
  
      # resting metabolic rate per day
      df_ad_agg$rmr <- NA
      
      # weight per day of adults (set to starting value at the beginning)
      df_ad_agg$wt_now <- df_ad_agg$weight
      
      # change in intake per day
      df_ad_agg$change_intake <- NA
    
    # Write database
    write.csv(df_ad_agg, paste(dir_path,
      "inputs/gaza_survey2020_kcal_bmi_agg.csv", sep = ""), row.names = FALSE)

    
#...............................................................................  
### Preparing data on older weight loss studies
#...............................................................................

  #...................................      
  ## Read data on older [w]eigh[t] loss studies

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "Literature on calorie reduction_Extraction 01-15.xlsx", sep="")
    
    # Read dataframe
    df_wt <- data.frame(readxl::read_excel(filename, sheet = "for_r"))
    
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

    
#...............................................................................  
### ENDS
#...............................................................................
    