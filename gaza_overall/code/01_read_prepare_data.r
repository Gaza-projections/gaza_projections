#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - OVERALL ++++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO READ DATASETS AND PARAMETERS, AND PREPARE FOR ANALYSIS  --- ##
#...............................................................................

                                # LSHTM (January 2024)
                                # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Reading in required parameters
#...............................................................................

  #...................................      
  ## Read parameters from main parameters file

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_overall_parameters.xlsx", sep="")
    
    # Read general parameters
    gen_pars <- data.frame(readxl::read_excel(filename, sheet = "general"))

    # Read population data
    pop <- data.frame(readxl::read_excel(filename, sheet = "pop"))
    
    # Read list of diseases
    diseases <- data.frame(readxl::read_excel(filename,sheet = "list_diseases"))
    
    
  # #...................................      
  # ## Read in or set other parameters
  # 
  #   # Prepare dataframe of runs and corresponding random values between [0, 1]
  #     # number of runs
  #     x <- as.integer(gen_pars[which(gen_pars$parameter == "runs"),"value_gen"])
  #   
  #     # dataframe of runs, sorted ascendingly
  #     runs <- data.frame(run = 1:x, rx = sort(runif(x)) )
     
  #...................................      
  ## Read in or set other parameters

    # Identify important dates
    date_crisis <- dmy(gen_pars[which(gen_pars$parameter == "date_crisis"), 
      "value_gen"])
    date_start <- dmy(gen_pars[which(gen_pars$parameter == "date_start"), 
      "value_gen"])
    date_mid <- dmy(gen_pars[which(gen_pars$parameter == "date_mid"), 
      "value_gen"])
    date_end <- dmy(gen_pars[which(gen_pars$parameter == "date_end"), 
      "value_gen"])
    
    # Identify subperiods
    subperiods <- c("months 1 to 3", "months 4 to 6")
    
    # Identify scenarios
    scenarios <- c("ceasefire", "status quo", "escalation")
    
    # Identify epidemic diseases
    epid_dis <- diseases[which(diseases$category == "epidemic"), "disease"]
    
    # Identify age groups
    ages_fine <- pop$age # groups we may have in the data
    ages <- c("0mo", "1 to 11mo", "12 to 59mo", "5 to 9yo", "10 to 14yo", 
      "15 to 19yo", "20 to 29yo", "30 to 39yo", "40 to 49yo", "50 to 59yo", 
      "60 to 69yo", "70 to 79yo", "80 to 100yo") # groups we want

    # Correspondence between age groups
    age_matches <- data.frame(age = ages_fine, age_target = 
      c("0mo", "1 to 11mo", "12 to 59mo", "5 to 9yo", "10 to 14yo", 
      "15 to 19yo", "20 to 29yo", "20 to 29yo", "30 to 39yo", "30 to 39yo", 
      "40 to 49yo", "40 to 49yo", "50 to 59yo", "50 to 59yo",
      "60 to 69yo", "60 to 69yo", "70 to 79yo",  "70 to 79yo", "80 to 100yo")
    )

#...............................................................................  
### Reading in and preparing dataset with outputs of all disease modules
#...............................................................................

  #...................................      
  ## Read in and and select data

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_overall_data.xlsx", sep="")
    
    # Read data
    df <- data.frame(readxl::read_excel(filename))
    
  #...................................      
  ## Clean data
    
    # Change column names
    colnames(df) <- gsub("d_", "", colnames(df))
    
    # Clean disease modules
    table(df$theme)
    df$module <- df$theme
    df$module <- ifelse(df$module == "infection", "infections - endemic",
      df$module)
    df$module <- ifelse(df$disease %in% epid_dis, "infections - epidemic",
      df$module)
    table(df$module, df$theme)
    
    df$module <- ifelse(df$module == "NCD", "NCDs", df$module)
    table(df$module)    

    # Clean disease
    sort(unique(df$disease))
    df$disease <- ifelse(df$disease == "trauma", "traumatic injury", 
      df$disease)
    sort(unique(df$disease))
        
    # Clean scenario
    table(df$scenario)

    # Clean subperiod
    table(df$subperiod)
    
    # Clean age
    sort(unique(df$age))
    df$age <- ifelse(df$age == "1-11 mo", "1 to 11mo", df$age)
    df$age <- ifelse(df$age == "1-4", "12 to 59mo", df$age)
    df$age <- ifelse(df$age == "10-14", "10 to 14yo", df$age)
    df$age <- ifelse(df$age == "15-19", "15 to 19yo", df$age)
    df$age <- ifelse(df$age == "20-24", "20 to 24yo", df$age)
    df$age <- ifelse(df$age == "25-29", "25 to 29yo", df$age)
    df$age <- ifelse(df$age == "30-34", "30 to 34yo", df$age)
    df$age <- ifelse(df$age == "35-39", "35 to 39yo", df$age)
    df$age <- ifelse(df$age == "40-44", "40 to 44yo", df$age)
    df$age <- ifelse(df$age == "45-49", "45 to 49yo", df$age)
    df$age <- ifelse(df$age == "5-9", "5 to 9yo", df$age)
    df$age <- ifelse(df$age == "50-54", "50 to 54yo", df$age)
    df$age <- ifelse(df$age == "55-59", "55 to 59yo", df$age)
    df$age <- ifelse(df$age == "60-64", "60 to 64yo", df$age)
    df$age <- ifelse(df$age == "65-69", "65 to 69yo", df$age)
    df$age <- ifelse(df$age == "70-74", "70 to 74yo", df$age)
    df$age <- ifelse(df$age == "75-79", "75 to 79yo", df$age)
    df$age <- ifelse(df$age == ">80", "80 to 100yo", df$age)    
    df$age <- gsub("  ", " ", df$age)
    sort(unique(df$age))
    base::setdiff(sort(unique(df$age)), ages) # outstanding
    table(df[, c("age", "module")])
    
    # Reduce columns
    cols <- c("mean", "lci", "uci")
    df <-df[, c("scenario", "module", "disease", "subperiod", "age", 
      paste("base", cols, sep = "_"), paste("crisis", cols, sep = "_"),
      paste("excess", cols, sep = "_"))]
 
    # Set NA values to 0 when base is missing
    x <- grep("base", colnames(df))
    df[, x] <- na.replace(df[, x], 0)
    
    # Set crisis = excess when crisis is missing (injuries, epidemics)
    x <- which(df$module %in% c("injuries", "infections - epidemic"))
    df[x, grep("crisis", colnames(df))] <-  df[x, grep("excess", colnames(df))]   
    
  #...................................      
  ## Distribute some age groups into fine age strata
    
    # # <1 yo
    # x <- pop[which(pop$age %in% c("0mo", "1 to 11mo")), "total"]
    # x <- proportions(x)
    # tofix <- subset(df, age == "<1")
    # tofix1 <- tofix
    # tofix1[, c("mean", "lci", "uci")] <- tofix[, c("mean", "lci", "uci")] * x[1]
    # tofix1$age <- "0mo"
    # tofix2 <- tofix
    # tofix2[, c("mean", "lci", "uci")] <- tofix[, c("mean", "lci", "uci")] * x[2]
    # tofix2$age <- "1 to 11mo"    
    # tofix <- rbind(tofix1, tofix2)    
    # df <- subset(df, age != "<1")
    # df <- rbind(df, tofix)
    
    # 15-49 (females only)
    tofix <- subset(df, age == "15-49")
    x <- pop[which(pop$age %in% c("15 to 19yo", "20 to 24yo", "25 to 29yo",
      "30 to 34yo", "35 to 39yo", "40 to 44yo", "45 to 49yo")), "female"]
    x <- proportions(x)
    names(x) <- c("15 to 19yo", "20 to 24yo", "25 to 29yo",
      "30 to 34yo", "35 to 39yo", "40 to 44yo", "45 to 49yo")
    
    tofix_out <- data.frame()
    for (i in c("15 to 19yo", "20 to 24yo", "25 to 29yo",
      "30 to 34yo", "35 to 39yo", "40 to 44yo", "45 to 49yo")) {
    
       # attribute this age group
      tofix_part <- tofix
      for (j in c("base", "crisis", "excess")) {
        tofix_part[, paste(j, cols, sep = "_")] <- 
          tofix_part[, paste(j, cols, sep = "_")] * x[[i]]
      }
      tofix_part$age <- i
      tofix_out <- rbind(tofix_out, tofix_part)
    }
    df <- subset(df, age != "15-49")
    df <- rbind(df, tofix_out)
    table(df[, c("age", "module")])
    
  #...................................      
  ## Distribute some age groups into desired age strata
    
    # Age groups to fix
    base::setdiff(sort(unique(df$age)), ages) # outstanding      
    
    # Correspondence between age groups
    df <- merge(df, age_matches, by = "age", all.x = TRUE)
    table(df[, c("age", "age_target")])
    df$age <- ifelse(is.na(df$age_target), df$age, df$age_target)

    # Aggregate
    df <- aggregate(df[, c(paste("base", cols, sep = "_"), 
      paste("crisis", cols, sep = "_"),
      paste("excess", cols, sep = "_"))], 
      by = df[, c("module", "disease", "scenario", "subperiod", "age")], 
      FUN = sum)
    
    # Check
    table(df$age)
    table(df[, c("disease", "scenario")])

  #...................................      
  ## Rectangularise dataset: make sure each disease has same age groups,
    # scenarios
    
    # Rectangularise  
    x <- expand.grid(disease = unique(df$disease),
      scenario = unique(df$scenario), subperiod = subperiods, age = ages)    
    df <- merge(x, df, by = c("disease", "scenario", "subperiod", "age"),
      all.x = TRUE)        
    table(df[, c("disease", "scenario")])
           
    # Specify that deaths are 0 when blank (= neonatal, maternal)
    for (i in cols) {
      df[, grep(i, colnames(df))] <- na.replace(df[, grep(i, colnames(df))], 0)}

    # Add missing module values
    x <- unique(df[, c("disease", "module")])
    x <- na.omit(x)
    df <- subset(df, select = -module)
    df <- merge(df, x, by = "disease", all.x = TRUE)    
    
    # Add population
      # first aggregate population
      pop <- merge(pop, age_matches, by = "age", all.x = TRUE)
      pop <- aggregate(pop[, c("male", "female", "total")], by = 
        list(age = pop$age_target), FUN = sum)
      
      # then add
      df <- merge(df, pop, by = "age", all.x = TRUE)
    
    
#...............................................................................
### ENDS
#...............................................................................

