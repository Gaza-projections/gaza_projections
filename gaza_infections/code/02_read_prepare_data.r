#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO READ DATASETS AND PARAMETERS, AND CALL OTHER SCRIPTS  ---- ##
#...............................................................................

 
#...............................................................................  
### Reading in required parameters
#...............................................................................

  #...................................      
  ## Read parameters from main parameters file

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_infections_parameters.xlsx", sep="")
    
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

    # Read endemic (stable-transmission) infection parameters
    endemic_pars <- data.frame(readxl::read_excel(filename, 
      sheet = "endemic_parameters"))

  #...................................      
  ## Read in or set other parameters

    # Prepare dataframe of runs and corresponding random values between [0, 1]
      # number of runs
      x <- as.integer(gen_pars[which(gen_pars$parameter == "runs"),"value_gen"])
    
      # dataframe of runs with two random values: one for transmissibility
        # parameters, one for probability of disease and CFR
      runs <- data.frame(run = 1:x, rx_r0 = runif(x), rx_cfr = runif(x) )
     
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
    
    # Identify whether we should include expert weights in the SEE distributions
    expert_wt <- gen_pars[which(gen_pars$parameter == "expert_wt"), "value_gen"]
    
    # Identify subperiods
    subperiods <- c("months 1 to 3", "months 4 to 6")
    
    # Identify scenarios
    scenarios <- c("ceasefire", "status quo", "escalation")
    
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
    
    # Identify exemplar diseases and diseases they represent       
    exemplars <- diseases[which(diseases$exemplar == "Y"), "disease"]
    list_exemplars <- list()
    for (e in exemplars) {
      x <- diseases[which(diseases$disease == e), "route"]
      list_exemplars[[e]] <- diseases[which(diseases$route == x), "disease"]
    }
    

#...............................................................................  
### Reading in and preparing expert elicitation data
#...............................................................................

  #...................................      
  ## Read in and and select data

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_infections_see_data.xlsx", sep="")
    
    # Read data
    see <- data.frame(readxl::read_excel(filename, sheet = "see_data"))
    
    # Select only data for current projections
    see <- subset(see, date_start == date_start)

  #...................................      
  ## Manage data and compute expert weights

    # Rescale the expert answers from percent to proportions
    x <- which(see$parameter %in% c("pu", "cfr"))
    see[x, c("value10", "value50", "value90")] <- 
      see[x, c("value10", "value50", "value90")] / 100
    
    # Compute experts' scores and performance weights, and add them to dataset
    experts <- f_score()
    see <- merge(see, experts[, c("expert", "wt")], by = "expert", all.x = TRUE)
    
    # Restrict dataset to columns and rows needed henceforth
    see <- subset(see, parameter != "cal")
    see <- see[, c("expert", "disease", "subperiod", "scenario", "parameter",
      "value10", "value50", "value90", "wt")]
   
    # Output experts' weights
    write.csv(experts, paste(dir_path, "/outputs/out_see_experts_wts.csv",
      sep = ""), row.names = FALSE)
    
    # If we do not want to take into account experts' weights, set equal weights
    if (expert_wt == "no") {
      see$wt <- 1 / nrow(experts)
    }
    
  #...................................      
  ## Compute empirical cumulative distributions for each question

    # Specify distribution support (min, max for each parameter)
    see$min <- 0 # default is 0 to 1 (for pu)
    see$max <- 1
    for (e in exemplars) {
      # R0
      see[which(see$parameter == "r0" & see$disease == e), "min"] <-
        epidemic_pars[which(epidemic_pars$parameter == "r0_min" & 
        epidemic_pars$disease == e), "value_gen"]
      see[which(see$parameter == "r0" & see$disease == e), "max"] <-
        epidemic_pars[which(epidemic_pars$parameter == "r0_max" & 
        epidemic_pars$disease == e), "value_gen"]

      # CFR
      see[which(see$parameter == "cfr" & see$disease == e), "min"] <-
        epidemic_pars[which(epidemic_pars$parameter == "cfr_min" & 
        epidemic_pars$disease == e), "value_gen"]
      see[which(see$parameter == "cfr" & see$disease == e), "max"] <-
        epidemic_pars[which(epidemic_pars$parameter == "cfr_max" & 
        epidemic_pars$disease == e), "value_gen"]
    }

    # Linearly extrapolate expert quantile distributions to each 0.025% of
      # parameter support
    for (i in 1:nrow(see)) {
      
      # come up with x-axis (parameter value) intervals
      x_out <- seq(see[i, "min"], see[i, "max"], 
        by = (see[i, "max"] - see[i, "min"]) / 25)
      see[i, paste("x", 1:length(x_out), sep = "_")] <- x_out
      
      # linearly extrapolate probabilities to each x value
      y <- approx(y = c(0.00, 0.10, 0.50, 0.90, 1.00),
        x = see[i, c("min", "value10", "value50", "value90", "max")],
        xout <- x_out, ties = "ordered", yright = 2, yleft = 2 )$y
      see[i, paste("pcum", 1:length(x_out), sep = "_")] <- y
    }
    
    # Compute mean distributions across experts, weighted for experts' weight
      # weighted cumulative probability distributions
      see_wt <- see
      see_wt[, grep("pcum_", colnames(see))] <- 
        see_wt[, grep("pcum_", colnames(see))] * see_wt$wt
      cols <- c("disease", "subperiod", "scenario", "parameter")
      see_wt <- aggregate(see_wt[, grep("pcum_", colnames(see_wt))],
        by = see_wt[, cols], FUN = sum)
    
      # add x-axis values
      x <- aggregate(see[, grep("x_", colnames(see))],
        by = see[, cols], FUN = mean)
      see_wt <- merge(x, see_wt, by = cols)
      see_wt$expert = "all"
      
      # add mean distributions to expert-specific ones
      x <- c(grep("x_", colnames(see), value = TRUE),
        grep("pcum_", colnames(see), value = TRUE))
      see <- rbind(see[, c("expert", cols, x)], see_wt[, c("expert", cols, x)] )
      see$scenario <- factor(see$scenario, levels = scenarios)
      
######################      
      # modify r0 for measles to override SEE
# NOTE: NEED TO RECTIFY DURING NEXT ITERATION
      x <- which(see$disease == "measles" & see$parameter == "r0" & 
        see$expert == "all")
      cols1 <- grep("x_", colnames(see))
      cols2 <- grep("pcum_", colnames(see))
      see[x[1], cols2]<-punif(as.numeric(see[x[1], cols1]), min = 16, max = 20)
      see[x[2], cols2]<-punif(as.numeric(see[x[2], cols1]), min = 24, max = 28)
      see[x[3], cols2]<-punif(as.numeric(see[x[3], cols1]), min = 20, max = 24)
      see[x[4], cols2]<-punif(as.numeric(see[x[4], cols1]), min = 18, max = 22)
      see[x[5], cols2]<-punif(as.numeric(see[x[5], cols1]), min = 26, max = 30)
      see[x[6], cols2]<-punif(as.numeric(see[x[6], cols1]), min = 22, max = 26)
######################
      
      # save output
      write_rds(see, paste(dir_path, 'inputs/',"see_distributions.rds", sep=""))
                  
                
#...............................................................................  
### Preparing inputs for SEIR models: susceptibility to infection and disease
#...............................................................................

  #...................................      
  ## Read and prepare modelled susceptibility estimates
    
    # Read susceptibility to infection      
    si_modelled <- read_rds(paste(dir_path, 
      "inputs/immunity_projections/output_susceptible.rds", sep =""))  
    
      # break up last age group into the standard three age groups, add months
      for (i in diseases[which(diseases$immunity_source == "model"),"disease"]){
        for (j in c("ceasefire", "escalation", "status_quo")) {
          x <- data.frame(month = 1:6)
          x[, ages] <- NA
          x[, ages[1:(length(ages) - 2)]] <- si_modelled[[i]][[j]]
          x[, ages[length(ages) - 1]] <- x[, ages[length(ages) - 2]]
          x[, ages[length(ages)]] <- x[, ages[length(ages) - 2]]
          si_modelled[[i]][[j]] <- x
        }
      }

    # Read susceptibility to disease (or actually its inverse)      
    sd_modelled <- read_rds(paste(dir_path, 
      "inputs/immunity_projections/output_immune_disease_only.rds", sep =""))  
    
      # break up last age group into the standard three age groups, add months,
        # take complement (susceptibility rather than immunity)
      for (i in diseases[which(diseases$immunity_source == "model"),"disease"]){
        for (j in c("ceasefire", "escalation", "status_quo")) {
          x <- data.frame(month = 1:6)
          x[, ages] <- NA
          x[, ages[1:(length(ages) - 2)]] <- sd_modelled[[i]][[j]]
          x[, ages[length(ages) - 1]] <- x[, ages[length(ages) - 2]]
          x[, ages[length(ages)]] <- x[, ages[length(ages) - 2]]
          x[, ages] <- 1 - x[, ages]
          sd_modelled[[i]][[j]] <- x
        }
      }
     
             
  #...................................      
  ## Populate proportions susceptible to infection values
    
    # Generate structure of each disease-scenario dataframe
    si <- data.frame(month = 1:6)
    si[, ages] <- 0
    
    # Nested list that will hold all of them
    si_list <- list()
    
    # For each disease...
    for (i in diseases$disease) {
      
      # if disease immunity is modelled, grab values from model output...
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          if (j == "status quo") {si_list[[i]][[j]] <- 
            si_modelled[[i]][["status_quo"]]}
          if (j != "status quo") {si_list[[i]][[j]] <- 
            si_modelled[[i]][[j]]}
        }
      }
      
      # if instead disease immunity is assumed, single value applies to all
        # scenarios and months...
      if (i %in% 
          diseases[which(diseases$immunity_source == "assumed"), "disease"]) {
        
        # grab values from assumptions table
        x <- immunity_assumptions[which(immunity_assumptions$parameter == "si" &
          immunity_assumptions$disease == i), 
          grep("value_a", colnames(immunity_assumptions))]
        
        # assign them to each scenario and month
        for (k in 1: nrow(si)) {
          si[k, ages] <- x
        }
        for (j in scenarios) {
          si_list[[i]][[j]] <- si
        }
      }
    }  
    
    
  #...................................      
  ## Populate proportions susceptible against severe disease
    
    # Generate structure of each disease-scenario dataframe
    sd <- data.frame(month = 1:6)
    sd[, ages] <- 0
    
    # Nested list that will hold all of them
    sd_list <- list()
    
    # For each disease...
    for (i in diseases$disease) {
      
      # if disease immunity is modelled, grab values from model output...
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          if (j == "status quo") {sd_list[[i]][[j]] <- 
            sd_modelled[[i]][["status_quo"]]}
          if (j != "status quo") {sd_list[[i]][[j]] <- 
            sd_modelled[[i]][[j]]}
        }
      }
      
      # if instead disease immunity is assumed, single value of 0 applies to all
        # scenarios and months...
      if (i %in% 
          diseases[which(diseases$immunity_source == "assumed"), "disease"]) {
        # grab values from assumptions table
        x <- immunity_assumptions[which(immunity_assumptions$parameter == "sd" &
          immunity_assumptions$disease == i), 
          grep("value_a", colnames(immunity_assumptions))]
        
        # assign them to each scenario and month
        for (k in 1: nrow(sd)) {
          sd[k, ages] <- x
        }
        for (j in scenarios) {
          sd_list[[i]][[j]] <- sd
        }
      }
    }  
    
    
#...............................................................................  
### Preparing other inputs for SEIR models: contact matrix, timeline, population
#...............................................................................
        
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
      scale_fill_gradient(low = "white", high = palette_gen[7]) +
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
      subperiod = "months 1 to 3")
    timeline[which(timeline$date >= date_mid), 
      "subperiod"] <- "months 4 to 6"
    
    timeline$month <- timeline$date - lubridate::day(min(timeline$date)) + 1
    timeline$month <- lubridate::month(timeline$month)
    timeline$month <- timeline$month - min(timeline$month) + 1
      
      # identify start and end times of each subperiod
      time_periods <- c(min(timeline$time), 
        max(timeline[which(timeline$subperiod == "months 1 to 3"), "time"]),
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
        

    
#...............................................................................  
### Reading and preparing data for endemic infection  model
#...............................................................................
        
  #...................................      
  ## Read in endemic data
    
    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_infections_endemic_data.xlsx", sep="")
    
    # Read data
    ende <- data.frame(readxl::read_excel(filename, sheet = "endemic_data"))
    

  #...................................      
  ## Model annual deaths as a function of COVID-19

    # Select data to model
    df <- ende[complete.cases(ende), c("year", "d", "pop", "prop_covid")]
    
    # Fit a neg-bin model to the data, with COVID-19 proportion as covariate    
    fit <- glm.nb(formula = d ~ prop_covid + offset(log(pop)), data = df)
    
    # Predict point estimate and confidence interval
    df$pred <- exp(predict(fit))
    x <- unlist(predict(fit, se.fit = TRUE)[2])
    df$lci <- exp(predict(fit) - 1.96 * x)   
    df$uci <- exp(predict(fit) + 1.96 * x)   
    
    # Plot data and modelled fit
      
      # prepare data for plotting
      df2 <-df
      df2$d_covid <- as.integer(df2$d * df2$prop_covid)
      df2$d_other <- df2$d - df2$d_covid
      df2 <- reshape(direction = "long", sep = "_",
        data = df2[, c("year", "d_covid", "d_other")], varying = 
        c("d_covid", "d_other"), timevar = "cause", 
        idvar = "year")
      df2$cause <- ifelse(df2$cause == "covid", "COVID-19", "other")
      
      # plot
      ggplot() +
        geom_bar(data = df2, aes(x = year, y = d, fill = cause, colour = cause), 
          alpha = 0.7, stat = "identity", position = "stack") +
        geom_point(data = df, aes(x = year, y = pred), colour = palette_gen[5])+
        geom_errorbar(data = df, aes(ymin = lci, ymax = uci, x = year), 
          width = 0.3, colour = palette_gen[5]) +
        scale_fill_manual(values = palette_gen[c(14, 10)]) +
        scale_colour_manual(values = palette_gen[c(14, 10)]) +
        theme_bw() +
        scale_x_continuous("year", breaks = 2016:2022) +
        scale_y_continuous("number of deaths due to endemic infections",
          breaks = seq(0, 1400, by = 200), expand = c(0, 40) ) +
        theme(panel.grid.major.x = element_blank(), legend.position = "top")
      
    # Save plot and fit
    ggsave(paste(dir_path, 'outputs/' , "fit_model_endemic_deaths.png", sep=""),
      dpi = "print", units = "cm", width = 20, height = 12)
    write_rds(fit, paste(dir_path, 'inputs/' , "fit_model_endemic_deaths.rds", 
      sep=""))
    
  #...................................      
  ## Prepare other inputs
    
    # Seasonality data
      # select data
      w <- endemic_pars[which(endemic_pars$parameter == "w"), c("disease",
        grep("value_m", colnames(endemic_pars), value = TRUE) )]
      colnames(w) <- c("disease", month.abb)
    
      # reshape long
      w <- reshape(data = w, direction = "long", v.names = "w", 
        times = month.abb,
        varying = month.abb, timevar = "month", idvar = "disease")
      
    # Proportional mortality data
    prop_d <- endemic_pars[which(endemic_pars$parameter == "prop_d"),
      c("disease", "value_gen")]
    
    # Age distribution of deaths, by disease
      # select data
      prop_age <- endemic_pars[which(endemic_pars$parameter == "prop_age"), 
        c("disease", grep("value_a", colnames(endemic_pars), value = TRUE) )]
      colnames(prop_age) <- gsub("value_a", "", colnames(prop_age))
    
      # reshape long
      prop_age <- reshape(data = prop_age, direction = "long", 
        v.names = "prop_age", times = ages,
        varying = ages, timevar = "age", idvar = "disease")
      
    # Daily timeline with age groups and diseases
      # needs to go back to Jan 2023 to take care of seasonality
    x <- ymd(paste("2023", "01", "01", sep = "-")) : 
      ymd(paste("2024", "12", "31", sep = "-"))
    timeline_ende <- expand.grid(date = as.Date(x), age= ages,
      disease = diseases[which(diseases$category == "endemic"), "disease"])
      
      # add year and month
      timeline_ende$year <- year(timeline_ende$date)  
      timeline_ende$month <- month(timeline_ende$date)
      timeline_ende$month <- sapply(timeline_ende$month, 
        function(x) {month.abb[x]})
    
      # add subperiod
      timeline_ende$subperiod <- "pre-war"
      timeline_ende[which(timeline_ende$date %in% 
        date_crisis : (date_start - 1)), "subperiod"] <- "to date"
      timeline_ende[which(timeline_ende$date %in% 
        date_start : (date_mid - 1)), "subperiod"] <- "months 1 to 3"
      timeline_ende[which(timeline_ende$date %in% 
        date_mid : date_end), "subperiod"] <- "months 4 to 6"
      timeline_ende[which(timeline_ende$date > date_end), "subperiod"] <- "post"
      
      # add population    
      timeline_ende <- merge(timeline_ende, ende[, c("year", "pop")],
        by = "year", all.x = TRUE)
      
      # add route of transmission
      timeline_ende <- merge(timeline_ende, diseases[, c("disease", "route")],
        by = "disease", all.x = TRUE)
      
      # add age distribution of deaths
      timeline_ende <- merge(timeline_ende, prop_age, by = c("disease", "age"),
        all.x = TRUE)
        
#...............................................................................
### ENDS
#...............................................................................

