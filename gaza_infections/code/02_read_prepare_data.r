#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO READ DATASETS AND PARAMETERS, AND CALL OTHER SCRIPTS  ---- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Reading in required parameters and data
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

    # Read data on past epidemiology
    past_epi <- data.frame(readxl::read_excel(filename, 
      sheet = "past_epidemiology"))

  #...................................      
  ## Read in or set other parameters

    # Prepare dataframe of runs and corresponding random values between [0, 1]
      # number of runs
      x <- as.integer(gen_pars[which(gen_pars$parameter == "runs"),"value_gen"])
    
      # dataframe of runs, sorted ascendingly
      runs <- data.frame(run = 1:x, rx = sort(runif(x)) )
     
  #...................................      
  ## Read in or set other parameters

    # Identify start and end dates of projection period
    date_start <- dmy(gen_pars[which(gen_pars$parameter == "date_start"), 
      "value_gen"])
    date_end <- dmy(gen_pars[which(gen_pars$parameter == "date_end"), 
      "value_gen"])
    
    # Identify subperiods
    subperiods <- c("subperiod1", "subperiod2")
    names(subperiods) <- c("months 1 to 3", "months 4 to 6")
    
    # Identify scenarios
    scenarios <- c("status quo", "escalation", "ceasefire")
    
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
### Preparing inputs for SEIR models
#...............................................................................

#######################    
  #...................................      
  ## Generate dummy data - ONLY FOR CODE TESTING
  source(paste(dir_path, "code/99_generate_dummy_data.R", sep =""))
######################
    
  #...................................      
  ## Populate proportions susceptible to infection values
    
    # Generate structure of each disease-scenario dataframe
    si <- data.frame(month = 1:6)
    si[, ages] <- 0
    
    # Nested list that will hold all of them
    si_list <- list()
    
    # For each disease...
    for (i in diseases$disease) {
      
      # if disease immunity is modelled, grab values from model output..
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          si_list[[i]][[j]] <- si_modelled[[i]][[j]]
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
      
      # if disease immunity is modelled, grab values from model output..
      if (i %in% 
          diseases[which(diseases$immunity_source == "model"), "disease"]) {
        for (j in scenarios) {
          sd_list[[i]][[j]] <- sd_modelled[[i]][[j]]
        }
      }
      
      # if instead disease immunity is assumed, single value of 0 applies to all
        # scenarios and months...
      if (i %in% 
          diseases[which(diseases$immunity_source == "assumed"), "disease"]) {
        for (j in scenarios) {
          sd_list[[i]][[j]] <- sd
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
    
    timeline$month <- timeline$date - lubridate::day(min(timeline$date)) + 1
    timeline$month <- lubridate::month(timeline$month)
    timeline$month <- timeline$month - min(timeline$month) + 1
      
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
        

    
#...............................................................................
### ENDS
#...............................................................................

