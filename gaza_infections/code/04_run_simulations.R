#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO RUN AND ANALYSE SIMULATIONS FOR INFECTIONS MODEL  ------ ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Initialising objects needed for the simulation
#...............................................................................

  #...................................      
  ## Initialise timeline with age groups and population
    
    # Initialise a timeline object
    timeline_sim <- timeline
    
    # Add age groups and convert to long
    timeline_sim[, ages] <- NA
    timeline_sim <- reshape(timeline_sim, direction = "long",
        varying = list(ages),
        timevar = "age",
        times = ages
      )
    timeline_sim <- timeline_sim[, ! colnames(timeline_sim) %in% c("0mo", "id")]

    # Add age categories used by the model
    age <- data.frame(age = ages, demography_group = names(demography_vector))
    timeline_sim <- merge(timeline_sim, age, by = "age", all.x = TRUE)

    # Add population
    pop <- data.frame(demography_group = names(demography_vector),
      pop = demography_vector)
    timeline_sim <- merge(timeline_sim, pop, by = "demography_group", 
      all.x = TRUE)
    
    
#...............................................................................   
### Running epidemic (SEIR model simulations)
#...............................................................................

# (run loop starts here)    
for (run_i in 1:max(runs$run)) {

  #...................................      
  ## Preparatory steps

    # Update progress bar
    setTxtProgressBar(pb, run_i)

  
  #...................................      
  ## For each scenario...
  for (i in scenarios) {
    
    # Get parameter values for this run and scenario
    sim_pars <- sim[[run_i]][[i]]
    
    # For each epidemic-prone disease...
    for (u in diseases_epid) {

      # start fresh timeline
      timeline_ui <- timeline_sim
      
      # get parameter values for this run, scenario and disease
      sim_pars_u <- subset(sim_pars, disease == u)      
      
      # add parameters
      
        # Probability of an epidemic
        pu <- sim_pars_u[which(sim_pars_u$disease == u &
          sim_pars_u$subperiod == "overall" & sim_pars_u$parameter == "pu"), 
          "value_gen"]  
        timeline_ui$pu <- pu
        
        # R0
        r0 <- sim_pars_u[which(sim_pars_u$disease == u & 
          sim_pars_u$parameter == "r0"), c("subperiod", "value_gen")]
        colnames(r0) <- c("subperiod", "r0")
        timeline_ui <- merge(timeline_ui, r0, by = "subperiod", all.x = TRUE)
  
        # infectiousness period
        tau <- sim_pars_u[which(sim_pars_u$disease == u & 
          sim_pars_u$parameter == "tau"), c("subperiod", "value_gen")]
        colnames(tau) <- c("subperiod", "tau")
        timeline_ui <- merge(timeline_ui, tau, by = "subperiod", all.x = TRUE)
        
        # pre-infectious period
        pre_tau <- sim_pars_u[which(sim_pars_u$disease == u & 
          sim_pars_u$parameter == "pre_tau"), c("subperiod", "value_gen")]
        colnames(pre_tau) <- c("subperiod", "pre_tau")
        timeline_ui <- merge(timeline_ui, pre_tau, by = "subperiod", all.x = TRUE)
        
        # proportion of symptomatics
        pd <- sim_pars_u[which(sim_pars_u$disease == u & 
          sim_pars_u$parameter == "pd"), c("subperiod", "value_gen")]
        colnames(pd) <- c("subperiod", "pd")
        timeline_ui <- merge(timeline_ui, pd, by = "subperiod", all.x = TRUE)
        
        # CFR values by subperiod and age group
        cfr <- sim_pars_u[which(sim_pars_u$disease == u & 
          sim_pars_u$parameter == "cfr" & sim_pars_u$subperiod %in% subperiods),
          c("subperiod", grep("value_a", colnames(sim_pars_u), value = TRUE))]
        cfr <- reshape(cfr, direction = "long",
          varying = list(colnames(cfr)[colnames(cfr) != "subperiod"]),
          timevar = "age", v.names = "cfr",
          idvar = "subperiod",
          times = colnames(cfr)[colnames(cfr) != "subperiod"]
        )
        cfr$age <- gsub("value_a", "", cfr$age)
        timeline_ui <- merge(timeline_ui, cfr, by = c("subperiod", "age"), 
          all.x = TRUE)
        
        # STARTING proportion susceptible to infection
        si <- si_list[[u]][[i]]
        si <- reshape(si, direction = "long", varying = list(ages),
          timevar = "age", v.names = "si", idvar = "month", times = ages
        )
        timeline_ui <- merge(timeline_ui, si, by = c("month", "age"), 
          all.x = TRUE)
        
        # proportion susceptible to severe disease
        sd <- sd_list[[u]][[i]]
        sd <- reshape(sd, direction = "long", varying = list(ages),
          timevar = "age", v.names = "sd", idvar = "month", times = ages
        )
        timeline_ui <- merge(timeline_ui, sd, by = c("month", "age"), 
          all.x = TRUE)
      
      # run SEIR model
      
          
      # collect results  
        
        
        
    } # (close u - disease loop)
  } # (close i - scenario loop)
} # (close run_i - run loop)
close(pb)    
      
      
