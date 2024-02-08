#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO RUN SIMULATIONS FOR EPIDEMIC AND ENDEMIC INFECTIONS ---- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Initialising objects needed for the simulations
#...............................................................................

  #...................................      
  ## Initialise daily timeline with age groups and population (for epidemics)
    
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
  
  #...................................      
  ## Initialise output
    
    # For epidemic infections model  
    out_epid <- data.frame()  

    # For stable-transmission infections model  
    out_ende <- data.frame()  
    
            
#...............................................................................   
### Running epidemic (SEIR model) simulations
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
        timeline_ui <- merge(timeline_ui, pre_tau, by ="subperiod",all.x = TRUE)
        
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
      x <- f_seir(disease_f = u, scenario_f = i, timeline_f = timeline_ui)
          
      # collect results  
      x$run <- run_i
      x$scenario <- i
      x$disease <- u
      out_epid <- rbind(out_epid, x)
        
    } # (close u - disease loop)
  } # (close i - scenario loop)
} # (close run_i - run loop)
close(pb)    
      
  #...................................      
  ## Save raw output
  write_rds(out_epid, paste(dir_path, "outputs/", 
    "out_epid_all_runs.rds", sep=""))


#...............................................................................   
### Running stable-transmission simulations
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
    sim_pars <- sim_pars[which(sim_pars$parameter %in% c("r0_rr", "cfr_rr")), 
      c("disease", "parameter", "subperiod", "value_gen")]

    # Initialise fresh timeline
    timeline_runi_i <- timeline_ende
    
    # Add relative risks
      # transmissibility
      x <- sim_pars[which(sim_pars$parameter == "r0_rr"), 
        c("disease", "subperiod", "value_gen")]
      colnames(x) <- c("disease", "subperiod", "r0_rr")
      timeline_runi_i <- merge(timeline_runi_i, x,
        by = c("disease", "subperiod"), all.x = TRUE)
    
      # CFR
      x <- sim_pars[which(sim_pars$parameter == "cfr_rr"), 
        c("disease", "subperiod", "value_gen")]
      colnames(x) <- c("disease", "subperiod", "cfr_rr")
      timeline_runi_i <- merge(timeline_runi_i, x,
        by = c("disease", "subperiod"), all.x = TRUE)
      
    # Generate a random number of infectious disease deaths by year
    d <- c()
    x <- predict(fit, newdata = 
      ende[which(ende$year %in% unique(timeline_ende$year)), ], se.fit = TRUE)
    for (j in 1:length(x$fit)) 
      {d[j] <- exp(qnorm(runif(1), x$fit[j], sd = x$se.fit[j]))}
    d_y <- data.frame(year = unique(timeline_ende$year), d_y = as.integer(d) )  
    
    # Distribute total deaths into each cause
    d_by <- unique(timeline_ende[, c("year", "disease", "route")])
    d_by <- merge(d_by, d_y, by = "year", all.x = TRUE)
    d_by$d <- NA
    for (j in 1:nrow(d_by)) {
      # COVID-19 deaths
      if (d_by[j, "disease"] == "COVID-19") {
        d_by[j, "d"] <- d_by[j, "d_y"] * 
          ende[which(ende$year == d_by[j, "year"]), "prop_covid"]
      }
      
      # other airborne droplet deaths
      if (d_by[j, "disease"] != "COVID-19" & 
        d_by[j, "route"] == "airborne-droplet") {
        d_by[j, "d"] <- d_by[j, "d_y"] * 
          ende[which(ende$year == d_by[j, "year"]), "prop_airborne"] *
          prop_d[which(prop_d$disease == d_by[j, "disease"]), "value_gen"]
      }
      
      # faecal-oral deaths
      if (d_by[j, "route"] == "faecal-oral") {
        d_by[j, "d"] <- d_by[j, "d_y"] *
          ende[which(ende$year == d_by[j, "year"]), "prop_faecal"] *
          prop_d[which(prop_d$disease == d_by[j, "disease"]), "value_gen"]
      }
    }
    
    # Now distribute by month of the year, based on seasonality
    d_m <- unique(timeline_ende[, c("disease", "year", "month")])
    d_m <- merge(d_m, d_by[, c("disease", "year", "d")], 
      by = c("disease", "year"), all.x = TRUE)
    d_m <- merge(d_m, w, by = c("disease", "month"), all.x = TRUE)
    d_m$d_m <- d_m$d * d_m$w

    # Now distribute by day
    timeline_runi_i <- merge(timeline_runi_i, 
      d_m[, c("disease", "year", "month", "d_m")],
      by = c("disease", "year", "month"), all.x = TRUE)
    timeline_runi_i$d_d <- timeline_runi_i$d_m / 
      days_in_month(timeline_runi_i$date)
      
    # Now distribute by age group
    timeline_runi_i$d_base <- timeline_runi_i$d_d * timeline_runi_i$prop_age 
  
    # Lastly, apply crisis relative risks
    timeline_runi_i$d_crisis <- timeline_runi_i$d_base * timeline_runi_i$r0_rr * 
      timeline_runi_i$cfr_rr
##### WILL NEED TO ADD CHANGING SUSCEPTIBILITY IN LATER EDITIONS    
    
    # Aggregate and collect output by subperiod
    x <- subset(timeline_runi_i, subperiod %in% subperiods)
    x <- aggregate(x[, c("d_base", "d_crisis")], by =
      x[, c("route", "disease", "subperiod", "age")], FUN = sum)    
    x$scenario <- i
    x$run <- run_i
    out_ende <- rbind(out_ende, x)
      
  } # (close i - scenario loop)
} # (close run_i - run loop)  
close(pb) 

  #...................................      
  ## Save raw output
  write_rds(out_ende, paste(dir_path, "outputs/", 
    "out_ende_all_runs.rds", sep=""))


#...............................................................................   
### ENDS
#...............................................................................
