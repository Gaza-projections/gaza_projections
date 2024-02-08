#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO PREPARE SIMULATIONS FOR INFECTIONS MODEL  ---------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Initialising objects needed for the simulation
#...............................................................................

  #...................................      
  ## Initialise simulated disease parameters dataframe

    # Initialise
    sim_pars <- expand.grid(disease = diseases$disease, 
      parameter = c("pu", "r0", "tau", "pre_tau", "pd", "cfr", "r0_rr", 
        "cfr_rr", "r0_base", "cfr_base"),
      subperiod = c("overall", subperiods)
    )
  
    # Merge in fixed values (i.e. that won't change from one run to the next)
    x <- subset(epidemic_pars, parameter %in% c("tau", "pre_tau"))  
    sim_pars <- merge(sim_pars, x[, c("disease", "parameter",
      "value_gen")], by = c("disease", "parameter"), all.x = TRUE)
    x <- subset(epidemic_pars, parameter == "cfr_age_rr")
    x$parameter <- "cfr"
    sim_pars <- merge(sim_pars, x[, c("disease", "parameter",
      grep("value_a", colnames(x), value = TRUE) )], 
      by = c("disease", "parameter"), all.x = TRUE)
    
    # Reduce dataframe (only needed disease - parameter combinations)
      # rows not needed
      x <- c(
        which(sim_pars$disease %in% diseases_epid & 
          sim_pars$subperiod == "overall" & 
          sim_pars$parameter %in% c("r0", "pd")) ,
        which(sim_pars$disease %in% diseases_epid & 
          ! sim_pars$disease %in% exemplars &
          sim_pars$parameter %in% c("r0_base", "cfr_base")) ,
        which(sim_pars$disease %in% diseases_epid & 
          sim_pars$parameter %in% c("r0_rr", "cfr_rr")) ,
        which(! sim_pars$disease %in% diseases_epid & 
          ! sim_pars$parameter %in% c("r0_rr", "cfr_rr")) ,
        which(! sim_pars$disease %in% diseases_epid & 
          sim_pars$parameter %in% c("r0_rr", "cfr_rr") &
          sim_pars$subperiod == "overall")
      )
      
      # delete unneeded rows
      sim_pars <- sim_pars[-x, ]
     
  #...................................      
  ## Initialise ranges of parameters to sample within
    
    # Select needed parameters
    ranges <- epidemic_pars[, c("disease", "parameter", "value_gen")]
    ranges <- subset(ranges, parameter %in% c("r0_min", "r0_max",
      "cfr_min", "cfr_max", "pd_min", "pd_max", "r0_base_min", "r0_base_max",
      "cfr_base_min", "cfr_base_max"))
    
    # Transform to wide version
    ranges$which_value <- substr(ranges$parameter, nchar(ranges$parameter) - 2,
      nchar(ranges$parameter))
    ranges$parameter <- sub("\\_m.*", "", ranges$parameter)
    ranges <- reshape(ranges, direction = "wide", 
      idvar = c("disease", "parameter"), timevar = "which_value")
    colnames(ranges) <- c("disease", "parameter", "max", "min")
    ranges <- ranges[, c("disease", "parameter", "min", "max")]
        
  #...................................      
  ## Initialise other objects

    # List of runs, which will hold all needed objects
      # list structure: [[run]][[scenario]][[subperiod]]
    sim <- list()
          
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)

    
#...............................................................................   
### Preparing simulation runs
#...............................................................................

# (run loop starts here)    
for (run_i in 1:max(runs$run)) {

  #...................................      
  ## Preparatory steps

    # Update progress bar
    setTxtProgressBar(pb, run_i)
  
    # Identify random numbers from [0,1]
    rx_r0 <- runs[run_i, "rx_r0"]
    rx_cfr <- runs[run_i, "rx_cfr"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  

    # Initialise fresh version of parameters dataframe
    sim_pars_run_i <- sim_pars
    
  #...................................      
  ## Generate random parameter values for exemplar diseases at baseline
    # (common to all scenarios and subperiods); not correlated to other
    # uncertainty distributions
      
  for (e in exemplars) {
    # R0 at baseline
    x <- ranges[which(ranges$disease == e & ranges$parameter == "r0_base"), 
      c("min", "max")]
    sim_pars_run_i[which(sim_pars_run_i$disease == e & 
      sim_pars_run_i$parameter == "r0_base"), "value_gen"] <- 
      x$min + runif(1) * (x$max - x$min)
    
    # CFR at baseline
    x <- ranges[which(ranges$disease == e & ranges$parameter == "cfr_base"), 
      c("min", "max")]
    sim_pars_run_i[which(sim_pars_run_i$disease == e & 
      sim_pars_run_i$parameter == "cfr_base"), "value_gen"] <- 
      x$min + runif(1) * (x$max - x$min)      
  }  

    
  #...................................      
  ## Generate random parameter values for each scenario...
  
  # (scenario loop starts here)    
  for (i in scenarios) {

    # Attribute probabilities of outbreaks
    for (u in diseases_epid) {
      x <- subset(see, disease == u & scenario == i & expert == "all" &
        parameter == "pu")
      sim_pars_run_i[which(sim_pars_run_i$disease == u & 
        sim_pars_run_i$parameter == "pu"), "value_gen"] <- 
        approx(x[, grep("pcum_", names(x))], x[, grep("x_", names(x))], rx_r0, 
          rule = 2, ties = list("ordered", mean))$y
    }

    # (subperiod loop starts here)
    #...and for each subperiod:
    for (j in subperiods) {
      
      # attribute R0, proportion symptomatic and CFR values, for each disease
        # under a given exemplar disease (i.e. same transmission route)...
      for (e in exemplars) {
        
        # figure out R0 value for exemplar disease of this route
        x <- subset(see, disease == e & scenario == i & subperiod == j &
          expert == "all" & parameter == "r0")
        r0_tmp <- approx(x[, grep("pcum_", names(x))], x[,grep("x_", names(x))], 
          rx_r0,  rule = 2, ties = list("ordered", mean))$y

          # compute proportion of range covered by this value
          x <- (ranges[which(ranges$disease == e & 
              ranges$parameter == "r0"), c("min", "max")])
          r0_prop <- r0_tmp / (x$max - x$min)
                
          # compute relative transmissibility
          r0_rr <- r0_tmp / sim_pars_run_i[which(sim_pars_run_i$disease == e & 
            sim_pars_run_i$parameter == "r0_base" & 
            sim_pars_run_i$subperiod == j), "value_gen"]
          
        # figure out CFR value for exemplar disease of this route
        x <- subset(see, disease == e & scenario == i & subperiod == j &
          expert == "all" & parameter == "cfr")
        cfr_tmp <- approx(x[,grep("pcum_", names(x))], x[,grep("x_", names(x))], 
          rx_cfr,  rule = 2, ties = list("ordered", mean))$y
        
          # compute proportion of range covered by this value
          x <- (ranges[which(ranges$disease == e & 
              ranges$parameter == "cfr"), c("min", "max")])
          cfr_prop <- cfr_tmp / (x$max - x$min)

          # compute relative CFR
          cfr_rr <- cfr_tmp / sim_pars_run_i[which(sim_pars_run_i$disease == e & 
            sim_pars_run_i$parameter == "cfr_base" & 
            sim_pars_run_i$subperiod == j), "value_gen"]
          
        # for each disease under this exemplar (i.e. same transmission route)...
        for (u in list_exemplars[[e]]) {
          
          # if the disease is epidemic-prone...
          if (u %in% diseases_epid) {
            
            # attribute R0 value
            x <- which(ranges$disease == u & ranges$parameter == "r0")
            sim_pars_run_i[which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "r0" & sim_pars_run_i$subperiod == j), 
              "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * r0_prop
            
            # attribute proportion symptomatic value within known range
            x <- which(ranges$disease == u & ranges$parameter == "pd")
            sim_pars_run_i[which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "pd" & sim_pars_run_i$subperiod == j), 
              "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * rx_cfr
            
            # attribute CFR value
            x <- which(ranges$disease == u & ranges$parameter == "cfr")
            sim_pars_run_i[which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "cfr" & sim_pars_run_i$subperiod== j), 
              "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * cfr_prop
            
            # attribute age-specific CFR values
            x <- which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "cfr" & sim_pars_run_i$subperiod == j)
            sim_pars_run_i[x, grep("value_a", colnames(sim_pars_run_i))] <- 
              sim_pars_run_i[which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "cfr" 
              & sim_pars_run_i$subperiod == j), "value_gen"] * 
              sim_pars_run_i[which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "cfr" 
              & sim_pars_run_i$subperiod == "overall"), 
                grep("value_a", colnames(sim_pars_run_i))]
          }
          
          # if the disease is NOT epidemic-prone...
          if (! u %in% diseases_epid) {
          
            # attribute relative transmissibility
            x <- which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "r0_rr" 
              & sim_pars_run_i$subperiod == j)
            sim_pars_run_i[x, "value_gen"] <- r0_rr
            
            # attribute relative CFR
            x <- which(sim_pars_run_i$disease == u & 
              sim_pars_run_i$parameter == "cfr_rr" 
              & sim_pars_run_i$subperiod == j)
            sim_pars_run_i[x, "value_gen"] <- cfr_rr
            
          }          
        }
      }

    } # (close j - subperiod loop) 

    # Attribute parameters table to this specific run and scenario
    sim[[paste("run", run_i, sep = "_")]][[i]] <- sim_pars_run_i  
    
  } # (close i - scenario loop)   

} # (close run_i - run loop)
close(pb)    
      
      
#...............................................................................
### ENDS
#...............................................................................