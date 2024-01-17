#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO PREPARE SIMULATIONS FOR INFECTIONS MODEL  ---------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Creating simulations
#...............................................................................

  #...................................      
  ## Initialise objects needed for each run

    # Simulated epidemic parameters dataframe
      # initialise
      sim_pars <- expand.grid(disease = diseases_epid, 
        parameter = c("pu", "r0", "tau", "pre_tau", "pd", "cfr", "w", "rr_r0", 
          "rr_cfr", "r0_base", "cfr_base"),
        subperiod = c("overall", subperiods)
      )
    
      # merge in fixed values
      x <- subset(epidemic_pars, parameter %in% c("tau", "pre_tau"))  
      sim_pars <- merge(sim_pars, x[, c("disease", "parameter",
        "value_gen")], by = c("disease", "parameter"), all.x = TRUE)
      x <- subset(epidemic_pars, parameter == "cfr_rr")
      x$parameter <- "cfr"
      sim_pars <- merge(sim_pars, x[, c("disease", "parameter",
        grep("value_a", colnames(x), value = TRUE) )], 
        by = c("disease", "parameter"), all.x = TRUE)
      
      
    # List of runs, which will hold all needed objects
    sim <- list()
  
    # Ranges of parameters to sample within
      # select needed parameters
      ranges <- epidemic_pars[, c("disease", "parameter", "value_gen")]
      ranges <- subset(ranges, parameter %in% c("r0_min", "r0_max",
        "cfr_min", "cfr_max", "pd_min", "pd_max", "r0_base_min", "r0_base_max",
        "cfr_base_min", "cfr_base_max"))
      
      # transform to wide version
      ranges$which_value <- substr(ranges$parameter, nchar(ranges$parameter) - 2,
        nchar(ranges$parameter))
      ranges$parameter <- sub("\\_m.*", "", ranges$parameter)
      ranges <- reshape(ranges, direction = "wide", 
        idvar = c("disease", "parameter"), timevar = "which_value")
      colnames(ranges) <- c("disease", "parameter", "max", "min")
      ranges <- ranges[, c("disease", "parameter", "min", "max")]
      
for (i in runs) {
    
    # Generate a random number from 0 to 1 (relative value of all crisis 
      #parameters, from lowest to highest)    
    rx <- runif(1)
  
  for (j in scenarios) {
    
    # Attribute probabilities of outbreaks
    for (u in diseases_epid) {
      x <- see_data[["pu"]][[j]][[u]]
      sim_pars[which(sim_pars$disease == u & sim_pars$parameter == "pu"), 
        "value_gen"] <- approx(x$p_cum,x$pu, rx)$y
    }

    for (k in subperiods) {
      
      # Figure out R0, proportion symptomatic and CFR values
      for (e in exemplars) {
        
        # figure out R0 value for exemplar disease of this route
        x <- see_data[["r0"]][[j]][[e]][[k]]
        r0_tmp <- approx(x$p_cum, x$r0, rx)$y

        # compute proportion of range covered by this value
        x <- (ranges[which(ranges$disease == e & 
            ranges$parameter == "r0"), c("min", "max")])
        r0_prop <- r0_tmp / (x[2] - x[1])
                
        # figure out CFR value for exemplar disease of this route
        x <- see_data[["cfr"]][[j]][[e]][[k]]
        cfr_tmp <- approx(x$p_cum, x$cfr, rx)$y
        
        # compute proportion of range covered by this value
        x <- (ranges[which(ranges$disease == e & 
            ranges$parameter == "cfr"), c("min", "max")])
        cfr_prop <- cfr_tmp / (x[2] - x[1])
                
        # for each disease under this exemplar (i.e. same transmission route)...
        for (u in list_exemplars[[e]]) {
          
          # if the disease is epidemic-prone...
          if (u %in% diseases_epid) {
            
            # attribute R0 value
            x <- which(ranges$disease == u & ranges$parameter == "r0")
            sim_pars[which(sim_pars$disease == u & sim_pars$parameter == "r0"
              & sim_pars$subperiod == k), "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * r0_prop
            
            # attribute proportion symptomatic value within known range
            x <- which(ranges$disease == u & ranges$parameter == "pd")
            sim_pars[which(sim_pars$disease == u & sim_pars$parameter == "pd"
              & sim_pars$subperiod == k), "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * rx
            
            # attribute CFR value
            x <- which(ranges$disease == u & ranges$parameter == "cfr")
            sim_pars[which(sim_pars$disease == u & sim_pars$parameter == "cfr" 
              & sim_pars$subperiod == k), "value_gen"] <- ranges[x, "min"] + 
              (ranges[x, "max"] - ranges[x, "min"]) * cfr_prop
            
            # attribute age-specific CFR values
            x <- which(sim_pars$disease == u & sim_pars$parameter == "cfr" 
              & sim_pars$subperiod == k)
            sim_pars[x, grep("value_a", colnames(sim_pars))] <- 
              sim_pars[x, grep("value_a", colnames(sim_pars))] * 
              sim_pars[x, "value_gen"]
          }
          
          # if the disease is NOT epidemic-prone...
          if (! u %in% diseases_epid) {
          
            # attribute relative transmissibility
            x <- which(ranges$disease == u & sim_pars$parameter == "r0")
            
            # attribute relative CFR
            x <- which(ranges$disease == u & sim_pars$parameter == "cfr")
            
          }          
        }
      }

    }
    
          
  }    
    
    
  }