#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO ESTIMATE DEATHS IN THE CEASEFIRE SCENARIO  ------- ##
#...............................................................................


#...............................................................................  
### Projecting deaths due to previous wounds in the ceasefire scenario
#...............................................................................
    
  #...................................
  ## Set up cohort timeline
  
    # Initialise timeline
    ci_cf <- ci_base

    # Initialise some variables
    ci_cf$prop_counted <- NA
    ci_cf$i_all <- NA
    
    # Modify CFR = ceasefire value
    cfr_tau <- cfr_tau * cfr_cf / cfr_sq

    # Add daily CFR to each tau day
    ci_cf[, paste("d", 1:730, sep = "")] <- t(replicate(nrow(ci_cf), cfr_tau))
    
    
  #...................................
  ## Simulation to project deaths due to injuries during the ceasefire period
  
    # Initialise runs and random numbers / values
    df_runs <- data.frame(run = 1:runs,
      rx_d_other = runif(runs, min = 0, max = 0.5), 
      rx_p_m = sample(out_pm$p_m, runs, replace = TRUE)
    )

    # Initialise output of each run
    out_cf <- expand.grid(run = 1:runs, period = subperiods,
      age = ages, gender = c("male", "female"))

      # add injuries
      out_cf$d_dow <- NA

      # sort
      out_cf <- out_cf[order(out_cf$run, out_cf$period, 
        out_cf$age, out_cf$gender), ]
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)
    
  for (run_i in 1:runs) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
    # Fresh cohort timeline
    ci_i <- ci_cf
    
    # Generate proportion of injuries that die immediately
    p_m <- df_runs[run_i, "rx_p_m"]
    
    # Generate random proportions counted for each day
    ci_i$prop_counted <- suppressWarnings(inv.logit(rnorm(nrow(ci_i), 
      mean = ci_i$prop_counted_pred, sd = ci_i$prop_counted_pred_se)))
    
    # Compute true injuries and deaths
    ci_i$i_all <- ci_i$i_counted / ci_i$prop_counted 
    
    # Deaths from wounds, by tau
    ci_i[, paste("d", 1:730, sep = "")] <- 
      ci_i[, paste("d", 1:730, sep = "")] * ci_i$i_all * (1 - p_m)
    
  #...................................
  ## Compute deaths due to wounds over time

    # Lag each tau column to match with chronological time
    for (tau in 1:730) {
      ci_i[, paste("d", tau, sep = "")] <- 
        dplyr::lag(ci_i[, paste("d", tau, sep = "")], tau - 1)
    }
    
    # Compute deaths due to wounds
    ci_i$d_dow <- rowSums(ci_i[, paste("d", 1:730, sep = "")], na.rm = TRUE)
    
    # Sum deaths by subperiod in projection period
    x <- subset(ci_i, period %in% subperiods)
    x <- aggregate(list(d_dow = x$d_dow), by = list(subperiod = x$period),
      FUN = sum)
    
    # Distribute deaths across age and gender
    dist1 <- moh_d  
    dist1$d_dow <- dist1$prop * x[which(x$subperiod== "months 1 to 3"), "d_dow"]
    dist1$period <- "months 1 to 3"
    dist2 <- moh_d  
    dist2$d_dow <- dist2$prop * x[which(x$subperiod== "months 4 to 6"), "d_dow"]
    dist2$period <- "months 4 to 6"    
    dist <- rbind(dist1, dist2)
    dist$period <- factor(dist$period, levels = subperiods)
    
    # Store results in output
    dist <- dist[order(dist$period, dist$age, dist$gender), ]
    out_cf[which(out_cf$run == run_i), "d_dow"] <- 
      dist$d_dow
    
  }   
close(pb)     
            
  #...................................      
  ## Save raw output
  write_rds(out_cf, paste(dir_path, "outputs/", 
    "out_cf_all_runs.rds", sep=""))   
    
    
#...............................................................................  
### Working out deaths due to unexploded ordnance and mines (ceasefire scenario)
#...............................................................................

  #...................................      
  ## Prepare necessary objects
    
    # Initialise runs and random numbers / values
    x <-  matrix(simulate(fit_pc, nsim = runs), nrow = nrow(df))
    x <- apply(x, 2, sample)
    df_runs <- data.frame(run = 1:runs, 
      rx_d_other = runif(runs, min = 0, max = 0.5),
      rx_pc = as.vector(x)
      )
      
    # Initialise output of each run
    out_ord <- expand.grid(run = 1:runs, period = subperiods,
      age = ages, gender = c("male", "female"))

      # add ordnance deaths and injuries
      out_ord$d_ord <- NA
      out_ord$i_ord <- NA
      
      # sort
      out_ord <- out_ord[order(out_ord$run, out_ord$period, out_ord$age, 
        out_ord$gender), ]

    # Static parameters
      
      # counted deaths to date
      counted <- max(daily$d_cum, na.rm = TRUE)
      
      # data from 2014 conflict (per day for ordnance deaths, since they are
        # over a year)
      d_2014 <- ord[which(ord$variable == "deaths_tot"), "value"]   
      i_2014 <- ord[which(ord$variable == "injuries_tot"), "value"]       
      d_ord_2014 <- ord[which(ord$variable == "deaths_ordnance"), "value"] / 365
      i_ord_2014 <- ord[which(ord$variable == "injuries_ordnance"), "value"] /
        365
      
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)
    

  #...................................      
  ## Implement simulation
        
for (run_i in 1:runs) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  

    # Generate a random proportion counted
    prop_counted <- df_runs[run_i, "rx_pc"]   

    # Distribute counted deaths across age and gender
    dist <- moh_d  
    dist$counted <- dist$prop * counted
    
    # Add uncounted deaths
    dist$all <- dist$counted / prop_counted
        
    # Subtract deaths from non-injury causes
    proj_i <- proj
    proj_i$d_other <- proj_i$d_other * df_runs[run_i, "rx_d_other"]
    dist <- merge(dist, proj_i, by = c("age", "gender"), all.x = TRUE)
    dist$all <- dist$all - dist$d_other
    dist$all <- ifelse(dist$all < 0, 0, dist$all)
        
    # Compute ordnance deaths during projection period 
    k <- (sum(dist$all) / d_2014)
    dist$d_ord <- k * dist$prop * d_ord_2014
    
    # Compute ordnance injuries during projection period
    k <- (sum(dist$all) / d_2014)
    dist$i_ord <- k * dist$prop * i_ord_2014
    
    # Divide equally into subperiods
    dist1 <- dist
    dist1[, c("d_ord", "i_ord")] <- 
      dist1[, c("d_ord", "i_ord")] *  days_period/2
    dist1$period <- "months 1 to 3"
    dist2 <- dist
    dist2[, c("d_ord", "i_ord")] <- 
      dist2[, c("d_ord", "i_ord")] *  days_period/2
    dist2$period <- "months 4 to 6"
    dist <- rbind(dist1, dist2)
    dist$period <- factor(dist$period, levels = subperiods)
    
    # Store results in output
    dist <- dist[order(dist$period, dist$age, dist$gender), ]
    out_ord[which(out_ord$run == run_i), c("d_ord", "i_ord")] <- 
      dist[, c("d_ord", "i_ord")]

} # close run_i loop
close(pb) 

  #...................................      
  ## Save raw output
  write_rds(out_ord, paste(dir_path, "outputs/", 
    "out_ord_all_runs.rds", sep=""))   
    


#...............................................................................  
### ENDS
#...............................................................................      
      