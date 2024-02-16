#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO IMPLEMENT SIMULATION AND COLLECT RESULTS  ---------- ##
#...............................................................................

                                # LSHTM (January 2024)
                                # francesco.checchi@lshtm_ac.uk 



#...............................................................................  
### Implementing status quo and escalation scenario simulation
#...............................................................................

  #...................................      
  ## Prepare necessary objects
    
    # Initialise runs and random numbers / values
    df_runs <- data.frame(run = 1:runs, 
      rx_d_other = runif(runs, min = 0, max = 0.5),
      rx_sq = exp(rnorm(runs, mean = sq["mean"], sd = sq["se"])),
      rx_es = exp(rnorm(runs, mean = es["mean"], sd = es["se"])),
      rx_pc = runif(runs)
      )
      
    # Initialise output of each run
    out <- expand.grid(run = 1:runs, scenario = scenarios,
      age = ages, gender = c("male", "female"))

      # add categories of deaths
      out[, c("all", "counted", "uncounted")] <- NA
out$prop_counted <- NA 
      # sort
      out <- out[order(out$run, out$scenario, out$age, out$gender), ]
    
    # Proportion of IDPs in shelters at end of to date period
    prop_sh_end <- df[nrow(df), "prop_sh_ipol"] # for model prediction
   
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)
    

    
for (run_i in 1:runs) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
  for (i in scenarios) {

  #...................................      
  ## Select random quantities needed for run
    
    # Non-injury daily deaths by age, corrected for random prob of counting
    proj_i <- proj
    proj_i$d_other <- proj_i$d_other * df_runs[run_i, "rx_d_other"]
  
    # Daily counted deaths and proportion counted
    if (i == "status quo") {
      counted <- df_runs[run_i, "rx_sq"] * pop_tot
      prop_counted <- predict(fit_pc, newdata = 
        data.frame(dr = df_runs[run_i, "rx_sq"] * 1000, 
        prop_sh_ipol = prop_sh_end), type = "response")
    }
    if (i == "escalation") {
      counted <- df_runs[run_i, "rx_es"] * pop_tot
      prop_counted <- predict(fit_pc, newdata = 
        data.frame(dr = df_runs[run_i, "rx_es"] * 1000, 
        prop_sh_ipol = prop_sh_end), type = "response")      
    }
    if (i == "ceasefire") {
      counted <- 0
      prop_counted <- 1
    }  
  

  #...................................      
  ## Compute deaths by category, age and gender
    
    # Distribute counted deaths across age and gender
    dist <- moh_d  
    dist$counted <- dist$prop * counted

    # Add uncounted deaths
    dist$all <- dist$counted / prop_counted
        
    # Subtract deaths from non-injury causes
    dist <- merge(dist, proj_i, by = c("age", "gender"), all.x = TRUE)
    dist$all <- dist$all - dist$d_other
    dist$all <- ifelse(dist$all < 0, 0, dist$all)
    
    # Revise counted and uncounted
    dist$counted <- dist$all * prop_counted
    dist$uncounted <- dist$all * (1 - prop_counted)
    
    # Multiply by days in period
    dist[, c("all", "counted", "uncounted")] <- 
      dist[, c("all", "counted", "uncounted")] *  days_period
dist$prop_counted <- prop_counted
    
    # Store results in output
    dist <- dist[order(dist$age, dist$gender), ]
    out[which(out$run == run_i & out$scenario == i), 
      c("all", "counted", "uncounted", "prop_counted")] <- 
      dist[, c("all", "counted", "uncounted", "prop_counted")]
    

  } # close scenario i loop  
} # close run_i loop
close(pb) 
    


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
    out_ord <- expand.grid(run = 1:runs,
      age = ages, gender = c("male", "female"))

      # add categories of deaths
      out_ord$ordnance <- NA
      
      # sort
      out_ord <- out_ord[order(out_ord$run, out_ord$age, out_ord$gender), ]

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
      
      # # aggregated projected non-injury deaths
      # d_other <- sum(proj$d_other)
      # 
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
    dist$ord <- k * dist$prop * d_ord_2014 * days_period
    

} # close run_i loop
close(pb) 
    
    

#...............................................................................  
### ENDS
#...............................................................................