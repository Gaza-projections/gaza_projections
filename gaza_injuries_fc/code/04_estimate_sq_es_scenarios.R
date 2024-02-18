#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO IMPLEMENT SIMULATION: STATUS QUO + ESCALATION SCENARIOS  -- ##
#...............................................................................



#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Prepare necessary objects
    
    # Initialise runs and random numbers / values
    df_runs <- data.frame(run = 1:runs, 
      rx_d_other = runif(runs, min = 0, max = 0.5),
      rx_dsq = exp(rnorm(runs, mean = est_dsq["mean"], sd = est_dsq["se"])),
      rx_des = exp(rnorm(runs, mean = est_des["mean"], sd = est_des["se"])),
      rx_isq = exp(rnorm(runs, mean = est_isq["mean"], sd = est_isq["se"])),
      rx_ies = exp(rnorm(runs, mean = est_ies["mean"], sd = est_ies["se"])),
      rx_pc = runif(runs),
      rx_p_m = sample(out_pm$p_m, runs, replace = TRUE)
      )
      
    # Initialise output of each run
    out_main <- expand.grid(run = 1:runs, 
      scenario = c("status quo", "escalation"),
      period = subperiods, age = ages, gender = c("male", "female"))

      # add categories of deaths
      out_main[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow")] <- NA

      # add injuries
      out_main$i_all <- NA
      
      # sort
      out_main <- out_main[order(out_main$run, out_main$scenario, 
        out_main$period, out_main$age, out_main$gender), ]
    
    # Proportion of IDPs in shelters at end of to date period
    prop_sh_end <- df[nrow(df), "prop_sh_ipol"] # for model prediction
   
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)
    
    
#...............................................................................  
### Implementing status quo and escalation scenario simulation
#...............................................................................

for (run_i in 1:runs) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
  for (i in c("status quo", "escalation")) {

  #...................................      
  ## Select random quantities needed for run
    
    # Generate proportion of injuries that die immediately
    p_m <- df_runs[run_i, "rx_p_m"]
        
    # Non-injury daily deaths by age, corrected for random prob of counting
    proj_i <- proj
    proj_i$d_other <- proj_i$d_other * df_runs[run_i, "rx_d_other"]
  
    # Daily counted deaths, injuries and proportion counted
    if (i == "status quo") {
      d_counted <- df_runs[run_i, "rx_dsq"] * pop_tot
      i_counted <- df_runs[run_i, "rx_isq"] * pop_tot
      prop_counted <- predict(fit_pc, newdata = 
        data.frame(dr = df_runs[run_i, "rx_dsq"] * 1000, 
        prop_sh_ipol = prop_sh_end), type = "response")
    }
    if (i == "escalation") {
      d_counted <- df_runs[run_i, "rx_des"] * pop_tot
      i_counted <- df_runs[run_i, "rx_ies"] * pop_tot
      prop_counted <- predict(fit_pc, newdata = 
        data.frame(dr = df_runs[run_i, "rx_des"] * 1000, 
        prop_sh_ipol = prop_sh_end), type = "response")      
    }

  #...................................      
  ## Compute deaths and injuries by category, age and gender
    
    # Distribute counted deaths across age and gender
    dist <- moh_d  
    dist$d_counted <- dist$prop * d_counted

    # Add uncounted deaths
    dist$prop_counted <- prop_counted
    dist$d_all <- dist$d_counted / dist$prop_counted
    
    # Subtract deaths from non-injury causes
    dist <- merge(dist, proj_i, by = c("age", "gender"), all.x = TRUE)
    dist$d_all <- dist$d_all - dist$d_other
    dist$d_all <- ifelse(dist$d_all < 0, 0, dist$d_all)
    
    # Revise counted and uncounted
    dist$d_counted <- dist$d_all * dist$prop_counted
    dist$d_uncounted <- dist$d_all - dist$d_counted
    
    # Add injuries
    dist$i_all <- dist$prop * i_counted / dist$prop_counted
    
    # Add immediate deaths
    dist$d_m <- dist$i_all * p_m
    
    # Add deaths due to wounds
    dist$d_dow <- dist$d_all - dist$d_m
    
    # Divide equally into subperiods
    dist1 <- dist
    dist1[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")] <- 
      dist1[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")] *  
      days_period/2
    dist1$period <- "months 1 to 3"
    dist2 <- dist
    dist2[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")] <- 
      dist2[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")] *  
      days_period/2
    dist2$period <- "months 4 to 6"
    dist <- rbind(dist1, dist2)
    dist$period <- factor(dist$period, levels = subperiods)
        
    # Store results in output
    dist <- dist[order(dist$period, dist$age, dist$gender), ]
    out_main[which(out_main$run == run_i & out_main$scenario == i), 
      c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")] <- 
      dist[, c("d_all", "d_counted", "d_uncounted", "d_m", "d_dow", "i_all")]
    

  } # close scenario i loop  
} # close run_i loop
close(pb) 
    
  #...................................      
  ## Save raw output
  write_rds(out_main, paste(dir_path, "outputs/", 
    "out_main_all_runs.rds", sep=""))   
    

#...............................................................................  
### ENDS
#...............................................................................