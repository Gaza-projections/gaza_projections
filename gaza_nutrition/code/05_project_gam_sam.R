#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO PROJECT GAM AND SAM PREVALENCE IN KIDS  ---------- ##
#...............................................................................

                          # Francesco Checchi, Zeina Jamaluddine (January 2024)


#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Generate baseline weight-for-height Z scores and SAM/GAM prevalence
    # plus proportion of flags
    
    # Add anthropometric indices
    x <- with(df_gm,
      anthro::anthro_zscores(sex = sex, age = age_mths, weight = weight,
          lenhei = height, is_age_in_month = TRUE)
    )
    
    # Compute proportion of flagged observations
    prop_flags_base <- prop.table(table(x$fwfl))[["1"]]
      
    # Remove flagged WHZ observations (<> 5SD)
    x <- subset(x, fwfl != 1)
      
    # compute SAM and GAM prevalence
    sam_base <- prop.table(table(x$zwfl < -3))["TRUE"]
    gam_base <- prop.table(table(x$zwfl < -2))["TRUE"]    


  #...................................      
  ## Generate a sub-sample of growth monitoring dataset (to expedite processing)
    
    # Sample rows in the dataset
    x <- sample.int(nrow(df_gm), 10000, replace = FALSE)

    # Sample 10,000 observations
    df_sub <- df_gm[x, ]
    
    # Compare SAM and GAM in the full and sub-sample
      # add anthropometric indices
      x <- with(df_sub,
        anthro::anthro_zscores(sex = sex, age = age_mths, weight = weight,
            lenhei = height, is_age_in_month = TRUE)
      )
      
      # Compute proportion of flagged observations
      prop_flags_sub <- prop.table(table(x$fwfl))[["1"]]
        
      # Remove flagged WHZ observations (<> 5SD)
      x <- subset(x, fwfl != 1)
        
      # compute SAM and GAM prevalence
      sam_sub <- prop.table(table(x$zwfl < -3))["TRUE"]
      gam_sub <- prop.table(table(x$zwfl < -2))["TRUE"]    
    
      # compare
      prop_flags_base - prop_flags_sub
      sam_base - sam_sub
      gam_base - gam_sub
        # comparison looks OK!
          
  #...................................      
  ## Initialise or read other objects 

    # Read output runs from adult survey analysis, if not already in environment
    if (!exists("out_all") ) {out_all <- read_rds(paste(dir_path, "outputs/", 
      "out_all_wt_loss_adults.rds", sep=""))}
      # rename columns
      colnames(out_all) <- gsub("percent_wt_loss_", "", colnames(out_all))
    
    # Range of relative risks of weight loss (children : adults)
    rr_range <- c(gen_pars[which(gen_pars$parameter == "wt_loss_child_min"), 
      "value_gen"],
      gen_pars[which(gen_pars$parameter == "wt_loss_child_max"), 
      "value_gen"] )
    rr_range <- as.numeric(rr_range)

    # Initialise output of each run
    out <- expand.grid(run = runs$run, scenario = scenarios, 
      period = c("pre-war", "to date", subperiods))
    out[, c("sam", "gam")] <- NA
    out <- out[order(out$run, out$scenario, out$period), ]
    
      # add baseline SAM and GAM
      out$period[ out$period == "baseline"] <- "pre-war"
      out[which(out$period == "pre-war"), "sam"] <- sam_base
      out[which(out$period == "pre-war"), "gam"] <- gam_base
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)
    
    
#...............................................................................  
### Estimating distribution and prevalence of GAM and SAM in children 6-59mo,
  # by scenario and period, for each simulation run
#...............................................................................

for (run_i in 1:nrow(runs)) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
  #...................................      
  ## Select random quantities needed for run

    # Identify random number from [0,1]
    rx <- runs[run_i, "rx"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  
    
    # Select random relative risk of weight loss (children : adults)
    rr <- rr_range[1] + rx * (rr_range[2] - rr_range[1])
  
  #...................................      
  ## For each scenario...
  for (i in scenarios) {
    
    #...and for each subperiod:
    for (j in c("to date", subperiods)) {
     
      # reduce weight of all children (= weight loss in adults +- uncertainty,
        # with uncertainty sampled based on value of rx for run)
      df_sub$weight_ij <- df_sub$weight *
        (1 - out_all[which(out_all$run == run_i & out_all$scenario == i &
          out_all$period == j), "percent_wt_loss"] * rr)
       
      # compute anthropometric indices and flags
      x <- with(df_sub,
        anthro::anthro_zscores(sex = sex, age = age_mths, weight = weight_ij,
          lenhei = height, is_age_in_month = TRUE) )
      
      # remove flagged WHZ observations (<> 5SD)
      x <- subset(x, fwfl != 1)
      
      # compute SAM and GAM prevalence
      sam <- prop.table(table(x$zwfl < -3))["TRUE"]
      gam <- prop.table(table(x$zwfl < -2))["TRUE"]
      
      # update output with SAM and GAM prevalence
      out[which(out$run == run_i & out$scenario == i & out$period == j),
        c("sam", "gam")] <- c(sam, gam)
      
    }
  }
} # close run_i loop
close(pb)      
    
    
#...............................................................................  
### Outputting and visualising results
#...............................................................................

  #...................................      
  ## Compute average and uncertainty of runs, by scenario and period
    
    # Output raw simulation runs
    write_rds(out, paste(dir_path, "outputs/","out_gam_sam_all_runs.rds", 
      sep=""))

    # Compute median and 95% percentile intervals of all estimated quantities    
    agg <- aggregate(out[, c("sam", "gam")], 
      by = out[, c("scenario", "period")],
      FUN = function(x) {quantile(x, c(0.50, 0.025, 0.975), na.rm = TRUE)})

    # Output percentile intervals
    write.csv(agg, paste(dir_path, "outputs/","out_gam_sam.csv", sep=""),
      row.names = FALSE)
    
  #...................................      
  ## Visualise, by scenario and period
    
    # Prepare dataset
    agg <- data.frame(agg[, c("scenario", "period")], unlist(agg[["sam"]]),
      unlist(agg[["gam"]]))
    colnames(agg) <- c("scenario", "period", 
      paste("sam", c("median", "lci", "uci"), sep = "_"),
      paste("gam", c("median", "lci", "uci"), sep = "_")
    )
    agg <- reshape(agg, direction = "long", timevar = "indicator",
      varying = list(median = grep("_median", colnames(agg), value = TRUE), 
        lci = grep("_lci", colnames(agg), value = TRUE),
        uci = grep("_uci", colnames(agg), value = TRUE)),
      v.names = c("median", "lci", "uci"),
      times = c("severe acute malnutrition", "global acute malnutrition"))    
      agg$scenario <- factor(agg$scenario, 
        levels = c("best", "central", "worst"), labels =
        c("Ceasefire", "Status Quo", "Escalation"))
    
    # Plot
    ggplot(data = agg, aes(x = period, colour = scenario, fill = scenario)) +
      geom_bar(aes(y = median), stat = "identity", alpha = 0.5) +
      # geom_errorbar(aes(ymin = lci, ymax = uci),
      #   width = 1, linetype = "21") +
      theme_bw() +
      facet_grid(indicator ~ scenario, scales = "free_y") +
      theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
      scale_y_continuous("prevalence", labels = percent, 
        breaks = seq(0, 1, 0.05)) +
      geom_text(aes(x = period, y = median * 1.05 + 0.02,
        label = scales::percent(median, accuracy = 0.1) ),
        colour = "grey20") +
      scale_colour_manual(values = palette_cb[c(12, 8, 4)]) +
      scale_fill_manual(values = palette_cb[c(12, 8, 4)])

    ggsave(paste(dir_path, "outputs/", "out_gam_sam_kids.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)       
