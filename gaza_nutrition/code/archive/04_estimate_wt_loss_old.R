#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------------- R SCRIPT TO ESTIMATE WEIGHT LOSS AMONG ADULTS  ------------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Initialise new columns of NCD survey dataframe
    
    # Value of caloric intake for each month to date in adult survey data
    df_ad$duration <- NA
    df_ad[, paste("intake_month", 1:months_todate, sep = "_")] <- NA
    
    # Intake reduction and weight loss
    for (i in c("to_date", names(subperiods)) ) {
      df_ad[, paste("intake_reduction", i, sep = "_")] <- NA
      df_ad[, paste("percent_wt_loss", i, sep = "_")] <- NA
    }
    df_ad$intake_reduction <- NA
    df_ad$percent_wt_loss <- NA
    
  #...................................      
  ## Initialise or read other objects 
    
    # Read model of weight loss as a function of dietary intake
    fit <- read_rds(paste(dir_path, "outputs/", "wt_loss_model.rds", sep=""))  
    
    # Read estimates of caloric intake from food aid to date, per run
    if (!exists("aid_to_date") ) {aid_to_date <- read_rds(paste(dir_path, 
      "outputs/", "out_food_aid_to_date.rds", sep=""))}
    aid_to_date <- aid_to_date[order(aid_to_date$run, aid_to_date$month), ]
    
    # Output columns of interest
    out_cols <- c(grep("intake_reduction_", colnames(df_ad), value = TRUE),
        grep("percent_wt_loss_", colnames(df_ad), value = TRUE) )
    
    # Initialise output of each run
    out <- expand.grid(run = runs$run, scenario = scenarios, 
      age_cat = c("40 to 49yo", "50 to 59yo", "60 to 69yo", "70+yo", "all"))
    out[, out_cols] <- NA
    out <- out[order(out$run, out$scenario, out$age_cat), ]
        
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)
    
#...............................................................................  
### Estimating weight loss as percent of starting weight and intake reduction
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
    
    # Select random food aid quantities per month to date
    aid <- as.vector(aid_to_date[which(aid_to_date$run == run_i), "aid"])
    
    # Select random proportions coming from different non-aid sources, per month
    todate_pars$prop <- todate_pars$min + 
      (1 - rx) * (todate_pars$max - todate_pars$min)
    
    # Select random scenario values of food intake
    scenario_pars$intake <- intake_target * (scenario_pars$min + (1 - rx) * 
      (scenario_pars$max - scenario_pars$min) )
  
  #...................................      
  ## Figure out intake and weight loss for period to date
    
    # Compute total non-aid proportion by month (may be > 1)
    prop <- aggregate(todate_pars$prop, by = list(month = todate_pars$month), 
      FUN = sum)$x
    
    # Compute intake per month in period to date
    for (i in 1:months_todate) {
      # intake is either the baseline or the sum of food aid and other sources,
        # whichever is lower
      df_ad[, paste("intake_month", i, sep = "_")] <- pmin(aid[i] + 
        df_ad$intake_baseline * prop[i], df_ad$intake_baseline)
    }
    
    # Compute overall mean intake reduction to date
    df_ad$intake_reduction <- df_ad$intake_baseline - 
      rowSums(df_ad[, grep("intake_month", colnames(df_ad))]) / months_todate
    
      # store value
      df_ad$intake_reduction_to_date <- df_ad$intake_reduction 
    
    # Estimate percent weight reduction to date from prediction and its SE
      # set months of duration
      df_ad$duration <- months_todate
      
      # predict
      x <- predict(fit, newdata = df_ad, se.fit = TRUE)
      
      # sample from prediction interval
      df_ad$percent_wt_loss <- exp(qnorm(rx, mean = x$fit, sd = x$se.fit)) *
        months_todate
      
      # store prediction
      df_ad$percent_wt_loss_to_date <- df_ad$percent_wt_loss

  #...................................      
  ## Figure out intake and weight loss by scenario...
    
  for (i in scenarios) {
    
    #...and projection subperiod:
    
    for (j in names(subperiods) ) {
      
      # figure out total time of exposure to projection period (midpoint)
      months_project <- ifelse(j == "subperiod1", 1.5, 4.5)
      
      # set months of exposure
      df_ad$duration <- months_todate + months_project
    
      # compute mean intake reduction at midpoint, weighted by period durations
      if (j == "subperiod1") {
        df_ad$intake_reduction <-(df_ad$intake_reduction_to_date * months_todate 
          + (df_ad$intake_baseline - 
              scenario_pars[which(scenario_pars$scenario == i 
              & scenario_pars$period == subperiods[[j]]), "intake"]) *  
            months_project) / (months_todate + months_project)
      }
      
      if (j == "subperiod2") {
      df_ad$intake_reduction <-(df_ad$intake_reduction_to_date * months_todate +
        (df_ad$intake_baseline - scenario_pars[which(scenario_pars$scenario == i 
          & scenario_pars$period == subperiods[["subperiod1"]]), "intake"])* 3 +
        (df_ad$intake_baseline - scenario_pars[which(scenario_pars$scenario == i 
          & scenario_pars$period == subperiods[["subperiod2"]]), "intake"]) *  
          1.5) / (months_todate + months_project)
      }
      
        # store value
        df_ad[, paste("intake_reduction", j, sep = "_")] <- 
          df_ad$intake_reduction
        
      # predict
      x <- predict(fit, newdata = df_ad, se.fit = TRUE)
      
      # sample from prediction interval
      df_ad$percent_wt_loss <- exp(qnorm(rx, mean = x$fit, sd = x$se.fit)) *
        (months_todate + months_project)
      
      # store prediction
      df_ad[, paste("percent_wt_loss", j, sep = "_")] <- 
        df_ad$percent_wt_loss
      
    } 
    
    # Output results of run, for this scenario
      # compute medians by age group
      out_i <- aggregate(df_ad[, out_cols], by = list(age_cat = df_ad$age_cat),
        FUN = median)
      
      # add overall medians
      out_i <-rbind(out_i[, 2:ncol(out_i)], apply(df_ad[, out_cols], 2, median) )
      
      # add to overall output
      out[which(out$run == run_i & out$scenario == i), out_cols] <- out_i
  }
}  # close run_i loop
close(pb)  


#...............................................................................  
### Outputting and visualising results
#...............................................................................

  #...................................      
  ## Compute distribution of runs by age group and overall
    
    # Compute median and 95% percentile intervals of all estimated quantities    
    agg <- aggregate(out[, out_cols], by = out[, c("scenario", "age_cat")],
      FUN = function(x) {quantile(x, c(0.50, 0.025, 0.975))})

    # Output percentile intervals
    write.csv(agg, paste(dir_path, "outputs/","out_wt_loss_adults.csv", sep=""),
      row.names = FALSE)
    
    # Output actual runs (all ages only), for sampling in subsequent script
    out_all <- out[which(out$age_cat == "all"), c("run", "scenario",
      grep("percent_wt_loss_", colnames(out), value = TRUE))]
    write_rds(out_all, paste(dir_path, "outputs/",
      "out_all_wt_loss_adults.rds", sep=""))
    
    
  #...................................      
  ## Plot distribution of runs by outcome (all age groups)
    
    # Outcome: Nutrient intake deficit
      # select and reshape data
      out_plot <- out[which(out$age_cat == "all"), c("run", "scenario", 
        grep("intake_reduction_", colnames(out), value = TRUE))]
      out_plot <- reshape(out_plot, direction = "long", 
        idvar = c("run", "scenario"), timevar = "period",
        v.names = "intake_reduction",
        varying = list(grep("intake_reduction_", colnames(out_plot), TRUE)) )
      out_plot$period <- c("to date", subperiods)[out_plot$period]
      out_plot$scenario <- factor(out_plot$scenario, 
        levels = c("worst", "central", "best"), labels =
        c("reasonable-worst", "central", "reasonable-best"))
      out_plot$period <- factor(out_plot$period, 
        levels = c("to date", subperiods))

      # plot  
      plot1 <- ggplot(data = out_plot, aes(x = intake_reduction, y = period,
        colour = period,
        fill = period)) +
        geom_boxplot(alpha = 0.5) +
        theme_bw() +
        scale_x_continuous("daily intake reduction (Kcal/day)",
          breaks = seq(0, 2000, 200), limits = c(0, NA)) +
        scale_colour_manual(values = palette_cb[c(12, 8, 4)]) +
        scale_fill_manual(values = palette_cb[c(12, 8, 4)]) +
        facet_wrap(. ~ scenario) +
        theme(legend.position = "none")
    
    # Outcome: Percent weight loss
      # select and reshape data
      out_plot <- out[which(out$age_cat == "all"), c("run", "scenario", 
        grep("percent_wt_loss_", colnames(out), value = TRUE))]
      out_plot <- reshape(out_plot, direction = "long", 
        idvar = c("run", "scenario"), timevar = "period",
        v.names = "percent_wt_loss",
        varying = list(grep("percent_wt_loss_", colnames(out_plot), TRUE)) )
      out_plot$period <- c("to date", subperiods)[out_plot$period]
      out_plot$scenario <- factor(out_plot$scenario, 
        levels = c("worst", "central", "best"), labels =
        c("reasonable-worst", "central", "reasonable-best"))
      out_plot$period <- factor(out_plot$period, 
        levels = c("to date", subperiods))      
      
      # plot  
      plot2 <- ggplot(data = out_plot, aes(x = percent_wt_loss, colour = period,
        fill = period)) +
        geom_density(alpha = 0.5) +
        theme_bw() +
        scale_x_continuous("percent cumulative weight loss", labels = percent,
          breaks = seq(0, 1, 0.1)) +
        scale_colour_manual(values = palette_cb[c(12, 8, 4)]) +
        scale_fill_manual(values = palette_cb[c(12, 8, 4)]) +
        facet_wrap(. ~ scenario) +
        theme(legend.position = "top", axis.text.y = element_blank())
         
    # Combined plot
    ggarrange(plot1, plot2, nrow = 2, labels = c("A", "B"), heights = c(1, 2))
    
    ggsave(paste(dir_path, "outputs/", "outcomes_adults_combi.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)       
          
#...............................................................................  
### ENDS
#...............................................................................

