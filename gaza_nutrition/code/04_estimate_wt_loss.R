#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ------------- R SCRIPT TO ESTIMATE WEIGHT LOSS AMONG ADULTS  ------------- ##
#...............................................................................

                          # Francesco Checchi, Zeina Jamaluddine (January 2024)


#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Initialise objects required for weight change model
    
    # Timeline
    timeline <- data.frame(date = as.Date(date_crisis : date_end), prop = NA)
    timeline$period <- "to_date"
    timeline[which(timeline$date %in% as.Date(date_start:(date_mid - 1))),
      "period"] <- names(subperiods)[1]
    timeline[which(timeline$date %in% as.Date(date_mid:date_end)),
      "period"] <- names(subperiods)[2]
    timeline$day <- 1:nrow(timeline)
    
    # Days for which an outcome is sought: start and midpoints of subperiods
    x <- c(date_start - 1, date_start + (date_mid - 1 - date_start) /2,
      date_mid + (date_end - date_mid +1) / 2)
    days_out <- which(timeline$date %in% x)
    
    # Projection days
    days <- timeline[which(timeline$period != "to_date"), "day"]
    
  #...................................      
  ## Initialise or read other objects 
    
    # Read estimates of caloric intake from food aid to date, per run
    if (!exists("aid_to_date") ) {aid_to_date <- read_rds(paste(dir_path, 
      "outputs/", "out_food_aid_to_date.rds", sep=""))}
    aid_to_date <- aid_to_date[order(aid_to_date$run, aid_to_date$date), ]
    
    # Initialise output by run
      # columns of interest  (wide version)
      out_cols <- c(paste("percent_wt_loss", c("to date", subperiods), sep = "_"),
        paste("change_intake", c("to date", subperiods), sep = "_") )
    
      # median intake reduction and percent weight loss by age, scenario, period
      out_age <- expand.grid(run = runs$run, scenario = scenarios, 
        period = c("to date", subperiods), 
        age_cat = c(unique(df_ad$age_cat), "all"))
      out_age[, c("percent_wt_loss", "change_intake")] <- NA
      out_age$scenario <- as.character(out_age$scenario)
      out_age$period <- as.character(out_age$period)
      out_age$age_cat <- as.character(out_age$age_cat)
      
      # median percent weight loss by scenario, period and day
      out_daily <- expand.grid(run = runs$run, scenario = scenarios, 
        date = timeline$date)
      out_daily <- merge(out_daily, timeline[, c("date", "period")], 
        by = "date", all.x = TRUE)
      out_daily[, c("percent_wt_loss", "change_intake")] <- NA
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)
    
#...............................................................................  
### Estimating intake reduction and weight loss
  # by scenario and day in the timeline, for each simulation run
#...............................................................................
  
  #...................................      
  ## Initialise state matrices accordingly

    # Intake reduction per day in timeline
    change_intake <- matrix(NA, nrow = nrow(df_ad), ncol = nrow(timeline) )
    colnames(change_intake) <- timeline$date
  
    # Weight per day in timeline
    wt_now <- matrix(NA, nrow = nrow(df_ad), ncol = nrow(timeline) )
    colnames(wt_now) <- timeline$date


#(loop starts here)
for (run_i in 1:max(runs$run)) {  

  #...................................      
  ## Preparations
    
    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
    # Reset time-changing objects
    change_intake[, ] <- NA
    wt_now[, ] <- NA
    df_ad$wt_now <- df_ad$weight
  
  #...................................      
  ## Select random quantities needed for run
    
    # Identify random number from [0,1]
    rx <- runs[run_i, "rx"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  
    
    # Select random food aid quantities per month to date; add to timeline
    aid <- aid_to_date[which(aid_to_date$run == run_i), c("date", "aid")]
    timeline_run_i <- merge(timeline, aid, by = "date", all.x = TRUE)
    
    # Select random proportions coming from different non-aid sources
      # select values by month
      non_aid_pars$prop <- non_aid_pars$min + 
        (1 - rx) * (non_aid_pars$max - non_aid_pars$min)
    
      # sum contributions from stocks and agriculture
      x <- aggregate(non_aid_pars$prop, 
        by = list(month_start = non_aid_pars$month_start), FUN = sum)
      colnames(x) <- c("month_start", "prop")
      
      # transfer to the daily timeline
      for (i in 1:nrow(x)) {
        if (i < nrow(x)) {
          timeline_run_i[which(timeline_run_i$date %in% 
            as.Date(as.Date(x[i, "month_start"]) :
            as.Date(x[i+1, "month_start"] - 1))), "prop"] <- x[i, "prop"]
        }
        
        if (i == nrow(x)) {
          timeline_run_i[which(timeline_run_i$date %in% 
            as.Date(as.Date(x[i, "month_start"]) :
            as.Date(date_end))), "prop"] <- x[i, "prop"]
        }  
      }
    
    # Select random scenario values of food intake during projection subperiods
    scenario_pars$intake <- intake_target_adj * (scenario_pars$min + (1 - rx) * 
      (scenario_pars$max - scenario_pars$min) )
  
  #...................................      
  ## Estimate intake for period to date
    
    # Compute intake per month in period to date
      # intake is either the baseline or the sum of food aid and other sources,
        # whichever is lower
      # first compute proportion of baseline intake met by stocks or agriculture
      x <- df_ad$intake_baseline %*% t(timeline_run_i$prop)
      
      # then add food aid
      x <- sweep(x, 2, timeline_run_i$aid, "+")
      
      # then work out the minimum between baseline intake and current intake
      x <- pmin(x, df_ad$intake_baseline)

    # Compute intake reduction per day
    change_intake <- sweep(x, 1, df_ad$intake_baseline, "-")

  #...................................      
  ## Estimate daily evolution of weight to date based on mechanistic model
  for (i  in 1:nrow(subset(timeline_run_i, period == "to_date"))) {
      
    # Update resting metabolic date
    df_ad$rmr <- apply(df_ad, 1, f_rmr)
    
    # Update change in intake
    df_ad$change_intake <- change_intake[, i]
      
    # Update instantaneous weight
    df_ad$wt_now <- df_ad$wt_now + apply(df_ad, 1, f_hall)
    
    # Store new end-of-day weight
    wt_now[, i] <- df_ad$wt_now
          
  }  
    # Store weight at end of period to date
    df_ad$wt_to_date <- df_ad$wt_now
    
  #...................................      
  ## Project future intake and weight loss by scenario
    
  for (i in scenarios) {
    
    # Reset time-changing objects
    change_intake[, days] <- NA
    wt_now[, days] <- NA
    df_ad$wt_now <- df_ad$wt_to_date

    # Add to the timeline food aid during projection period
    for (j in subperiods) {
      timeline_run_i[which(timeline_run_i$period == 
        names(subperiods)[subperiods == j] ), "aid"] <-
        scenario_pars[which(scenario_pars$scenario == i & 
          scenario_pars$period == j), "intake"]
    }
    
    # Compute intake per month during projection period
      # intake is either the baseline or the sum of food aid and other sources,
        # whichever is lower
      # first compute proportion of baseline intake met by stocks or agriculture
      x <- df_ad$intake_baseline %*% t(timeline_run_i[days, "prop"])
      
      # then add food aid
      x <- sweep(x, 2, timeline_run_i[days, "aid"], "+")
      
      # # then work out the minimum between baseline intake and current intake
      # x <- pmin(x, df_ad$intake_baseline)
      #   # not run, as assume people might compensate by eating more
      
    # Compute intake reduction per day
    change_intake[, days] <- sweep(x, 1, df_ad$intake_baseline, "-")
    
    # Estimate daily evolution of weight to date based on mechanistic model
    for (j  in days) {
        
      # Update resting metabolic date
      df_ad$rmr <- apply(df_ad, 1, f_rmr)
      
      # Update change in intake
      df_ad$change_intake <- change_intake[, j]
        
      # Update instantaneous weight
      df_ad$wt_now <- df_ad$wt_now + apply(df_ad, 1, f_hall)
      
      # Store new end-of-day weight
      wt_now[, j] <- df_ad$wt_now
    }
    
    # Output results of run by age, for this scenario
      # weight: restrict to dates of interest
      x <- wt_now[, days_out]
      
      # compute proportion of weight lost
      x <- 1 - (x / df_ad$weight)
      
      # intake reduction: compute means at dates of interest
      for (j in days_out) { x <- cbind(x, rowMeans(change_intake[, 1:j])) }
      
      # add age categories and survey weights
      x <- data.frame(df_ad[, c("age_cat", "svy_wt")], x)
      colnames(x) <- c("age_cat", "svy_wt", out_cols)
      
      # compute weighted means by age group
        # (weight only matters if using aggregated dataset)
      out_i <- by(x, x$age_cat, function(x) {
        apply(x[, out_cols], 2, weighted.mean, w = x$svy_wt)} )
      out_i <- data.frame(unique(x$age_cat), do.call(rbind, out_i))
      colnames(out_i) <- c("age_cat", out_cols)
      
      # add overall weighted mean
      out_i <-rbind(out_i, c("all", apply(x[, out_cols], 2, weighted.mean,
        x$svy_wt) ) )
      
      # reshape long
      out_i <- reshape(out_i, direction = "long", timevar = "period",
        varying = list(percent_wt_loss = out_cols[1:3], 
          change_intake = out_cols[4:6]),
        v.names = c("percent_wt_loss", "change_intake"),
        times = c("to date", subperiods), idvar = "age_cat")
      out_i <- out_i[order(out_i$period, out_i$age_cat), ]
  
      # add to overall output
      out_age <- out_age[order(out_age$run, out_age$scenario,
        out_age$period, out_age$age_cat), ]  
      out_age[which(out_age$run == run_i & out_age$scenario == i), 
        c("age_cat", "period", "percent_wt_loss", "change_intake")] <- out_i
      
    # Output results of run by day (all ages), for this scenario
      # compute daily percent weight change
      x <- 1 - (wt_now / df_ad$weight)
      
      # aggregate by day
      out_i <- data.frame(percent_wt_loss = apply(x, 2, weighted.mean,
        df_ad$svy_wt), change_intake = NA)
      
      # compute mean intake change by day
      x <- apply(change_intake, 2, weighted.mean, df_ad$svy_wt)
      out_i$change_intake <- cumsum(x) / seq_along(x)
      
      # add to overall output
      out_daily <- out_daily[order(out_daily$run, out_daily$scenario,
        out_daily$date), ] 
      out_daily[which(out_daily$run == run_i & out_daily$scenario == i), 
        c("percent_wt_loss", "change_intake")] <- out_i
  }
      
}  # close run_i loop
close(pb)  


#...............................................................................  
### Outputting and visualising results
#...............................................................................

  #...................................      
  ## Compute distribution of runs by age group and overall
    
    # Write raw output
    write_rds(out_age,
      paste(dir_path,"outputs/", "out_wt_loss_age_all_runs.rds", sep=""))
    write_rds(out_daily, 
      paste(dir_path,"outputs/","out_wt_loss_daily_all_runs.rds", sep=""))

    # Compute mean, median, 95% percentile intervals of all estimated quantities
    for(i in c("percent_wt_loss", "change_intake")) 
     {out_age[, i] <- as.numeric(out_age[, i])}
    agg <- aggregate(out_age[, c("percent_wt_loss", "change_intake")],
      by = out_age[, c("scenario", "period", "age_cat")],
      FUN = function(x){return(c(mean(x), quantile(x, c(0.50, 0.025, 0.975))))})

    # Output
    write.csv(agg, paste(dir_path, "outputs/","out_wt_loss_intake_adults.csv", 
      sep=""), row.names = FALSE)
    
    # Output actual runs (all ages only), for sampling in subsequent script
    out_all <- out_age[which(out_age$age_cat == "all"), c("run", "scenario",
      "period", "percent_wt_loss")]
    write_rds(out_all, paste(dir_path, "outputs/",
      "out_wt_loss_adults_all_runs.rds", sep=""))
    
    
  #...................................      
  ## Plot daily evolution, by scenario (all age groups)
  
    # Aggregate daily output into mean and 95% interval, by scenario and date
    agg <- aggregate(out_daily[, c("percent_wt_loss", "change_intake")],
      by = out_daily[, c("scenario", "date")], 
      FUN = function(x){return(c(mean(x), quantile(x, c(0.50, 0.025, 0.975))))})
    agg <- data.frame(agg[, c("scenario", "date")], unlist(agg$percent_wt_loss),
      unlist(agg$change_intake))
    colnames(agg) <- c("scenario", "date", 
      paste("percent_wt_loss", c("mean", "median", "lci", "uci"), sep = "_"),
      paste("change_intake", c("mean", "median", "lci", "uci"), sep = "_") )
    x <- grep("percent_wt_loss", colnames(agg))
    agg[, x] <- -agg[, x]
    
    # Split to date period from projection-scenario periods
    x <- subset(agg, date < date_start & scenario == "ceasefire")
    x$scenario <- "to date"
    agg <- subset(agg, date >= date_start)
    agg <- rbind(agg, x)
    agg$scenario <- factor(agg$scenario, levels = c("to date", "ceasefire",
      "status quo", "escalation"))

    # Plot weight loss
    ggplot(data = agg, aes(x = date, y = percent_wt_loss_mean, 
      colour = scenario, fill = scenario, shape = scenario)) +
      geom_line() +
      geom_point(size = 0.75) +
      geom_ribbon(aes(ymin = percent_wt_loss_lci, ymax = percent_wt_loss_uci),
        alpha = 0.1, colour = NA) +
      theme_bw() +
      scale_colour_manual(values = palette_periods[c(2,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(2,3,4,5)]) +
      scale_shape_manual(values = c(15,16,17,18)) +
      scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
      scale_y_continuous("cumulative weight loss as percent of baseline",
        labels = percent, breaks = seq(-0.5, 0.5, by = 0.05)) +
      geom_vline(aes(xintercept = date_start), linetype = "11",
        colour = "grey70", linewidth = 0.75) +
      geom_vline(aes(xintercept = date_mid), linetype = "11",
        colour = "grey70", linewidth = 0.75) +
      annotate("text", x = timeline[days_out[2], "date"], y = 0.075, 
        label = stringr::str_wrap("projection months 1-3", 10)) +
      annotate("text", x = timeline[days_out[3], "date"], y = 0.075, 
        label = stringr::str_wrap("projection months 4-6", 10)) +
      theme(legend.position = "top", legend.title = element_blank())
    
    ggsave(paste(dir_path, "outputs/", "adult_daily_wt_loss.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)       
          
#...............................................................................  
### ENDS
#...............................................................................

