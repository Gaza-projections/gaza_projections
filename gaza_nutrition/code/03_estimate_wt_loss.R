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
    for (i in c("todate", names(subperiods)) ) {
      df_ad[, paste("intake_reduction", i, sep = "_")] <- NA
      df_ad[, paste("percent_wt_loss", i, sep = "_")] <- NA
    }
    df_ad$intake_reduction <- NA
    df_ad$percent_wt_loss <- NA
    
  #...................................      
  ## Initialise or read other objects 
    
    # Read model of weight loss as a function of dietary intake
    fit <- read_rds(paste(dir_path, "outputs/", "wt_loss_model.rds", sep=""))  
    
    # Initialise output of each run
    out <- expand.grid(run = runs$run, scenario = scenarios, 
      age_cat = c("40 to 49yo", "50 to 59yo", "60 to 69yo", "70 to 79yo",
        "80+yo", "all"))
    for (i in c("to_date", names(subperiods)) ) {
      out[, paste(c("intake_reduction", "percent_wt_loss"), i, sep = "_")] <- NA
    }
    out <- out[order(out$run, out$scenario, out$age_cat), ]
        
#...............................................................................  
### Function to estimate weight loss in adults as percent of starting weight
  # based on weight loss - intake model and projected intake; one simulation run
#...............................................................................
  
# f_wl <- function(fit = fit, df_ad = df_ad, scenario_pars = scenario_pars,
#   todate_pars = todate_pars, rx = runif(1), todate_aid = todate_aid,
#   months_todate = months_todate, subperiods = subperiods) {

for (i in 1:nrow(runs)) {  

  #...................................      
  ## Select random quantities needed for run
    
    # Identify random number from [0,1]
    rx <- runs[i, "rx"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  
    
    # Select random food aid quantities per month to date
    aid <- todate_aid$min + (1 - rx) * (todate_aid$max - todate_aid$min)
    
    # Select random proportions coming from different non-aid sources, per month
    todate_pars$prop <- todate_pars$min + 
      (1 - rx) * (todate_pars$max - todate_pars$min)
    
    # Select random scenario values of food intake
    scenario_pars$intake <- 2100 * (scenario_pars$min + (1 - rx) * 
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
      df_ad$intake_reduction_todate <- df_ad$intake_reduction 
    
    # Estimate percent weight reduction to date from prediction and its SE
      # set months of duration
      df_ad$duration <- months_todate
      
      # predict
      x <- predict(fit, newdata = df_ad, se.fit = TRUE)
      
      # sample from prediction interval
      df_ad$percent_wt_loss <- exp(qnorm(rx, mean = x$fit, sd = x$se.fit)) *
        months_todate
      
      # store prediction
      df_ad$percent_wt_loss_todate <- df_ad$percent_wt_loss

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
        df_ad$intake_reduction <- (df_ad$intake_reduction_todate * months_todate 
          + (df_ad$intake_baseline - 
              scenario_pars[which(scenario_pars$scenario == i 
              & scenario_pars$period == subperiods[[j]]), "intake"]) *  
            months_project) / (months_todate + months_project)
      }
      
      if (j == "subperiod2") {
      df_ad$intake_reduction <- (df_ad$intake_reduction_todate * months_todate +
        (df_ad$intake_baseline - scenario_pars[which(scenario_pars$scenario == i 
          & scenario_pars$period == "subperiod1"), "intake"]) *  3 +
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
      df_ad[, paste("percent_weight_loss", j, sep = "_")] <- 
        df_ad$percent_wt_loss
      
    } 
    
    # Output results of run, for this scenario
      # compute medians by age group
      x <- c(grep("intake_reduction_", colnames(df_ad)),
        grep("percent_weight_loss_", colnames(df_ad))
      )
      out_i <- aggregate(df_ad[, x], by = list(age_cat = df_ad$age_cat),
        FUN = median)
      
      # add overall medians
      out_i <-rbind(out_i, apply(df_ad[, x], 2, median) )

  }
}  
  


#...............................................................................  
### ENDS
#...............................................................................

