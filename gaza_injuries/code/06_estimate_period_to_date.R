#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## ------- R SCRIPT TO ESTIMATE INJURY DEATHS DURING PERIOD TO DATE  -------- ##
#...............................................................................


#...............................................................................  
### Setting up timeline for each day in the period to date
#...............................................................................

  #...................................
  ## Prepare timeline of deaths

    # Initialise timeline - rows = time (t)
    ci_base <- data.frame(date = date_crisis : (date_start - 1), 
      period = "to date")
    ci_base$date <- as.Date(ci_base$date)
    
    # Add counted deaths per day (interpolated values)
    ci_base$d_counted <- daily[which(daily$date < date_start), "d_ipol"]

    # Add covariates of prop_counted model
    x <- daily
    x$dr <- x$d_ipol * 1000 / x$pop
    ci_base <- merge(ci_base, x[, c("date", "dr", "prop_sh_ipol")],all.x = TRUE)
    
    # Generate prediction and its SE for prop_counted, by day
    x <- predict(fit_pc, newdata = ci_base, se.fit = TRUE)
    ci_base$prop_counted_pred <- x$fit
    ci_base$prop_counted_pred_se <- x$se.fit

  #...................................
  ## Add deaths due to other causes (as estimated by us)

    # Read file
    proj <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
      "gaza_noninjury_to_date.xlsx", sep="")))
    
    # Aggregate across all modules and dates
    proj$d_other <- proj$d_female + proj$d_male
    proj <- aggregate(list(d_other = proj$d_other), 
      by = list(month_start = proj$month_start), FUN = sum)

    # Compute mean deaths per day in timeline
    ci_base$d_other <- NA
    for (i in 1:nrow(proj)) {
      t_start <- as.Date(proj[i, "month_start"])
      if (i == nrow(proj)) {t_end <- date_start - 1}
      if (i < nrow(proj)) {t_end <- proj[i+1, "month_start"] - 1}
      t_end <- as.Date(t_end)
      x <- which(ci_base$date %in% as.Date(t_start : t_end))
      ci_base[x, "d_other"] <- proj[i, "d_other"] / length(x)
    }
    
    
#...............................................................................  
### Implementing simulation
#...............................................................................

  #...................................
  ## Preparing for simulation
    
    # Initialise output
    out <- expand.grid(run = 1:runs, date = date_crisis : (date_start - 1))
    out$d_all <- NA
    out$date <- as.Date(out$date)
    
    # Add random proportion of non-injury deaths that were part of MoH count
    rx_d_other <- runif(runs, min = 0, max = 0.5)

    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)

  #...................................
  ## Implement simulation
  for (run_i in 1:runs) {
    
    # Update progress bar
    setTxtProgressBar(pb, run_i)  
    
    # Fresh cohort timeline
    ci_i <- ci_base
    
    # Generate random proportions counted for each day
    ci_i$prop_counted <- suppressWarnings(inv.logit(rnorm(nrow(ci_i), 
      mean = ci_i$prop_counted_pred, sd = ci_i$prop_counted_pred_se)))
    
    # Compute total deaths including uncounted
    ci_i$d_all <- ci_i$d_counted / ci_i$prop_counted
    
    # Subtract random deaths not due to injury during to date period
    ci_i$d_all <- ci_i$d_all - ci_i$d_other * rx_d_other[run_i]

    # Output
    out[which(out$run == run_i), "d_all"] <- ci_i$d_all
  }  
close(pb)             

  #...................................
  ## Distribute deaths by age and sex and save raw output

    # Distribute deaths by age and sex
    out <- merge(out, moh_d)  
    out$d_all <- out$prop * out$d_all

    # Save
    write_rds(out, paste0(dir_path, "outputs/", "out_to_date_all_runs.rds"))   
    
    
#...............................................................................  
### Visualising results
#...............................................................................

  #...................................
  ## Tabulate injury deaths by age and sex (before adjustment for multiple
      # causes of death)

    # Aggregate by age and sex
    stats <- c("mean", "lci", "uci")
    
    x1 <- aggregate(list(d_all = out$d_all), 
      by = out[, c("run", "age", "gender")], FUN = sum)
    tab <- aggregate(list(d_all = x1$d_all), 
      by = x1[, c("age", "gender")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    tab <- data.frame(tab[, c("age", "gender")], tab$d_all)
    colnames(tab) <- c("age", "gender", stats)
         
    # Write raw output
    write.csv(tab, paste(dir_path, "outputs/out_tab_age_gender_to_date.csv", 
      sep = "/"), row.names = FALSE)

  #...................................
  ## Tabulate injury deaths by age and month (as inputs to compute adjustment 
      # factors for multiple causes of death)

    # Aggregate by age and month (for computing adjustment factors)
    stats <- c("mean", "lci", "uci")
    month_starts <- as.Date(c(proj$month_start, date_start - 1))
    out$month_start <- NA
    for (i in 1:(length(month_starts) - 1)) {
      x <- as.Date(month_starts[i] : month_starts[i+1])
      out[which(out$date %in% x), "month_start"] <- month_starts[i]
    }
    out$month_start <- as.Date(out$month_start)
    
    x1 <- aggregate(list(d_all = out$d_all), 
      by = out[, c("run", "age", "month_start")], FUN = sum)
    tab <- aggregate(list(d_all = x1$d_all), 
      by = x1[, c("age", "month_start")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    tab <- data.frame(tab[, c("age", "month_start")], tab$d_all)
    colnames(tab) <- c("age", "month_start", stats)
         
    # Write raw output
    write.csv(tab, paste(dir_path, "outputs/out_tab_age_month_to_date.csv", 
      sep = "/"), row.names = FALSE)



#...............................................................................  
### ENDS
#...............................................................................      
      