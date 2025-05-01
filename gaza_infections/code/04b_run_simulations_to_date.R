#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO RUN SIMULATIONS FOR ENDEMIC INFECTIONS - PERIOD TO DATE -- ##
#...............................................................................

 
#...............................................................................
### Running stable-transmission simulations for to date period
#...............................................................................

# (run loop starts here)
with_progress({
  p <- progressor(steps = length(sim))
  out_ende <- future_lapply(sim, future.seed = TRUE, function(run_sim) {

    p()
  ## For each scenario...
  lapply(scenarios, function(i) {

    # Get parameter values for this run and scenario
    sim_pars <- run_sim[[i]][which(run_sim[[i]]$parameter %in% c("r0_rr", "cfr_rr")),
      c("disease", "parameter", "subperiod", "value_gen")]

    # Initialise fresh timeline
    timeline_runi_i <- timeline_ende
    
    # Add relative risks
      # transmissibility
      x <- sim_pars[which(sim_pars$parameter == "r0_rr"), 
        c("disease", "subperiod", "value_gen")]
      colnames(x) <- c("disease", "subperiod", "r0_rr")
      x <- subset(x, subperiod == "months 1 to 3")
      timeline_runi_i <- merge(timeline_runi_i, x[, c("disease", "r0_rr")],
        by = "disease", all.x = TRUE)
    
      # CFR
      x <- sim_pars[which(sim_pars$parameter == "cfr_rr"), 
        c("disease", "subperiod", "value_gen")]
      colnames(x) <- c("disease", "subperiod", "cfr_rr")
      x <- subset(x, subperiod == "months 1 to 3")
      timeline_runi_i <- merge(timeline_runi_i, x[, c("disease", "cfr_rr")],
        by = "disease", all.x = TRUE)
      
    # Generate a random number of infectious disease deaths by year
    d <- c()
    x <- predict(fit, newdata = 
      ende[which(ende$year %in% unique(timeline_ende$year)), ], se.fit = TRUE)
    for (j in 1:length(x$fit)) 
      {d[j] <- exp(qnorm(runif(1), x$fit[j], sd = x$se.fit[j]))}
    d_y <- data.frame(year = unique(timeline_ende$year), d_y = as.integer(d) )  
    
    # Distribute total deaths into each cause
    d_by <- unique(timeline_ende[, c("year", "disease", "route")])
    d_by <- merge(d_by, d_y, by = "year", all.x = TRUE)
    d_by$d <- NA
    for (j in 1:nrow(d_by)) {
      # COVID-19 deaths
      if (d_by[j, "disease"] == "COVID-19") {
        d_by[j, "d"] <- d_by[j, "d_y"] * 
          ende[which(ende$year == d_by[j, "year"]), "prop_covid"]
      }
      
      # other airborne droplet deaths
      if (d_by[j, "disease"] != "COVID-19" & 
        d_by[j, "route"] == "airborne-droplet") {
        d_by[j, "d"] <- d_by[j, "d_y"] * 
          ende[which(ende$year == d_by[j, "year"]), "prop_airborne"] *
          prop_d[which(prop_d$disease == d_by[j, "disease"]), "value_gen"]
      }
      
      # faecal-oral deaths
      if (d_by[j, "route"] == "faecal-oral") {
        d_by[j, "d"] <- d_by[j, "d_y"] *
          ende[which(ende$year == d_by[j, "year"]), "prop_faecal"] *
          prop_d[which(prop_d$disease == d_by[j, "disease"]), "value_gen"]
      }
    }
    
    # Now distribute by month of the year, based on seasonality
    d_m <- unique(timeline_ende[, c("disease", "year", "month")])
    d_m <- merge(d_m, d_by[, c("disease", "year", "d")], 
      by = c("disease", "year"), all.x = TRUE)
    d_m <- merge(d_m, w, by = c("disease", "month"), all.x = TRUE)
    d_m$d_m <- d_m$d * d_m$w

    # Now distribute by day
    timeline_runi_i <- merge(timeline_runi_i, 
      d_m[, c("disease", "year", "month", "d_m")],
      by = c("disease", "year", "month"), all.x = TRUE)
    timeline_runi_i$d_d <- timeline_runi_i$d_m / 
      days_in_month(timeline_runi_i$date)
      
    # Now distribute by age group
    timeline_runi_i$d_base <- timeline_runi_i$d_d * timeline_runi_i$prop_age 
  
    # Lastly, apply crisis relative risks
    timeline_runi_i$d_crisis <- timeline_runi_i$d_base * timeline_runi_i$r0_rr * 
      timeline_runi_i$cfr_rr
##### WILL NEED TO ADD CHANGING SUSCEPTIBILITY IN LATER EDITIONS    
    
    # Aggregate and collect output by subperiod
    x <- subset(timeline_runi_i, subperiod == "to date")
    x <- aggregate(x[, c("d_base", "d_crisis")], by =
      x[, c("route", "disease", "month", "year", "age")], FUN = sum)    
    x$scenario <- i
    x$run <- run_sim$id
    return(x)
  }) # (close i - scenario loop)
}) # (close run_i - run loop)
}) # (close withr for progress bar)

out_ende <- do.call(bind_rows, flatten(out_ende))

  #...................................      
  ## Tabulate deaths by scenario, age, month and year

    # Aggregate
    out_ende <- subset(out_ende, scenario == "status quo")
    out <- aggregate(out_ende[, grep("d_", colnames(out_ende))],
      by = out_ende[, c("age", "month", "year", "run")], FUN = sum)
    out <- aggregate(out[, grep("d_", colnames(out))],
      by = out[, c("age", "month", "year")], 
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))) )} )
    out <- data.frame(out[, c("age", "month", "year")],
      out$d_base, out$d_crisis)
    colnames(out) <- c("age", "month", "year", 
      paste("base", c("mean", "lci", "uci"), sep = "_"),
      paste("crisis", c("mean", "lci", "uci"), sep = "_")
    )

    # Generate start date of month
    out$month_start <- ymd(paste(out$year, out$month, "07", sep = "-"))
    
    # Save
    write.csv(out, paste(dir_path, "outputs/out_ende_to_date_by_age.csv", 
      sep = "/"), row.names = FALSE)    
  
    
  #...................................      
  ## Tabulate deaths by scenario, disease, month and year

    # Aggregate
    out_ende <- subset(out_ende, scenario == "status quo")
    out <- aggregate(out_ende[, grep("d_", colnames(out_ende))],
      by = out_ende[, c("disease", "month", "year", "run")], FUN = sum)
    out <- aggregate(out[, grep("d_", colnames(out))],
      by = out[, c("disease", "month", "year")], 
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))) )} )
    out <- data.frame(out[, c("disease", "month", "year")],
      out$d_base, out$d_crisis)
    colnames(out) <- c("disease", "month", "year", 
      paste("base", c("mean", "lci", "uci"), sep = "_"),
      paste("crisis", c("mean", "lci", "uci"), sep = "_")
    )

    # Generate start date of month
    out$month_start <- ymd(paste(out$year, out$month, "07", sep = "-"))
    
    # Save
    write.csv(out, paste(dir_path, "outputs/out_ende_to_date_by_disease.csv", 
      sep = "/"), row.names = FALSE)    
    
#...............................................................................   
### ENDS
#...............................................................................
