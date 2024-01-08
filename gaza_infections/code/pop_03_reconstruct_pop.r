#...............................................................................
### ++++++++++ RECONSTRUCTING POPULATION DENOMINATORS IN SOMALIA +++++++++++ ###
#...............................................................................

#...............................................................................
## ---- R SCRIPT TO RECONSTRUCT POPULATION WHILE PROPAGATING UNCERTAINTY ---- ##
#...............................................................................

                          # LSHTM, SIMAD University (August 2023)
                          # francesco.checchi@lshtm_ac.uk 



#...............................................................................  
### Estimating the proportion of children under 5y from SMART survey data
#...............................................................................

  #.........................................
  ## Prepare household survey data for fitting
    
    # Restrict to non-missing observations within period of interest
    df <- na.omit(hh_obs)
    df <- subset(df, year %in% c(min(ts$y) : max(ts$y)) )
    
    # Remove a handful of observations that have zero people
    df <- subset(df, n > 0)
    
    # Deal with a handful where n_u5 > n (set these to <= n)
    df$n_u5 <- ifelse(df$n_u5 > df$n, df$n, df$n_u5)
    
    # Create / modify other necessary variables
    df$n_5plus <- df$n - df$n_u5
    df$district <- factor(df$district)
    breaks_biennium <- c(min(ts$y), seq(min(ts$y) + 3, 
      max(ts$y) + 1, by = 2))
    df$biennium <- cut(df$year, breaks = breaks_biennium, 
      include.lowest = TRUE, right = FALSE, ordered_result = TRUE,
      labels = paste(breaks_biennium[1:(length(breaks_biennium) - 1)], " to ", 
        breaks_biennium[2:(length(breaks_biennium))] - 1, sep = "") )
    table(df[, c("year", "biennium")])
  
  
  #.........................................
  ## Fit and evaluate a mixed logit proportion additive model
    # (biennium = single predictor; district = random effect) 
    
    # Fit model
    fit <- mgcv::bam(formula = cbind(n_u5, n_5plus) ~ biennium + 
      s(district, bs ="re"), data = df, family = quasibinomial() )
    
    # Visualise summary
    summary(fit)
    model_parameters(fit, exponentiate = TRUE)
    
    # Look at model diagnostics
    if (any(class(fit) %in% c("gam", "bam")) ) {mgcv::plot.gam(fit)}
  
    # Compare fitted predictions to observations  
      # observations
      df$prop_u5_obs <- df$n_u5 / df$n
      
      # predictions and 95%CI
      df$prop_u5_pred <- predict(fit, type = "response")
      x <- predict(fit, se.fit = TRUE)
      x_lci <- x$fit - 1.96 * x$se.fit
      df$prop_u5_lci <- exp(x_lci) / (1 + exp(x_lci)) 
      x_uci <- x$fit + 1.96 * x$se.fit
      df$prop_u5_uci <- exp(x_uci) / (1 + exp(x_uci)) 
    
      # aggregate to district and year
      df_agg <- aggregate(df[, c("prop_u5_obs", "prop_u5_pred", 
        "prop_u5_lci", "prop_u5_uci")],
        by = df[, c("region", "district", "biennium")], FUN = mean)
      
      # any bias?
      mean(df_agg$prop_u5_obs - df_agg$prop_u5_pred) # negligible

  #.........................................
  ## Predict under 5 proportion for all districts and biennia

    # Prepare prediction dataset
    ts_y <- unique(ts[, c("region", "district", "y")])
    colnames(ts_y)[colnames(ts_y) == "y"] <- "year"
    ts_y$biennium <- cut(ts_y$year, breaks = breaks_biennium, 
      include.lowest = TRUE, right = FALSE, ordered_result = TRUE,
      labels = paste(breaks_biennium[1:(length(breaks_biennium) - 1)], " to ", 
        breaks_biennium[2:(length(breaks_biennium))] - 1, sep = "") )

    # Predict for all districts and biennia
    ts_y$prop_u5_pred <- predict(fit, type = "response", newdata = ts_y,
      allow.new.levels = TRUE)
    x <- predict(fit, type = "link", newdata = ts_y, se.fit = TRUE,
      allow.new.levels = TRUE)
    ts_y$prop_u5_pred_link <- x$fit
    ts_y$prop_u5_pred_se <- x$se.fit
      
    # Graph evolution of predicted under 5 proportion by year
    plot <- ggplot(data = ts_y, aes(x = biennium, y = prop_u5_pred)) +
      geom_boxplot(colour = palette_cb[6], alpha = 0.7) +
      theme_bw() +
      scale_y_continuous("percentage of population aged < 5 years", 
        labels = label_percent(), breaks = seq(0, 0.50, by = 0.02)) +
      scale_x_discrete("period")
    ggsave(paste(dir_path, "/output/som_out_pop_u5prop_pred.png", sep =""),
      dpi = "print", units = "cm", height = 13, width = 20)
    

  #.........................................
  ## Generate many random values of under 5 proportion, for reconstruction below
  
    # Generate random values from log(estimate) and its standard error
    x <- replicate(runs_final, {
        rnorm(nrow(ts_y), mean = ts_y$prop_u5_pred_link, 
          sd = ts_y$prop_u5_pred_se)
      }
    )
    x <- exp(x) / (1 + exp(x))
    
    # Prepare dataset of random values by district-year-month, for reconstruction
    x <- cbind(ts_y[, c("district", "year")], x)
    colnames(x)[colnames(x) == "year"] <- "y"
    u5_prop_rand <- merge(ts, x, by = c("district", "y"), all.x = TRUE)
    u5_prop_rand <-u5_prop_rand[order(u5_prop_rand$district, u5_prop_rand$tm), ]
        
    
#...............................................................................  
### Estimating population by district-month in multiple stochastic runs
#...............................................................................

  #.........................................
  ## Prepare for stochastic runs

    # If the smoothed likelihood surfaces need to be loaded...
    if (! exists("out_smooth")) {out_smooth <- 
      read.csv(paste(dir_path, "output/som_pop_out_smooth.csv", sep = "")) }

    # Initialise dataframe of strata and their return parameters
    stratum_pars <- data.frame(stratum = strata, prop_stay = NA, ret_time = NA)
    stratum_pars <- stratum_pars[order(stratum_pars$stratum), ]
    
    # # Remove polio 2018 from pop_sources to be sampled from
    # pop_sources2 <- subset(pop_sources, worksheet != "pop_polio_2018")
    # pop_sources2$quality_prob <- cumsum(pop_sources2$quality_score) /
    #   max(cumsum(pop_sources2$quality_score))
 
    # Initialise output
    out_pop <- c()
    
  #.........................................
  ## Implement stochastic runs
  
  for (i in 1:runs_final) {
    
    # Progress statement
    print(paste("now working on run ", i, " of ", runs_final, sep = ""))
    
    # Select random population source
    x <- findInterval(runif(1), pop_sources$quality_prob,
      all.inside = FALSE) + 1
    rand_source <- pop_sources[x, "worksheet"]
    out_smooth_i <- subset(out_smooth, pop_source == rand_source)
    
    # Select random set of return parameters from their likelihood surface
      # for each stratum
    for (j in stratum_pars$stratum) {
      # subset likelihood surface
      out_smooth_ij <- subset(out_smooth_i, stratum == j)
      
      # select random parameter values from likelihood surface
      x <- findInterval(runif(1), out_smooth_ij$prob_cum) + 1
      stratum_pars[which(stratum_pars$stratum == j), 
        c("prop_stay", "ret_time")] <-
        out_smooth_ij[x, c("prop_stay", "ret_time")]
    }
    
    # Reconstruct populations
    pop_i <- f_pop(which_source_f = rand_source, 
      pars_f = as.vector(t(stratum_pars[, c("prop_stay", "ret_time")])) )
    pop_i$run <- i
    pop_i$pop_source <- rand_source
    
    # Add random value of the population under 5y
    pop_i$pop_u5 <- pop_i$pop * u5_prop_rand[, as.character(i)]
    
    # Output
    out_pop <- rbind(out_pop, pop_i)
  }  


    
#...............................................................................  
### Managing reconstruction output
#...............................................................................

  #.........................................
  ## Track success of reconstruction runs

    # Add proportion of IDPs
    out_pop$prop_idp <- out_pop$n_idp / out_pop$pop
    
    # Track outcome of each run in terms of completeness and implausible values
      # create tracking dataframe
      track <- expand.grid(admin2$district, 1:runs_final)
      colnames(track) <- c("district", "run")
      track <- track[order(track$run, track$district), ]
      
      # for which districts is pop 100% complete across time, for a given run?
      complete_pop <- aggregate(out_pop$pop, 
        by = out_pop[, c("district", "run")],FUN = function(x) {!any(is.na(x))})
      colnames(complete_pop) <- c("district", "run", "pop_complete")  

      # for which districts is n_idp 100% complete across time, for a given run?
      complete_n_idp <- aggregate(out_pop$n_idp, 
        by = out_pop[, c("district", "run")],FUN = function(x) {!any(is.na(x))})
      colnames(complete_n_idp) <- c("district", "run", "n_idp_complete")  

      # for which districts is pop always positive, for a given run?
      plausible_pop <- aggregate(out_pop$pop, 
        by = out_pop[, c("district", "run")], FUN = function(x) {!(any(x) < 0)})
      colnames(plausible_pop) <- c("district", "run", "pop_plausible")  

      # for which districts is prop_idp always >0% and <= 100%, for a given run?
      plausible_prop_idp <- aggregate(out_pop$prop_idp, 
        by = out_pop[, c("district", "run")], FUN = function(x) 
          {all(between(na.omit(x), 0, 1))})
      colnames(plausible_prop_idp) <- c("district", "run", "prop_idp_plausible")  
      
      # merge all the above into one    
      track <- Reduce(function(df1, df2) {merge(df1, df2, 
        by = c("district", "run"), all = TRUE)},list(track, complete_pop, 
          complete_n_idp, plausible_pop,  plausible_prop_idp))

      # tabulate run success by district
      which_cols <- colnames(track)[! colnames(track) %in% c("district", "run")]
      x <- aggregate(track[, which_cols], by = list(track$district), 
        FUN = mean, na.rm = TRUE)
      colnames(x) <- c("district", paste("prop_", which_cols, sep = ""))
      write.csv(x, paste(dir_path, "/output/som_out_pop_track_runs.csv", 
        sep = ""), row.names = FALSE)
      prop.table(table(track$pop_complete))
      prop.table(table(track$n_idp_complete))
      prop.table(table(track$pop_plausible))
      prop.table(table(track$prop_idp_plausible))
      
      # merge tracking with reconstruction output
      out_pop <- merge(out_pop, track, by = c("district", "run"), all.x = TRUE)
      
      # save raw reconstruction output
      saveRDS(out_pop, paste(dir_path, "/output/som_out_pop_runs_raw.rds", 
        sep = ""), compress = TRUE)
      
    # Clean up heavy objects to free up memory
    rm(complete_n_idp, complete_pop, df, hh_obs, out_smooth,
      out_smooth_i, out_smooth_ij, plausible_pop, plausible_prop_idp, plot,
      track, u5_prop_rand)
      
  #.........................................
  ## Restrict dataset and add further variables
    
    # Load reconstruction output if not already in memory
    if (! exists("out_pop")) {out_pop <- readRDS(paste(dir_path, 
      "/output/som_out_pop_runs_raw.rds", sep = ""))}      
    
    # Restrict output to runs with 100% completeness
    out_pop$discard <- apply(out_pop[, c("pop_complete", "n_idp_complete")], 1, 
      function (x) {any(!x)})
    table(out_pop$discard)
    pop <- out_pop
    pop <- subset(pop, discard == FALSE)
    rm(out_pop)
              
    # Restrict to needed variables
    pop <- pop[, c("admin0", "region", "district", "tm", "pop", "pop_u5",
      "n_idp", "prop_idp", "run")]

    # Add dates
    pop <- merge(pop, unique(ts[, c("tm", "m", "y", "date")]), by = "tm",
      all.x = TRUE)
    
    # Add IDP departures and arrivals from PRMN matrix
      # departures
      dep <- aggregate(prmn$n_idp, by = prmn[, c("district_dep", "tm")],
        FUN = sum)
      colnames(dep) <- c("district", "tm", "n_dep")
      pop <- merge(pop, dep, by = c("district", "tm"), all.x = TRUE)
      
      # arrivals
      arr <- aggregate(prmn$n_idp, by = prmn[, c("district_arr", "tm")],
        FUN = sum)
      colnames(arr) <- c("district", "tm", "n_arr")
      pop <- merge(pop, arr, by = c("district", "tm"), all.x = TRUE)
      
      # compute rates per 1000 person-months
      pop$dep_rate <- pop$n_dep * 1000 / pop$pop
      pop$arr_rate <- pop$n_arr * 1000 / pop$pop
    
    # Add alternative population under 5 (based on SHNS survey, 2020)  
    pop$pop_u5_alt <- pop$pop * demog_pars[demog_pars$parameter == "u5_prop",
      "value"]
          
    # Make final polishes
      # reorder variables
      pop <- pop[, c("run", "admin0", "region", "district", "tm", "y", "m",
        "date", "pop", "pop_u5", "pop_u5_alt", "n_idp", "n_dep", "n_arr",
        "prop_idp", "dep_rate", "arr_rate")]
      pop <- pop[order(pop$run, pop$district, pop$tm), ]
      
    # Save  
    saveRDS(pop, file = paste(dir_path, "/output/som_out_pop_runs_clean.rds", 
      sep =""), compress = TRUE)
                    

#...............................................................................  
### Estimating and visualising population median estimates and CIs
#...............................................................................

    # Load reconstruction output if not already in memory
    if (! exists("pop")) {pop <- readRDS(paste(dir_path, 
      "/output/som_out_pop_runs_clean.rds", sep = ""))}      
    
  #.........................................
  ## Compute mean, median and 95% percentile estimates: by district-month
    
    # Specify columns of interest
    which_cols <-c("pop", "pop_u5", "pop_u5_alt", "n_idp", "prop_idp",
      "dep_rate", "arr_rate")
          
    # Aggregate to district-month
    pop_dis <- aggregate(pop[, which_cols], 
      by = pop[, c("region", "district", "tm", "y", "m", "date")], 
      FUN = function(x) {c(mean(x[is.finite(x)], na.rm = TRUE), 
        quantile(x[is.finite(x)], c(0.50, 0.025, 0.975), na.rm = TRUE))})    
    pop_dis <- do.call(cbind.data.frame, pop_dis)
    colnames(pop_dis) <- c("region", "district", "tm", "y", "m", "date",
      paste(rep(which_cols, each= 4), c("mean", "500", "025", "975"), sep ="_"))

    # Save best estimates as output for computing predictor rate values
      # select best estimates
      pop_best <- pop_dis[, c("district", "tm", "y", "m", "pop_mean",
        "pop_u5_mean", "pop_u5_alt_mean", "prop_idp_mean", "dep_rate_mean", 
        "arr_rate_mean")] 
      colnames(pop_best) <- gsub("_mean", "", colnames(pop_best))
      
      # round
      pop_best[, c("pop", "pop_u5", "pop_u5_alt")] <- 
        round(pop_best[, c("pop", "pop_u5", "pop_u5_alt")], 0)
      
      # adjust a few remaining instances of implausible proportion of IDPs
      pop_best$prop_idp <- ifelse(pop_best$prop_idp > 1, 1, pop_best$prop_idp)
      pop_best$prop_idp <- ifelse(pop_best$prop_idp < 0, 0, pop_best$prop_idp)
      
      # write file
      write.csv(pop_best, file = paste(dir_path, "/output/som_out_pop_best.csv", 
        sep =""), row.names = FALSE)
      
    # For population and population under 5y, compute an empirical distribution
      # function, to be used to compute death tolls
    pop_cedf <- aggregate(pop[, c("pop", "pop_u5", "pop_u5_alt")], 
      by = pop[, c("region", "district", "tm", "y", "m", "date")], 
      FUN = function(x) {ecdf(x[is.finite(x)])})
    saveRDS(pop_cedf, file = paste(dir_path, "/output/som_out_pop_cedfs.rds", 
      sep =""), compress = TRUE)

    # By region-month 
    pop_reg <- aggregate(pop[, c("pop", "n_idp")],
      by = pop[, c("run", "region", "tm", "date")], FUN = sum) 
    pop_reg$prop_idp <- pop_reg$n_idp / pop_reg$pop
    pop_reg <- aggregate(pop_reg[, c("pop", "prop_idp")], 
      by = pop_reg[, c("region", "tm", "date")], 
      FUN = function(x) {c(mean(x[is.finite(x)], na.rm = TRUE), 
        quantile(x[is.finite(x)], c(0.50, 0.025, 0.975), na.rm = TRUE ) )})    
    pop_reg <- do.call(cbind.data.frame, pop_reg)
    colnames(pop_reg) <- c("region", "tm", "date",
      paste(rep(c("pop", "prop_idp"), each = 4), 
        c("mean", "500", "025", "975"), sep = "_") )
      
    # By country-month
    pop_nat <- aggregate(pop[, c("pop", "n_idp")],
      by = pop[, c("run", "tm", "date")], FUN = sum)
    pop_nat$prop_idp <- pop_nat$n_idp / pop_nat$pop
    pop_nat <- aggregate(pop_nat[, c("pop", "prop_idp")], 
      by = pop_nat[, c("tm", "date")], 
      FUN = function(x) {c(mean(x[is.finite(x)], na.rm = TRUE), 
        quantile(x[is.finite(x)], c(0.50, 0.025, 0.975), na.rm = TRUE ) )})    
    pop_nat <- do.call(cbind.data.frame, pop_nat)
    colnames(pop_nat) <- c("tm", "date",
      paste(rep(c("pop", "prop_idp"), each = 4),
        c("mean", "500", "025", "975"), sep = "_") )

    
  #.........................................
  ## Visualise evolution of total population
    
    # By region-month    
    plot <- plot <- ggplot(data = pop_reg, aes(x = date, y = pop_mean)) +
      geom_line(alpha = 0.8, colour = palette_cb[6]) +
      geom_ribbon(aes(ymin = pop_025, ymax = pop_975), alpha = 0.2,
        fill = palette_cb[6]) +
      scale_x_date("date") +
      scale_y_continuous("estimated population", labels = label_comma()) +
      theme_bw() +
      facet_wrap(. ~ region, scales = "free", ncol = 3)
    ggsave(paste(dir_path, "/output/som_out_pop_evol_region.png", sep =""),
      dpi = "print", units = "cm", height = 40, width = 30)
    
    # By country-month    
    plot <- ggplot(data = pop_nat, aes(x = date, y = pop_mean)) +
      geom_line(alpha = 0.8, colour = palette_cb[6]) +
      geom_ribbon(aes(ymin = pop_025, ymax = pop_975), alpha = 0.2,
        fill = palette_cb[6]) +
      scale_x_date("date") +
      scale_y_continuous("estimated population", labels = label_comma()) +
      theme_bw()
    ggsave(paste(dir_path, "/output/som_out_pop_evol_country.png", sep =""),
      dpi = "print", units = "cm", height = 13, width = 20)    
    
    
  #.........................................
  ## Visualise evolution of the proportion of IDPs
    
    # By region-month    
    plot <- ggplot(data = pop_reg, aes(x = date, y = prop_idp_mean)) +
      geom_line(alpha = 0.8, colour = palette_cb[7]) +
      geom_ribbon(aes(ymin = prop_idp_025, ymax = prop_idp_975), alpha = 0.2,
        fill = palette_cb[7]) +
      scale_x_date("date") +
      scale_y_continuous("estimated percentage of IDPs out of total population",
        labels = label_percent()) +
      theme_bw() +
      facet_wrap(. ~ region, ncol = 3)
    ggsave(paste(dir_path, "/output/som_out_pop_evol_prop_idp_region.png", 
      sep = ""), dpi = "print", units = "cm", height = 40, width = 30)
    
    # By country-month    
    plot <- ggplot(data = pop_nat, aes(x = date, y = prop_idp_mean)) +
      geom_line(alpha = 0.8, colour = palette_cb[7]) +
      geom_ribbon(aes(ymin = prop_idp_025, ymax = prop_idp_975), alpha = 0.2,
        fill = palette_cb[7]) +
      scale_x_date("date") +
      scale_y_continuous("estimated percentage of IDPs out of total population",
        labels = label_percent()) +
      theme_bw()
    ggsave(paste(dir_path, "/output/som_out_pop_evol_prop_idp_country.png",
      sep = ""), dpi = "print", units = "cm", height = 13, width = 20)    
    
          
#...............................................................................
### ENDS
#...............................................................................
  
  