#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO FIT MODELS AND PREPARE OTHER COMPONENTS OF SIMULATION  --- ##
#...............................................................................

 

#...............................................................................  
### Visualising distributions of death rates
#...............................................................................

  #...................................      
  ## Explore distribution of daily death rate
  
    # As a distribution
    plot1 <- ggplot(data = daily, aes(x = dr)) +
      geom_histogram(alpha = 0.6, fill = palette_gen[6],
        colour = palette_gen[6]) +
      theme_bw() +
      scale_x_continuous("daily death rate per 1000 people", 
        breaks = seq(0, 0.5, 0.05)) +
      scale_y_continuous("frequency")
      
    # Over time
    daily$mean_since <- ifelse(daily$t_since_d > 1, "*", "")
    plot2 <- ggplot(data = daily, aes(x = date, y = dr)) +
      geom_col(colour = palette_gen[9], fill = palette_gen[9], alpha = 0.5) +
      theme_bw() +
      scale_x_date(breaks = "1 month") +
      scale_y_continuous("daily death rate per 1000 people") +
      geom_text(aes(x = date, y = dr, label = mean_since), size = 4,
        colour = "grey20",nudge_y = 0.005)

    # Cumulative death rate among general and UNRWA population, over time
    df_plot1 <- daily[, c("date", "dr_cum")]
    df_plot2 <- daily[, c("date", "dr_unrwa_cum")]
    colnames(df_plot2) <- colnames(df_plot1)
    df_plot <- rbind(df_plot1, df_plot2)
    df_plot$population <- c(rep("general", nrow(daily)), 
      rep("UNRWA", nrow(daily)) )
    
    plot3 <- ggplot(data = df_plot, aes(x = date, y = dr_cum,
      colour = population, linetype = population)) +
      geom_step(alpha = 0.8, linewidth = 1) +
      theme_bw() +
      scale_x_date(breaks = "1 month") +
      scale_y_continuous("cumulative death rate per 1000 people") +
      scale_colour_manual(values = palette_gen[c(9,3)]) +
      scale_linetype_manual(values = c("solid", "solid"))  +
      theme(legend.position = "top")
    
    # Combined
    ggarrange(plot1, plot2, plot3, labels = c("A", "B", "C"), ncol = 1,
      heights = c(1, 1, 1.5), vjust = c(1.5, 0, 1.5), align = "v")
    
    ggsave(paste(dir_path, 'outputs/', 
      "distribution_daily_dr_combi.png", sep = ""),
      dpi = "print", units = "cm", height = 25, width = 20)


#...............................................................................  
### Modelling the daily death and injury counts - for projections
#...............................................................................

  #...................................      
  ## Set reference periods for status quo and escalation scenarios
    
    # Set period for status quo scenario
    period_sq <- as.Date(as.Date("2023/10/15") : as.Date("2024/01/15"))
    
    # Figure out 30-day period with highest death toll (for escalation scenario)
      # compute rolling sum with window of 30 days (use interpolate deaths)
      daily$d_running <- rollsum(daily$d_ipol, k = 30, align = "left", 
        fill = c(NA, NA, NA))    
      
      # find start date with highest death toll in subsequent 30 days
      x <- daily[which.max(daily$d_running), "date"]
      
      # so period for escalation scenario is
      period_es <- as.Date(x : (x + 30))
    
    
  #...................................      
  ## Fit a null count model to daily injury count - used to project status quo
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_sq), c("date", "i", "ptime")]
    df <- na.omit(df)
    fit_isq <- glm.nb(data = df, i ~ 1 + offset(log(ptime)))
    summary(fit_isq)

    # Store logged coefficient and standard error, for use later
    est_isq <- summary(fit_isq)$coefficients[, c(1,2)]
    names(est_isq) <- c("mean", "se")    
      
  #...................................      
  ## Fit a count model to daily injury count - used to project escalation
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_es), c("date", "i", "ptime")]
    df <- na.omit(df)
    fit_ies <- glm.nb(data = df, i ~ 1 + offset(log(ptime)))
    summary(fit_ies)

    # Store logged coefficient and standard error, for use later
    est_ies <- summary(fit_ies)$coefficients[, c(1,2)]
    names(est_ies) <- c("mean", "se")    

  #...................................      
  ## Fit a null count model to daily death count - used to project status quo
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_sq), c("date", "d", "ptime")]
    df <- na.omit(df)
    fit_dsq <- glm.nb(data = df, d ~ 1 + offset(log(ptime)))
    summary(fit_dsq)

    # Store logged coefficient and standard error, for use later
    est_dsq <- summary(fit_dsq)$coefficients[, c(1,2)]
    names(est_dsq) <- c("mean", "se")    
      
  #...................................      
  ## Fit a count model to daily death count - used to project escalation
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_es), c("date", "d", "ptime")]
    df <- na.omit(df)
    fit_des <- glm.nb(data = df, d ~ 1 + offset(log(ptime)))
    summary(fit_des)

    # Store logged coefficient and standard error, for use later
    est_des <- summary(fit_des)$coefficients[, c(1,2)]
    names(est_des) <- c("mean", "se")    
      
    
#...............................................................................  
### Estimating the proportion of counted deaths (assumed to be = for injuries)
#...............................................................................

  #...................................      
  ## Fit a beta-distributed GAM model of general death rate vs UNRWA death rate
    
    # Fit model excluding period at the beginning when system was coping
    df <- na.omit(daily[which(daily$date > as.Date("2023-10-20")), 
      c("date", "time", "pc", "dr", "prop_sh_ipol")])
    # df$pc <- ifelse(df$pc >= 1, 0.99999, df$pc)

#    fit_pc <- betareg(data = df, pc ~ dr + prop_sh_ipol)
    fit_pc <- mgcv::gam(pc ~ s(dr) + s(prop_sh_ipol),data = df,family = betar())
    summary(fit_pc)

    # Visualise GAM model fit
    df$pc_pred <- predict(fit_pc, type = "response")
    x <- predict(fit_pc, se.fit = TRUE)
    df$pc_pred_lci <- inv.logit(x$fit - 1.96 * x$se.fit)
    df$pc_pred_uci <- inv.logit(x$fit + 1.96 * x$se.fit)

    ggplot(data = df, aes(x = date, y = pc)) +
      geom_point(alpha = 0.8, colour = palette_gen[3],
        fill = palette_gen[3]) +
      geom_line(aes(y = pc_pred), colour = palette_gen[13], linewidth = 1) +
      geom_ribbon(aes(ymin = pc_pred_lci, ymax = pc_pred_uci),
        alpha = 0.2, fill = palette_gen[13]) +
      theme_bw() +
      scale_y_continuous("ratio of general to UNRWA death rate") +
      scale_x_date("date", breaks = "1 month")

    ggsave(paste(dir_path, 'outputs/', "pc_model_fit.png", sep = ""),
      dpi = "print", units = "cm", height = 12, width = 22)

 
#...............................................................................  
### Figuring out age and gender distribution of deaths
#...............................................................................

  #...................................      
  ## Tabulate age and gender distribution of MoH-reported deaths
    
    # Tabulate
    moh_d <- na.omit(moh_d)
    moh_d <- subset(moh_d, gender != "unknown")
    table(moh_d[, c("age", "gender")])
    
    # Categorise age as wanted
    moh_d$age_cat <- cut(moh_d$age, breaks = c(
      0, 1, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 130), 
      include.lowest = FALSE, right = FALSE)
    moh_d$n <- 1
    
    # Aggregate by age category
    moh_d <- aggregate(list(n = moh_d$n), by = moh_d[, c("gender", 
      "age_cat")], FUN = sum)
    x <- data.frame(age_cat = levels(moh_d$age_cat), age = ages[2:length(ages)])
    moh_d <- merge(moh_d, x, by = "age_cat", all.x = TRUE)
      
    # Divide 0 year age bracket into neonates and other infants
    x <- subset(moh_d, age == "1 to 11mo")
    x1 <- x
    x1$age <- "1 to 11mo"
    x1$n <- round(x1$n * 11/12, 0)
    x$n <- round(x$n * 1/12, 0)
    x$age <- "0mo"
    moh_d <- subset(moh_d, age != "1 to 11mo")
    moh_d <- rbind(x, x1, moh_d)

    # Come up with proportions
    moh_d$prop <- moh_d$n / sum(moh_d$n)
    
    # Simplify
    moh_d$age <- factor(moh_d$age, levels = ages)
    moh_d$gender <- factor(moh_d$gender, levels = c("male", "female"))
    moh_d <- moh_d[, c("gender", "age", "n", "prop")]
    

#...............................................................................  
### Preparing basic cohort timeline to use for different steps
#...............................................................................

  #...................................
  ## Distribute CFR by time to injury death data from the US
  
    # Compute cumulative deaths in data
    surv$p_dcum <- cumsum(surv$p_d)

    # Distribute probability of dying over two years
    cfr_tau <- data.frame(day = 0:730, cfr = cfr_sq)
    cfr_tau <- merge(cfr_tau, surv, by = "day", all.x = TRUE)
    cfr_tau$cfr_tau <- approx(x = surv$day, y = surv$p_dcum, 
      xout = cfr_tau$day)$y
    cfr_tau$cfr_tau <- c(cfr_tau$cfr_tau[1], diff(cfr_tau$cfr_tau))

    # Distribute CFR (assume status quo CFR = same as 'to date' CFR)
    cfr_tau$cfr_tau <- cfr_tau$cfr * cfr_tau$cfr_tau
    cfr_tau <- cfr_tau$cfr_tau[2:nrow(cfr_tau)]
    
  #...................................
  ## Set up cohort timeline
    
    # Initialise timeline - rows = time (t), cols = longevity of injury (tau)
    ci_base <- data.frame(date = date_crisis : (date_end + 730), 
      period = "to date")
    ci_base$date <- as.Date(ci_base$date)
    
    # Work out periods
    ci_base$period <- ifelse(ci_base$date >= date_mid, subperiods[2], 
      ci_base$period)
    ci_base$period <- ifelse(ci_base$date %in% 
      as.Date(date_start : (date_mid - 1)), subperiods[1], ci_base$period)
    ci_base$period <- ifelse(ci_base$date > date_end, "post", ci_base$period)
    table(ci_base$period)    

    # Add counted injuries per day (interpolated values)
    ci_base$i_counted <- NA
    ci_base[which(ci_base$date < date_start), "i_counted"] <- 
      daily[which(daily$date < date_start), "i_ipol"]
    
    # Add counted deaths per day (interpolated values)
    ci_base$d_counted <- NA
    ci_base[which(ci_base$date < date_start), "d_counted"] <- 
      daily[which(daily$date < date_start), "d_ipol"]

    # Add deaths due to other causes (as estimated by us)
    ci_base$d_other <- sum(proj$d_other)
    
    # Initialise deaths due to wounds
    ci_base$d_dow <- NA
    
    # Add covariates of prop_counted model
    x <- daily
    x$dr <- x$d_ipol * 1000 / x$pop
    ci_base <- merge(ci_base, x[, c("date", "dr", "prop_sh_ipol")],all.x = TRUE)
    
    # Generate prediction and its SE for prop_counted, by day
    x <- predict(fit_pc, newdata = ci_base, se.fit = TRUE)
    ci_base$prop_counted_pred <- x$fit
    ci_base$prop_counted_pred_se <- x$se.fit
    
    # Create tau-specific number of deaths of wounds
    ci_base[, paste("d", 0:730, sep = "")] <- NA


#...............................................................................  
### Implementing a cohort to work out the proportion who die immediately
#...............................................................................

  #...................................
  ## Compute deaths among counted injuries, by tau (time since injury)
    # here, 'd0' columns means immediate deaths, 'd1' is same day but later
    
    # Initialise cohort
    ci_pm <- ci_base
    
    # Add daily CFR to each tau day
    ci_pm[, paste("d", 1:730, sep = "")] <- t(replicate(nrow(ci_pm), cfr_tau))
    
    # Deaths from wounds if none were immediate, by tau
    ci_pm[, paste("d", 1:730, sep = "")] <- 
      ci_pm[, paste("d", 1:730, sep = "")] * ci_pm$i_counted
    
  #...................................
  ## Compute deaths due to wounds over time

    # Lag each tau column to match with chronological time
    for (tau in 1:730) {
      ci_pm[, paste("d", tau, sep = "")] <- 
        dplyr::lag(ci_pm[, paste("d", tau, sep = "")], tau - 1)
    }
    
    # Compute deaths due to wounds among counted injuries
    ci_pm$d_dow <- rowSums(ci_pm[, paste("d", 1:730, sep = "")], na.rm = TRUE)
    
    # Compute deaths due to wounds that occur after the period 'to date'
      # if no injuries led to immediate death - see equations
    d_dow_after <- sum(ci_pm[which(ci_pm$period != "to date"), "d_dow"],
      na.rm = TRUE)


  #...................................
  ## Solve for p_m (proportion of injuries that result in immediate death)
      # but probabilistically, allowing variation in prop_counted and
      # the proportion of non-injury deaths that are part of the MoH count
    
    # Initialise output
    out_pm <- data.frame(run = 1:runs,
      rx_d_other = runif(runs, min = 0, max = 0.5), p_m = NA, prop_dow = NA)
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = runs, style = 3)

  for (run_i in 1:runs) {
    
    # Update progress bar
    setTxtProgressBar(pb, run_i)  
    
    # Fresh cohort timeline
    ci_i <- ci_pm
    
    # Generate random proportions counted for each day
    ci_i$prop_counted <- suppressWarnings(inv.logit(rnorm(nrow(ci_i), 
      mean = ci_i$prop_counted_pred, sd = ci_i$prop_counted_pred_se)))
    
    # Subtract random deaths not due to injury during to date period 
    ci_i$d_counted <- ci_i$d_counted - ci_i$d_other * 
      out_pm[run_i, "rx_d_other"] * ci_i$prop_counted

    # Compute p_m (see model equations)
    x <- subset(ci_i, period == "to date")
    A <- sum(x$i_counted, na.rm = TRUE) * cfr_sq - d_dow_after
    p_m <- (sum(x$d_counted, na.rm = TRUE) - A) / 
      (sum(x$i_counted, na.rm = TRUE) - A)
    
    # Compute mean proportion who die of wounds in to date period
    prop_dow <- mean(x$d_counted * (1 - p_m) / x$d_counted, na.rm = TRUE)
    
    # Output
    out_pm[run_i, c("p_m", "prop_dow")] <- c(p_m, prop_dow)
  }  
close(pb)             

    
#...............................................................................  
### ENDS
#...............................................................................
