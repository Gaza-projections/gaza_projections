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
    daily$mean_since <- ifelse(daily$t_since > 1, "*", "")
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
### Modelling the daily death count - for projection
#...............................................................................

  #...................................      
  ## Set reference periods for status quo and escalation scenarios
    
    # Set period for status quo scenario
    period_sq <- as.Date(as.Date("2023/10/15") : as.Date("2024/01/15"))
    
    # Figure out 30-day period with highest death toll (for escalation scenario)
      # first interpolate daily deaths to fill NA values
      x <- na.omit(daily[, c("date", "d_cum")])
      daily$d_cum_ipol <- approx(x = x$date, y = x$d_cum, xout <- daily$date)$y
      daily$d_ipol <- c(daily[1, "d_cum_ipol"], diff(daily$d_cum_ipol))
      
      # then compute rolling sum with window of 30 days
      daily$d_running <- rollsum(daily$d_ipol, k = 30, align = "left", 
        fill = c(NA, NA, NA))    
      
      # find start date with highest death toll in subsequent 30 days
      x <- daily[which.max(daily$d_running), "date"]
      
      # so period for escalation scenario is
      period_es <- as.Date(x : (x + 30))
    
    
  #...................................      
  ## Fit a null count model to daily death count - used to project status quo
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_sq), c("date", "d", "ptime")]
    df <- na.omit(df)
    fit_sq <- glm.nb(data = df, d ~ 1 + offset(log(ptime)))
    summary(fit_sq)
  
    # # Visualise goodness of fit against data
    # df$predict <- predict(fit_sq, type = "response")
    # df$dr_obs <- df$d * 1000 / df$ptime
    # df$dr_pred <- df$predict * 1000 / df$ptime
    # plot_sq <- ggplot(data = df, aes(x = dr_obs)) +
    #   geom_histogram(fill = palette_periods[4], colour = palette_periods[4],
    #     alpha = 0.7) +
    #   geom_density(aes(x = dr_pred), colour = palette_gen[5], linewidth = 1,
    #     adjust = 3) +
    #   theme_bw() +
    #   scale_x_continuous("daily death rate per 1000 people")

    # Store logged coefficient and standard error, for use later
    sq <- summary(fit_sq)$coefficients[, c(1,2)]
    names(sq) <- c("mean", "se")    
      
  #...................................      
  ## Fit a count model to daily death count - used to project escalation
    
    # Fit model (neg-bin)
    df <- daily[which(daily$date %in% period_es), c("date", "d", "ptime")]
    df <- na.omit(df)
    fit_es <- glm.nb(data = df, d ~ 1 + offset(log(ptime)))
    summary(fit_es)
  
    # # Visualise goodness of fit against data
    # df$predict <- predict(fit_es, type = "response")
    # df$dr_obs <- df$d * 1000 / df$ptime
    # df$dr_pred <- df$predict * 1000 / df$ptime
    # plot_es <- ggplot(data = df, aes(x = dr_obs)) +
    #   geom_histogram(fill = palette_periods[5], colour = palette_periods[5],
    #     alpha = 0.7) +
    #   geom_density(aes(x = dr_pred), colour = palette_gen[5], linewidth = 1, 
    #     adjust = 4) +
    #   theme_bw() +
    #   scale_x_continuous("daily death rate per 1000 people")

    # Store logged coefficient and standard error, for use later
    es <- summary(fit_es)$coefficients[, c(1,2)]
    names(es) <- c("mean", "se")    
      
  # #...................................      
  # ## Combination graph
  #   
  #   # Graph
  #   ggarrange(NULL, plot_sq, plot_es, nrow = 3,labels = c("", "status quo",
  #     "escalation"), font.label = list(size = 10), vjust = 0, heights = 
  #     c(0.1, 1, 1))
  #   
  #   # Save        
  #   ggsave(paste(dir_path, 'outputs/', 
  #     "count_models_scenarios_combi.png", sep = ""),
  #     dpi = "print", units = "cm", height = 15, width = 22)


#...............................................................................  
### Estimating the proportion of counted deaths
#...............................................................................

  # #...................................      
  # ## Fit growth model of cumulative deaths against cumulative UNRWA rate
  #   
  #   # Fit model
  #   df <- daily[, c("time", "d_cum", "dr_unrwa_cum")]
  #   df <- na.omit(df)
  #   fit_gm <- mgcv::gam(data = df, d_cum ~ s(time) + dr_unrwa_cum, family = "poisson")
  #   summary(fit_gm)
  #   exp(coef(fit_gm))
  # 
  #   # Visualise goodness of fit against data
  #   df$d_cum_pred <- predict(fit_gm, type = "response")
  #   ggplot(data = df, aes(x = time)) +
  #     geom_line(aes(y = d_cum), colour = palette_gen[15]) +
  #     geom_line(aes(y = d_cum_pred), colour = palette_gen[6]) +
  #     theme_bw() +
  #     scale_y_continuous("cumulative death rate per 1000 people")
  # 
    
    
  #...................................      
  ## Fit a beta regression of general death rate vs UNRWA death rate
    
    # Fit model excluding period at the beginning when system was coping
    df <- na.omit(daily[which(daily$date > as.Date("2023-10-20")), 
      c("date", "time", "pc", "dr", "prop_sh_ipol")])
    # df$pc <- ifelse(df$pc >= 1, 0.99999, df$pc)

#    fit_pc <- lm(data = df, pc ~ 1)
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
    moh_d <- moh_d[, c("gender", "age", "prop")]
    

#...............................................................................  
### ENDS
#...............................................................................
