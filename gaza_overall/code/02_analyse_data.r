#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - OVERALL ++++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO ADJUST FOR COMPETING CAUSES OF DEATH AND RESCALE VALUES  -- ##
#...............................................................................

                                # LSHTM (January 2024)
                                # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Computing adjustment factors and applying these to the data
#...............................................................................

  #...................................      
  ## Convert all deaths to death risks (mean deaths only) and complement
  
    # For all cases divide by total population...
    df$risk <- df$mean / df$total  

    #...except maternal deaths?
#######TO THINK THROUGH: Makes very little difference either way (small n)
    
    # Compute complement (probability of survival)
    df$surv <- 1 - df$risk
    
    # Compute and store crude (unadjusted) risks
    crude <- aggregate(list(mean = df$mean), by = 
      df[, c("age", "scenario", "subperiod")], FUN = sum)
    crude <- merge(crude, pop, by = "age", all.x = TRUE)
    crude$risk_crude <- crude$mean / crude$total
              
  #...................................
  ## Multiply all survival probabilities together, by age, subperiod,
    # scenario, and take the complement = adjusted risk of dying
  df_agg <- aggregate(list(surv = df$surv), by = 
    df[, c("age", "scenario", "subperiod")], FUN = prod)
  df_agg$risk_adj <- 1 - df_agg$surv

  #...................................
  ## Compute adjustment factor, by age, subperiod, scenario
  df_agg <- merge(df_agg, crude[, c("age", "scenario", "subperiod",
    "risk_crude")], by = c("age", "scenario", "subperiod"), 
    all.x = TRUE)  
  df_agg$adj <- df_agg$risk_adj / df_agg$risk_crude
  
  #...................................
  ## Apply factor to all data and compute death rate
    
    # Save adjustment factors
    write.csv(df_agg, paste(dir_path, 'outputs/', 
      "out_adjustment_factors.csv", sep = ""), row.names = FALSE)
  
    # Apply to all data
    df <- merge(df, df_agg[, c("age", "scenario", "subperiod", 
      "adj")], by = c("age", "scenario", "subperiod"), all.x = TRUE)
    df$mean <- df$mean * df$adj    
    df$lci <- df$lci * df$adj    
    df$uci <- df$uci * df$adj    
        
    # Compute age-/cause-specific death rate per 1000 (annualised)
    df$dr_mean <- df$mean * 1000 * 365 / (df$total * 30.41 * 3)
    df$dr_lci <- df$lci * 1000 * 365 / (df$total * 30.41 * 3)
    df$dr_uci <- df$uci * 1000 * 365 / (df$total * 30.41 * 3)

    # Save
    write.csv(df, paste(dir_path, 'outputs/', 
      "out_final_estimates.csv", sep = ""), row.names = FALSE)
    
#...............................................................................  
### Visualising the estimates
#...............................................................................

  #...................................
  ## Tabulate baseline and excess deaths by scenario, subperiod and cause
    
    # Aggregate
    tab1 <- aggregate(df[, c("mean", "lci", "uci")], 
      by = df[, c("scenario", "subperiod", "module")], FUN = sum)
    x <- subset(tab1, scenario == "baseline")
    colnames(x) <- c("scenario", "subperiod", "module", 
      paste(c("mean", "lci", "uci"), "base", sep = "_"))
    x <- subset(x, select = -scenario)
    tab1 <- subset(tab1, scenario != "baseline")
    tab1 <- merge(tab1, x, by = c("subperiod", "module"), all.x = TRUE)
    for (i in c("mean", "lci", "uci")) {
      tab1[, paste(i, "excess", sep = "_")] <- tab1[, i] -
        tab1[, paste(i, "base", sep = "_")]
    }
    tab1 <- tab1[, c("module", "scenario", "subperiod", "mean_excess",
      "lci_excess", "uci_excess")]
    
    # Add period and grand totals
    x <- aggregate(tab1[, c("mean_excess", "lci_excess", "uci_excess")],
      by = tab1[, c("module", "scenario")], FUN = sum) 
    x$subperiod <- "total"
    x <- x[, c("module", "scenario", "subperiod",
      "mean_excess", "lci_excess", "uci_excess")]
    tab1 <- rbind(tab1, x)
    x <- aggregate(tab1[, c("mean_excess", "lci_excess", "uci_excess")],
      by = tab1[, c("scenario", "subperiod")], FUN = sum)
    x$module <- "all"
    x <- x[, c("module", "scenario", "subperiod",
      "mean_excess", "lci_excess", "uci_excess")]
    tab1 <- rbind(tab1, x)
    
    # Improve format 
    cols <- c("mean_excess", "lci_excess", "uci_excess")
    tab1[, cols] <- apply(tab1[, cols], 2, round, -1)
    tab1[, cols] <- apply(tab1[, cols], 2, format, big.mark = ",")
    tab1[, cols] <- apply(tab1[, cols],
      2, function(x) {trimws(as.character(x))} )
    tab1$excess_deaths <- paste(tab1$mean_excess, " (", tab1$lci_excess, 
      " to ", tab1$uci_excess, ")", sep = "")
    tab1 <- tab1[, c("module", "scenario", "subperiod", "excess_deaths")]
    
    # Reshape wide
    tab1 <- reshape(tab1, direction = "wide", timevar = "scenario",
      idvar =  c("module", "subperiod") )
    colnames(tab1) <- gsub("excess_deaths.", "", colnames(tab1))
    tab1 <- tab1[, c("module", "subperiod", scenarios)]
    tab1 <- tab1[order(tab1$module, tab1$subperiod), ]
    tab1 <- tab1[, c("disease", "subperiod", scenarios)]
    write.csv(tab1, paste(dir_path, "outputs/out_tab_epid_dis_pretty.csv", 
      sep = "/"), row.names = FALSE)
     
  #...................................
  ## Plot age-specific death rate by scenario and subperiod

    # Prepare data    
    df_plot <- aggregate(df[, c("dr_mean", "dr_lci", "dr_uci")],
      by = df[, c("scenario", "age", "subperiod")], FUN = sum)
    df_plot_baseline <- subset(df_plot, scenario == "baseline")
    df_plot_baseline <- subset(df_plot_baseline, select = -scenario)
    colnames(df_plot_baseline) <- c("age", "subperiod",
      "dr_base_mean", "dr_base_lci", "dr_base_uci")
    df_plot <- subset(df_plot, scenario != "baseline")
    df_plot <- merge(df_plot, df_plot_baseline, by = c("age", "subperiod"),
      all.x = TRUE)
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)  

    # Plot
    ggplot(data = df_plot, aes(group = scenario,
      colour = scenario, fill = scenario)) +
      geom_line(aes(x = age, y = dr_mean)) +
      geom_point(aes(x = age, y = dr_mean), shape = 15) +
      geom_ribbon(aes(x = age, ymin = dr_lci, ymax = dr_uci), alpha = 0.3,
        linetype = "21", linewidth = 0.5) +
      geom_line(aes(x = age, y = dr_base_mean),
        colour = palette_periods[1], linetype = "22") +
      geom_point(aes(x = age, y = dr_base_mean),
        colour = palette_periods[1], shape = 1) +
      # geom_ribbon(aes(x = age, ymin = dr_base_lci, ymax = dr_base_uci), alpha = 0.3,
      #   linetype = "21", linewidth = 0.5, fill = palette_periods[1]) +
      scale_y_continuous("annualised age-specific death rate (per 1000)") +
      theme_bw() +
      facet_grid(scenario ~ subperiod) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1)) +
      scale_colour_manual(values = palette_periods[c(3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(3,4,5)])
      
    # Save
    ggsave(paste(dir_path, 'outputs/', "age_specific_dr.png", sep = ""),
      dpi = "print", units = "cm", height = 15, width = 20)
    
  #...................................
  ## Plot crude death rate by scenario and subperiod
    
    # Prepare data    
    df_plot <- aggregate(df[, c("mean", "lci", "uci")],
      by = df[, c("scenario", "subperiod")], FUN = sum)
    df_plot$pop <- sum(pop$total)
    df_plot$cdr1000y_mean <-df_plot$mean * 1000 * 365/ (df_plot$pop * 30.41 * 3)
    df_plot$cdr1000y_lci<-df_plot$lci * 1000 * 365/ (df_plot$pop * 30.41 * 3)
    df_plot$cdr1000y_uci <-df_plot$uci * 1000 * 365/ (df_plot$pop * 30.41 * 3)
    df_plot$cdr10000d_mean <-df_plot$mean * 10000 / (df_plot$pop * 30.41 * 3)
    df_plot$cdr10000d_lci<-df_plot$lci * 10000 / (df_plot$pop * 30.41 * 3)
    df_plot$cdr10000d_uci <-df_plot$uci * 10000 / (df_plot$pop * 30.41 * 3)
    df_plot$scenario <- factor(df_plot$scenario, 
      levels = c("baseline", scenarios) )

    # Plot - per 1000 person-years
    ggplot(data = df_plot, aes(x = scenario, y = cdr1000y_mean, 
      colour = scenario, fill = scenario)) +
      geom_bar(stat = "identity", alpha = 0.5) +
      # geom_errorbar(aes(ymin = cdr1000y_lci, ymax = cdr1000y_uci),
      #   width = 0.3) +
      theme_bw() +
      facet_wrap(. ~ subperiod) +
      scale_y_continuous("crude death rate per 1000 person-years",
        expand = c(0.01, 0.05), breaks = seq(0, 100, 5)) +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      theme(legend.position = "top", panel.grid.major.x = element_blank())
      
    ggsave(paste(dir_path, 'outputs/', "cdr_1000py.png", sep = ""),
      dpi = "print", units = "cm", height = 12, width = 20)
    
    # Plot - per 10,000 person-days
    ggplot(data = df_plot, aes(x = scenario, y = cdr10000d_mean, 
      colour = scenario, fill = scenario)) +
      geom_bar(stat = "identity", alpha = 0.5) +
      # geom_errorbar(aes(ymin = cdr10000d_lci, ymax = cdr10000d_uci),
      #   width = 0.3) +
      theme_bw() +
      facet_wrap(. ~ subperiod) +
      scale_y_continuous("crude death rate per 10,000 person-days",
        expand = c(0.01, 0.05), breaks = seq(0, 2.5, 0.25)) +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      theme(legend.position = "top", panel.grid.major.x = element_blank() )
      
    ggsave(paste(dir_path, 'outputs/', "cdr_10000pd.png", sep = ""),
      dpi = "print", units = "cm", height = 12, width = 20)
    
     
    
#...............................................................................
### ENDS
#...............................................................................

