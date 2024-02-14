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
  
    # Columns to refer to often
    type <- c("base", "crisis", "excess")    
    cols <- c("mean", "lci", "uci")
    
    # For all cases divide by total population...
    df[, paste(type, "risk", sep = "_")] <- NA
    for (i in type) {df[, paste(i, "risk", sep = "_")] <- 
      df[, paste(i, "mean", sep = "_")] / df$total}

    #...except maternal deaths?
#######TO THINK THROUGH: Makes very little difference either way (small n)
    
    # Compute complement (probability of survival)
    df[, paste(type, "surv", sep = "_")] <- NA
    for (i in type) {df[, paste(i, "surv", sep = "_")] <- 
      1 - df[, paste(i, "risk", sep = "_")]}
    
    # Compute and store crude (unadjusted) risks
    crude <- aggregate(df[, grep("mean", colnames(df))], by = 
      df[, c("age", "scenario", "subperiod")], FUN = sum)
    crude <- merge(crude, pop, by = "age", all.x = TRUE)
    for (i in type) {crude[, paste(i, "risk_crude", sep = "_")] <- 
      crude[, paste(i, "mean", sep = "_")] / crude$total}
              
  #...................................
  ## Multiply all survival probabilities together, by age, subperiod,
    # scenario, and take the complement = adjusted risk of dying
  df_agg <- aggregate(df[, grep("surv", colnames(df))], by = 
    df[, c("age", "scenario", "subperiod")], FUN = prod)
  for (i in type) {df_agg[, paste(i, "risk_adj", sep = "_")] <- 
    1 - df_agg[, paste(i, "surv", sep = "_")]}


  #...................................
  ## Compute adjustment factor, by age, subperiod, scenario
  df_agg <- merge(df_agg, crude[, c("age", "scenario", "subperiod",
    grep("risk_crude", colnames(crude), value = TRUE))], 
    by = c("age", "scenario", "subperiod"), all.x = TRUE)
  for (i in type) {df_agg[, paste(i, "adj", sep = "_")] <- 
    df_agg[, paste(i, "risk_adj", sep = "_")] /
    df_agg[, paste(i, "risk_crude", sep = "_")]}

  #...................................
  ## Apply factor to all data and compute death rate
    
    # Save adjustment factors
    write.csv(df_agg, paste(dir_path, 'outputs/', 
      "out_adjustment_factors.csv", sep = ""), row.names = FALSE)
  
    # Apply to all data
    df <- merge(df, df_agg[, c("age", "scenario", "subperiod", 
      paste(type, "adj", sep = "_"))], by = c("age", "scenario", "subperiod"), 
      all.x = TRUE)
    for (i in type) {
      df[, paste(i, cols, sep = "_")] <-  df[, paste(i, cols, sep = "_")] *
        df[, paste(i, "adj", sep = "_")]
    }

    # Compute age-/cause-specific death rate per 1000 (annualised)
    x <- 1000 * 365 / (df$total * 30.41 * 3)
    for (i in type) {
      df[, paste(i, "dr", cols, sep = "_")] <- 
        df[, paste(i, cols, sep = "_")] * x
    }
    
    # Clean up columns
    x <- c("module", "disease", "scenario", "subperiod", "age",
      paste(type, "mean", sep = "_"), paste(type, "lci", sep = "_"),
      paste(type, "uci", sep = "_"), grep("_dr_", colnames(df), value = TRUE))
    
    # Save
    write.csv(df, paste(dir_path, 'outputs/', 
      "out_final_estimates.csv", sep = ""), row.names = FALSE)
    
    
#...............................................................................  
### Tabulating the estimates
#...............................................................................

  #...................................
  ## Tabulate baseline and excess deaths by scenario, subperiod and cause
    
    # Aggregate by scenario, subperiod and module
    tab1 <- data.frame()
    x <- c(paste(type, "mean", sep = "_"), paste(type, "lci", sep = "_"),
      paste(type, "uci", sep = "_"))
    x1 <- aggregate(df[, x], 
      by = df[, c("scenario", "subperiod", "module")], FUN = sum)

    # Add whole period totals
    x2 <- aggregate(x1[, x], by = x1[, c("scenario", "module")], FUN = sum) 
    x2$subperiod <- "total"
    x2 <- x2[, colnames(x1)]
    x1$subperiod <- as.character(x1$subperiod)
    tab1 <- rbind(x1, x2)
    
    # Add grand totals by period
    x2 <- aggregate(x1[, x], by = x1[, c("scenario", "subperiod")], 
      FUN = sum)
    x2$module <- "all"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
    
    # Add grand totals
    x2 <- aggregate(x1[, x], by = list(scenario = x1$scenario), 
      FUN = sum)
    x2$module <- "all"
    x2$subperiod <- "total"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
    
    # Add grand totals by period without epidemics
    x1 <- subset(x1, module != "infections - epidemic")
    x2 <- aggregate(x1[, x], by = x1[, c("scenario", "subperiod")], 
      FUN = sum)
    x2$module <- "all minus epidemics"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
    
    # Add grand totals without epidemics
    x2 <- aggregate(x1[, x], by = list(scenario = x1$scenario), 
      FUN = sum)
    x2$module <- "all minus epidemics"
    x2$subperiod <- "total"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
 
    # Write raw output
    write.csv(tab1, paste(dir_path, "outputs/out_tab_dis.csv", 
      sep = "/"), row.names = FALSE)
    
    # Improve format 
    tab1[, x] <- apply(tab1[, x], 2, round, -1)
    tab1[, x] <- apply(tab1[, x], 2, format, big.mark = ",")
    tab1[, x] <- apply(tab1[, x],
      2, function(x) {trimws(as.character(x))} )
    for (i in type) {
      tab1[, paste("deaths", i, sep = "_")] <- paste(
        tab1[, paste(i, "mean", sep = "_")], " (", 
        tab1[, paste(i, "lci", sep = "_")], " to ",
        tab1[, paste(i, "uci", sep = "_")], ")", sep = ""
        )
    }
    tab1 <- tab1[, c("module", "scenario", "subperiod", "deaths_base",
      "deaths_crisis", "deaths_excess")]
    
    # Write pretty output
    write.csv(tab1, paste(dir_path, "outputs/out_tab_dis_long_pretty.csv", 
      sep = "/"), row.names = FALSE)
    
    # Reshape wide - excess only
    tab1 <- reshape(tab1, direction = "wide", timevar = "scenario",
      idvar =  c("module", "subperiod"), sep = "_" )
    tab1 <- tab1[, c("module", "subperiod", 
      grep("deaths_excess", colnames(tab1), value = TRUE))]
    tab1$module <- factor(tab1$module, levels = c ("injuries", 
      "infections - epidemic", "infections - endemic", "MNH", "NCDs",
      "all minus epidemics", "all"))
    tab1 <- tab1[order(tab1$module, tab1$subperiod), ]
    tab1 <- tab1[, c("module", "subperiod", paste("deaths_excess", scenarios, 
      sep = "_"))]    
    write.csv(tab1, paste(dir_path,"outputs/out_tab_dis_wide_excess_pretty.csv", 
      sep = "/"), row.names = FALSE)
     

  #...................................
  ## Tabulate baseline and excess deaths by scenario, subperiod and age
    
    # Aggregate by scenario, subperiod and age
    tab1 <- data.frame()
    x <- c(paste(type, "mean", sep = "_"), paste(type, "lci", sep = "_"),
      paste(type, "uci", sep = "_"))
    x1 <- aggregate(df[, x], 
      by = df[, c("scenario", "subperiod", "age")], FUN = sum)

    # Add whole period totals
    x2 <- aggregate(x1[, x], by = x1[, c("scenario", "age")], FUN = sum) 
    x2$subperiod <- "total"
    x2 <- x2[, colnames(x1)]
    x1$subperiod <- as.character(x1$subperiod)
    tab1 <- rbind(x1, x2)
    
    # Add grand totals by period
    x2 <- aggregate(x1[, x], by = x1[, c("scenario", "subperiod")], 
      FUN = sum)
    x2$age <- "all"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
    
    # Add grand totals
    x2 <- aggregate(x1[, x], by = list(scenario = x1$scenario), 
      FUN = sum)
    x2$age <- "all"
    x2$subperiod <- "total"
    x2 <- x2[, colnames(x1)]
    tab1 <- rbind(tab1, x2)
    
    # Write raw output
    write.csv(tab1, paste(dir_path, "outputs/out_tab_age.csv", 
      sep = "/"), row.names = FALSE)
    
    # Improve format 
    tab1[, x] <- apply(tab1[, x], 2, round, -1)
    tab1[, x] <- apply(tab1[, x], 2, format, big.mark = ",")
    tab1[, x] <- apply(tab1[, x],
      2, function(x) {trimws(as.character(x))} )
    for (i in type) {
      tab1[, paste("deaths", i, sep = "_")] <- paste(
        tab1[, paste(i, "mean", sep = "_")], " (", 
        tab1[, paste(i, "lci", sep = "_")], " to ",
        tab1[, paste(i, "uci", sep = "_")], ")", sep = ""
        )
    }
    tab1 <- tab1[, c("age", "scenario", "subperiod", "deaths_base",
      "deaths_crisis", "deaths_excess")]
    
    # Write pretty output
    write.csv(tab1, paste(dir_path, "outputs/out_tab_age_long_pretty.csv", 
      sep = "/"), row.names = FALSE)
    
    # Reshape wide - excess only
    tab1 <- reshape(tab1, direction = "wide", timevar = "scenario",
      idvar =  c("age", "subperiod"), sep = "_" )
    tab1 <- tab1[, c("age", "subperiod", 
      grep("deaths_excess", colnames(tab1), value = TRUE))]
    tab1 <- tab1[order(tab1$age, tab1$subperiod), ]
    tab1 <- tab1[, c("age", "subperiod", paste("deaths_excess", scenarios, 
      sep = "_"))]
    write.csv(tab1, paste(dir_path,"outputs/out_tab_age_wide_excess_pretty.csv", 
      sep = "/"), row.names = FALSE)
  
       
    
#...............................................................................  
### Producing graphs of the estimates
#...............................................................................
    
  #...................................
  ## Plot age-specific death rate by scenario and subperiod, with/without RRs

    # Prepare data    
    df_plot_crisis <- aggregate(df[, paste("crisis_dr", cols, sep = "_")],
      by = df[, c("scenario", "age", "subperiod")], FUN = sum)
    x <- subset(df, scenario == "ceasefire")
    df_plot_base <- aggregate(x[, paste("base_dr", cols, sep = "_")],
      by = x[, c("scenario", "age", "subperiod")], FUN = sum)
    df_plot_base <- subset(df_plot_base, select = -scenario)
    df_plot <- merge(df_plot_crisis, df_plot_base, by = c("age","subperiod"), 
      all.x = TRUE)
    df_plot$rr <- df_plot$crisis_dr_mean / df_plot$base_dr_mean
    df_plot$rr <- scales::label_number(accuracy = 0.1, big.mark=",")(df_plot$rr)
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)  

    # Plot without RR labels
    plot1 <- ggplot(data = df_plot, aes(group = scenario,
      colour = scenario, fill = scenario)) +
      geom_line(aes(x = age, y = crisis_dr_mean)) +
      geom_point(aes(x = age, y = crisis_dr_mean), shape = 15) +
      geom_ribbon(aes(x = age, ymin = crisis_dr_lci, ymax = crisis_dr_uci),
        alpha = 0.3, linetype = "blank", linewidth = 0.5) +
      geom_line(aes(x = age, y = base_dr_mean),
        colour = palette_periods[1], linetype = "22") +
      geom_point(aes(x = age, y = base_dr_mean),
        colour = palette_periods[1], shape = 1) +
      # geom_ribbon(aes(x = age, ymin = base_dr_lci, ymax = base_dr_uci), 
        # alpha = 0.3, linetype = "21", linewidth = 0.5, 
        # fill = palette_periods[1]) +
      scale_y_continuous("annualised age-specific death rate (per 1000)") +
      theme_bw() +
      facet_grid(scenario ~ subperiod) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1)) +
      scale_colour_manual(values = palette_periods[c(3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(3,4,5)])

      # save
      ggsave(paste(dir_path, 'outputs/', "age_specific_dr.png", sep = ""),
        dpi = "print", units = "cm", height = 15, width = 20)
    
    # Plot with RR labels
    plot2 <- plot1 + 
      geom_text(aes(x = age, y = crisis_dr_mean, colour = scenario,
        label = rr), size = 3, nudge_y = 300, angle = 45)
      
      # save
      ggsave(paste(dir_path, 'outputs/', "age_specific_dr_w_rrs.png", sep = ""),
        dpi = "print", units = "cm", height = 15, width = 20)
 
  #...................................
  ## Plot age-specific death rate by scenario, with/without RRs, with/without CI

    # Prepare data    
    df_plot_crisis <- aggregate(df[, paste("crisis_dr", cols, sep = "_")],
      by = df[, c("scenario", "age")], FUN = sum)
    x <- subset(df, scenario == "ceasefire")
    df_plot_base <- aggregate(x[, paste("base_dr", cols, sep = "_")],
      by = x[, c("scenario", "age")], FUN = sum)
    df_plot_base <- subset(df_plot_base, select = -scenario)
    df_plot <- merge(df_plot_crisis, df_plot_base, by = "age", 
      all.x = TRUE)
    df_plot$rr <- df_plot$crisis_dr_mean / df_plot$base_dr_mean
    df_plot$rr <- scales::label_number(accuracy = 0.1, big.mark=",")(df_plot$rr)
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)  

    # Plot without RR labels or CIs
    plot1 <- ggplot(data = df_plot, aes(group = scenario,
      colour = scenario, fill = scenario)) +
      geom_line(aes(x = age, y = crisis_dr_mean)) +
      geom_point(aes(x = age, y = crisis_dr_mean), shape = 15) +
      # geom_ribbon(aes(x = age, ymin = crisis_dr_lci, ymax = crisis_dr_uci),
      #   alpha = 0.3, linetype = "blank", linewidth = 0.5) +
      geom_line(aes(x = age, y = base_dr_mean),
        colour = palette_periods[1], linetype = "22") +
      geom_point(aes(x = age, y = base_dr_mean),
        colour = palette_periods[1], shape = 1) +
      # geom_ribbon(aes(x = age, ymin = base_dr_lci, ymax = base_dr_uci), 
        # alpha = 0.3, linetype = "21", linewidth = 0.5, 
        # fill = palette_periods[1]) +
      scale_y_continuous("annualised age-specific death rate (per 1000)") +
      theme_bw() +
      facet_grid(scenario ~ .) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1)) +
      scale_colour_manual(values = palette_periods[c(3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(3,4,5)])

      # save
      ggsave(paste(dir_path, 'outputs/', "age_specific_dr_no_sub.png", sep = ""),
        dpi = "print", units = "cm", height = 15, width = 20)
    
    # Plot with RR labels but no CIs
    plot2 <- plot1 + 
      geom_text(aes(x = age, y = crisis_dr_mean, colour = scenario,
        label = rr), size = 3, nudge_y = 500, angle = 45)

      # save
      ggsave(paste(dir_path, 'outputs/', 
        "age_specific_dr_w_rrs_no_sub_no_ci.png", sep = ""), 
        dpi = "print", units = "cm", height = 15, width = 20)
    
    # Plot with RR labels and CIs
    plot3 <- plot2 + 
      geom_text(aes(x = age, y = crisis_dr_mean, colour = scenario,
        label = rr), size = 3, nudge_y = 500, angle = 45) +
      geom_ribbon(aes(x = age, ymin = crisis_dr_lci, ymax = crisis_dr_uci),
        alpha = 0.3, linetype = "blank", linewidth = 0.5)
          
      # save
      ggsave(paste(dir_path, 'outputs/', 
        "age_specific_dr_w_rrs_no_sub_ci.png", sep = ""), 
        dpi = "print", units = "cm", height = 15, width = 20)
    
    
  #...................................
  ## Plot crude death rate by scenario + 
    # baseline (estimated counterfactual and 2022 - actual)
    
    # Prepare data    
    df_plot_crisis <- aggregate(df[, c("crisis_mean", "crisis_lci", 
      "crisis_uci")], by = list(scenario = df$scenario), FUN = sum)
    df_plot_crisis$pop <- sum(pop$total)
    x1 <- 1000 * 365/ (df_plot_crisis$pop * 30.41 * 6)
    x2 <- 10000 / (df_plot_crisis$pop * 30.41 * 6)
    df_plot_crisis$cdr1000y_mean <-df_plot_crisis$crisis_mean * x1
    df_plot_crisis$cdr1000y_lci<-df_plot_crisis$crisis_lci * x1
    df_plot_crisis$cdr1000y_uci <-df_plot_crisis$crisis_uci * x1
    df_plot_crisis$cdr10000d_mean <-df_plot_crisis$crisis_mean * x2
    df_plot_crisis$cdr10000d_lci<-df_plot_crisis$crisis_lci * x2
    df_plot_crisis$cdr10000d_uci <-df_plot_crisis$crisis_uci * x2
    df_plot_crisis$scenario <- factor(df_plot_crisis$scenario, 
      levels = c("baseline", scenarios) )
    df_plot_crisis$category <- "crisis"
    df_plot_crisis <- df_plot_crisis[, c("scenario",
      "category", grep("cdr", colnames(df_plot_crisis), value = TRUE))]
    
    df_plot_base <- aggregate(df[, c("base_mean", "base_lci", 
      "base_uci")], by = list(scenario = df$scenario), FUN = sum)
    df_plot_base$pop <- sum(pop$total)
    x1 <- 1000 * 365/ (df_plot_base$pop * 30.41 * 6)
    x2 <- 10000 / (df_plot_base$pop * 30.41 * 6)
    df_plot_base$cdr1000y_mean <-df_plot_base$base_mean * x1
    df_plot_base$cdr1000y_lci<-df_plot_base$base_lci * x1
    df_plot_base$cdr1000y_uci <-df_plot_base$base_uci * x1
    df_plot_base$cdr10000d_mean <-df_plot_base$base_mean * x2
    df_plot_base$cdr10000d_lci<-df_plot_base$base_lci * x2
    df_plot_base$cdr10000d_uci <-df_plot_base$base_uci * x2
    df_plot_base$scenario <- factor(df_plot_base$scenario, 
      levels = c("baseline", scenarios) )
    df_plot_base$category <- "baseline"
    df_plot_base <- df_plot_base[, c("scenario",
      "category", grep("cdr", colnames(df_plot_base), value = TRUE))]

    df_plot <- rbind(df_plot_crisis, df_plot_base)
    df_plot$scenario <- as.character(df_plot$scenario)
    df_plot$category <- ifelse(df_plot$category == "baseline", "baseline",
      df_plot$scenario)
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)
    df_plot$category <- factor(df_plot$category, 
      levels = c(scenarios, "baseline"))
    df_plot$cdr1000y_label <- paste(scales::label_number(accuracy = 0.1)(
      df_plot$cdr1000y_mean), " (95%CI ", 
      scales::label_number(accuracy = 0.1)(
      df_plot$cdr1000y_lci), " to ", 
      scales::label_number(accuracy = 0.1)(
      df_plot$cdr1000y_uci), ")", sep = "")
    df_plot$cdr10000d_label <- paste(scales::label_number(accuracy = 0.01)(
      df_plot$cdr10000d_mean), " (95%CI ", 
      scales::label_number(accuracy = 0.01)(
      df_plot$cdr10000d_lci), " to ", 
      scales::label_number(accuracy = 0.01)(
      df_plot$cdr10000d_uci), ")", sep = "")

    # Plot - per 1000 person-years
    plot_1000y <- ggplot(data = df_plot, aes(y = cdr1000y_mean, x = scenario, 
      colour = category, fill = category)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
      # geom_errorbar(aes(ymin = cdr1000y_lci, ymax = cdr1000y_uci), width = 0.2,
      #   linetype = "21") +
      theme_bw() +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_y_continuous("crude death rate per 1000 person-years",
        breaks = seq(0, 300, 20), expand = expansion(mult = c(0, 0.07))) +
      scale_x_discrete(expand = expansion(mult = c(0.5, 0.3))) +
      theme(legend.position = "none", panel.grid.major.x = element_blank()) +
      geom_hline(aes(yintercept = 2.72), colour = palette_gen[5],
        linetype = "21") +
      geom_text(aes(x = scenario, y = cdr1000y_mean, label = cdr1000y_label),
        nudge_y = c(7.55, 7.5, 7.5, 5, 5, 5)) +
      annotate("text", x = 0.25, y = 9, colour = palette_gen[5], size = 3, 
        label = stringr::str_wrap("pre-war mean CDR (2.7)", 15))
    
    ggsave(paste(dir_path, 'outputs/', "cdr_1000py.png", sep = ""),
      dpi = "print", units = "cm", height = 8, width = 20)
    
    # Plot - per 10,000 person-days
    plot_10000d <- ggplot(data = df_plot, aes(y = cdr10000d_mean, x = scenario, 
      colour = category, fill = category)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
      # geom_errorbar(aes(ymin = cdr1000y_lci, ymax = cdr1000y_uci), width = 0.2,
      #   linetype = "21") +
      theme_bw() +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_y_continuous("crude death rate per 10,000 person-days",
        breaks = seq(0, 3, 0.2), expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(expand = expansion(mult = c(0.5, 0.3))) +
      theme(legend.position = "none", panel.grid.major.x = element_blank()) +
      geom_hline(aes(yintercept = 0.0745), colour = palette_gen[5],
        linetype = "21") +
      geom_text(aes(x = scenario, y = cdr10000d_mean, label = cdr10000d_label),
        nudge_y = c(0.2, 0.2, 0.2, 0.12, 0.12, 0.12)) +
      annotate("text", x = 0.25, y = 0.3, colour = palette_gen[5], size = 3, 
        label = stringr::str_wrap("pre-war mean CDR (0.07)", 15))
    
    ggsave(paste(dir_path, 'outputs/', "cdr_10000pd.png", sep = ""),
      dpi = "print", units = "cm", height = 8, width = 20)
    
    # Combined age-specific and crude death rate plot
    ggarrange(plot3, plot_1000y, nrow = 2, heights =
      c(1, 0.5), labels = c("A", "B"), hjust = -3, vjust = c(3, -1.5))
     
    ggsave(paste(dir_path, 'outputs/', 
      "age_specific_and_cdr_combi.png", sep = ""),
      dpi = "print", units = "cm", height = 22, width = 20)

 
  #...................................
  ## Plot the proportion of excess deaths by age, by scenario
    
    # Prepare data for plotting
    df_plot <- aggregate(list(excess = df$excess_mean), by = 
      df[, c("scenario", "age")], FUN = sum)
    x <- aggregate(list(tot_excess = df$excess_mean), by = 
      list(scenario = df$scenario), FUN = sum)
    df_plot <- merge(df_plot, x, by = "scenario", all.x = TRUE)
    df_plot$prop <- df_plot$excess / df_plot$tot_excess
    df_plot$prop_label <- scales::label_percent(accuracy = 0.1)(
      df_plot$prop)
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)
    
    # Plot
    ggplot(data = df_plot, aes(x = age, y = prop, colour = scenario,
      fill = scenario)) +
      geom_bar(alpha = 0.5, stat = "identity") +
      theme_bw() +
      scale_colour_manual(values = palette_periods[scenarios]) +
      scale_fill_manual(values = palette_periods[scenarios]) +
      scale_y_continuous(name = "percentage of all excess deaths",
        breaks = seq(0, 1, 0.025), labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none", panel.grid.major.x = element_blank()) +
      facet_wrap(scenario ~ ., ncol = 1) +
      geom_text(aes(x = age, y = prop, label = prop_label), size = 3,
        nudge_y = 0.01)
    
    ggsave(paste(dir_path, 'outputs/', 
      "age_distribution_deaths.png", sep = ""),
      dpi = "print", units = "cm", height = 20, width = 20)
           
#...............................................................................
### ENDS
#...............................................................................

