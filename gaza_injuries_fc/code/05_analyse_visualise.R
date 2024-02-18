#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO ANALYSE AND VISUALISE SIMULATION OUTPUTS  ---------- ##
#...............................................................................


#...............................................................................  
### Aggregating all scenarios together
#...............................................................................

  #...................................
  ## Ceasefire: combine ordnance deaths / injuries and deaths due to wounds
  
    # Read simulation output if not already in environment
    if (! exists("out_cf")) {out_cf <- read_rds(paste(dir_path,
      'outputs/',"out_cf_all_runs.rds", sep=""))}

    # Read simulation output if not already in environment
    if (! exists("out_ord")) {out_ord <- read_rds(paste(dir_path,
      'outputs/',"out_ord_all_runs.rds", sep=""))}
    
    # Merge the two together
    out_cf <- merge(out_cf, out_ord, by = c("run", "period", "age", "gender"))

    # Add deaths and injuries
    out_cf$d_all <- out_cf$d_dow + out_cf$d_ord
    out_cf$i_all <- out_cf$i_ord
    out_cf$d_dow <- out_cf$d_dow + out_cf$d_ord_dow

    # Add other categories that feature in other scenarios
    out_cf$d_counted <- out_cf$d_all
    out_cf$d_uncounted <- 0 # assume full counting in ceasefire scenario
    out_cf$d_m <- out_cf$d_all - out_cf$d_dow
    
    # Add scenario
    out_cf$scenario <- "ceasefire"
    
  #...................................
  ## Aggregate with status quo and escalation 
    
    # Read simulation output if not already in environment
    if (! exists("out_main")) {out_main <- read_rds(paste(dir_path,
      'outputs/',"out_main_all_runs.rds", sep=""))}
    
    # Add other categories that feature in ceasefire scenario
    out_main$d_ord <- NA
    out_main$i_ord <- NA

    # Bind together
    out <- rbind(out_cf[, colnames(out_main)], out_main)
    
    # Aggregate across all runs and combining genders together
    stats <- c("mean", "lci", "uci")
    
    tab1 <- aggregate(list(d_all = out$d_all), 
      by = out[, c("run", "scenario", "period", "age")], FUN = sum)

    tab1 <- aggregate(list(d_all = tab1$d_all), 
      by = tab1[, c("scenario", "period", "age")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    tab1 <- data.frame(tab1[, c("scenario", "period", "age")], 
      tab1$d_all)
    colnames(tab1) <- c("scenario", "period", "age", stats)

    # Save
    write.csv(tab1, paste(dir_path, "outputs/out_for_aggregation.csv", 
      sep = "/"), row.names = FALSE)
    
  #...................................
  ## If aggregation has already been done, adjust for multiple risks of deaths
    
    # Specify file path
    filename <- paste(dir_path, 'inputs/',"out_adjustment_factors.csv", sep="")
    
  if (file.exists(filename)) {
    
    # Read adjustment factors
    adj <- read.csv(filename)
    adj <- adj[, c("age", "scenario", "subperiod", "crisis_adj")]
    colnames(adj) <- c("age", "scenario", "period", "adj")
    
    # Apply adjustment to all results in the output
    out <- merge(out, adj, by = c("age", "scenario", "period"), all.x = TRUE)
    out[, grep("d_", colnames(out))] <- out[, grep("d_", colnames(out))] *
      out$adj
  }
 
    
#...............................................................................  
### Tabulating the estimates
#...............................................................................

  #...................................
  ## Tabulate injury deaths by scenario, age, gender and subperiod

    # Aggregate by scenario, age and subperiod
    stats <- c("mean", "lci", "uci")
    
    x1 <- aggregate(list(d_all = out$d_all), 
      by = out[, c("run", "scenario", "age", "gender", "period")], FUN = sum)
    tab2 <- aggregate(list(d_all = x1$d_all), 
      by = x1[, c("scenario", "age", "gender", "period")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    
    tab2 <- data.frame(tab2[, c("scenario", "age", "gender","period")], 
      tab2$d_all)
    colnames(tab2) <- c("scenario", "age", "gender", "period", stats)
         
    # Add totals by scenario, gender and age
    x1 <- aggregate(list(d_all = out$d_all),
      by = out[, c("run", "scenario", "age", "gender")], FUN = sum )
    x2 <- aggregate(list(d_all = x1$d_all), 
      by = x1[, c("scenario", "age", "gender")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    x2 <- data.frame(x2[, c("scenario", "age", "gender")], x2$d_all)
    x2$period <- "total"
    colnames(x2) <- c("scenario", "age", "gender", stats, "period")
    x2 <- x2[, colnames(tab2)]
    tab2 <- rbind(tab2, x2)
    
    
    # # Add totals by scenario and gender
    # x1 <- aggregate(list(d_all = out$d_all), 
    #   by = out[, c("run", "scenario", "gender")], FUN = sum )
    # x2 <- aggregate(list(d_all = x1$d_all), by = x1[, c("scenario", "gender")], 
    #   FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    # x2 <- data.frame(x2[, c("scenario", "gender")], x2$d_all)
    # x2$age <- "all"
    # colnames(x2) <- c("scenario", "gender", stats, "age")
    # x2 <- x2[, colnames(tab2)]
    # tab2 <- rbind(tab2, x2)
    # 
    # # Add totals by scenario
    # x1 <- aggregate(list(d_all = out$d_all), 
    #   by = out[, c("run", "scenario")], FUN = sum )
    # x2 <- aggregate(list(d_all = x1$d_all), by = list(scenario = x1$scenario), 
    #   FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    # x2 <- data.frame(x2$scenario, x2$d_all)
    # x2$age <- "all"
    # x2$gender <- "all"
    # colnames(x2) <- c("scenario", stats, "age", "gender")
    # x2 <- x2[, colnames(tab2)]
    # tab2 <- rbind(tab2, x2)
    
    # Write raw output
    write.csv(tab2, paste(dir_path, "outputs/out_tab_age_gender.csv", 
      sep = "/"), row.names = FALSE)
    
    # Improve format 
    tab2[, stats] <- apply(tab2[, stats], 2, round, 0)
    tab2[, stats] <- apply(tab2[, stats], 2, format, big.mark = ",")
    tab2[, stats] <- apply(tab2[, stats],
      2, function(x) {trimws(as.character(x))} )
    tab2[, "deaths"] <- paste(tab2[, "mean"], " (", tab2[, "lci"], " to ",
        tab2[, "uci"], ")", sep = "")
    
    # Write pretty output
    write.csv(tab2, paste(dir_path, "outputs/out_tab_age_gender_pretty.csv", 
      sep = "/"), row.names = FALSE)

    # Reshape wide
    tab3 <- tab2[, c("scenario", "age", "period", "gender", "deaths")]
    tab3 <- reshape(tab3, direction = "wide", timevar = "scenario",
      v.names = "deaths", idvar =  c("period", "age", "gender"), sep = "_" )
    tab3 <- tab3[order(tab3$gender, tab3$age, tab3$period), ]
    write.csv(tab3, paste(dir_path,"outputs/out_tab_age_gender_wide_pretty.csv",
      sep = "/"), row.names = FALSE)


  #...................................
  ## Tabulate maternal/neonatal deaths + stillbirths due to injury, by scenario

    # Maternal deaths
      # Compute female deaths in 15-49y age group, by scenario
      x <- subset(out, gender == "female" & 
        age %in% c("15 to 19yo", "20 to 29yo", "30 to 39yo", "40 to 49yo"))
      x <- aggregate(list(d_all = x$d_all), by = x[, c("run", "scenario")], 
        FUN = sum)
      x <- aggregate(list(d_all = x$d_all), by = list(scenario = x$scenario),
        FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
      
      # Multiply all deaths by estimated percent pregnant or post-partum
      x1 <- x
      x1[, grep("d_all", colnames(x1))] <- x1[, grep("all", colnames(x1))] * 
        0.1134
      x1$category <- "maternal"
      
    # Stillbirths
      x2 <- x
      x2[, grep("d_all", colnames(x1))]<- x2[, grep("d_all", colnames(x1))] * 
        0.0292
      x2$category <- "stillbirths"
      
    # Neonatal deaths
      # Compute neonatal deaths, by scenario
      x3 <- subset(out, age == "0mo")
      x3 <- aggregate(list(d_all = x3$d_all), by = x3[, c("run", "scenario")], 
        FUN = sum)
      x3 <- aggregate(list(d_all = x3$d_all), by = list(scenario = x3$scenario),
        FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
      x3$category <- "neonatal"
      
    # Output
    x <- rbind(x1, x2, x3)
    x <- data.frame(x$scenario, x$category, x$d_all)
    colnames(x) <- c("scenario","category", "mean", "lci", "uci")
    write.csv(x, paste(dir_path, "outputs/out_mnh_injuries.csv", 
      sep = "/"), row.names = FALSE)    

 
  #...................................
  ## Tabulate injuries and deaths by scenario, age, subperiod and type

    # Aggregate by scenario, age and subperiod
    stats <- c("mean", "lci", "uci")
    cols <- c("i_all", "d_all", "d_uncounted", "d_dow", "d_ord")
    x1 <- aggregate(out[, cols], 
      by = out[, c("run", "scenario", "period")], FUN = sum)
    x1$d_ord <- na.replace(x1$d_ord, 0)
    tab4 <- aggregate(x1[, cols], 
      by = x1[, c("scenario", "period")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    tab4 <- data.frame(tab4[, c("scenario", "period")], 
      tab4$i_all, tab4$d_all, tab4$d_uncounted, tab4$d_dow, tab4$d_ord)
    colnames(tab4) <- c("scenario", "period",
      paste(rep(cols, each = length(stats)), stats, sep = "_") )
    
    # Add figures for total period
    x1 <- aggregate(out[, cols], 
      by = out[, c("run", "scenario")], FUN = sum)
    x1$d_ord <- na.replace(x1$d_ord, 0)
    x2 <- aggregate(x1[, cols], 
      by = list(scenario = x1$scenario),
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    x2 <- data.frame(x2$scenario, 
      x2$i_all, x2$d_all, x2$d_uncounted, x2$d_dow, x2$d_ord)
    x2$period <- "total"
    x <- paste(rep(cols, each = length(stats)), stats, sep = "_")
    colnames(x2) <- c("scenario",
      x, "period" )
    x2 <- x2[, colnames(tab4)]
    tab4 <- rbind(tab4, x2)
    
    tab5 <- tab4
    tab5[, x] <- apply(tab5[, x], 2, round, 0)
    tab5[, x] <- apply(tab5[, x], 2, format, big.mark = ",")
    tab5[, x] <- apply(tab5[, x],
      2, function(x) {trimws(as.character(x))} )
    for (i in cols) {
      tab5[, i] <- paste(tab5[, paste(i, "mean", sep = "_")], " (", 
        tab5[, paste(i, "lci", sep = "_")], " to ",
          tab5[, paste(i, "uci", sep = "_")], ")", sep = "")
    }
    
    # Reduce columns
    tab5 <- tab5[, c("scenario", "period", cols)]
    
    # Reshape first long than wide
    tab6 <- tab5
    tab6 <- reshape(tab6, direction = "long", varying = cols,
      v.names = "number", timevar = "type", times = cols, 
      idvar = c("period", "scenario") )
    tab6 <- reshape(tab6, direction = "wide", timevar = "scenario",
      idvar =  c("type", "period"), sep = "_" )
    
    # Save
    write.csv(tab6, paste(dir_path, "outputs/out_scenario_type_wide_pretty.csv", 
      sep = "/"), row.names = FALSE)    
    
    
       
#...............................................................................  
### Graphing the estimates
#...............................................................................

  #...................................
  ## Bar plot of deaths and deaths due to wounds, by scenario and subperiod

    # Select data
    df_plot <- subset(tab4, period != "total")
    df_plot$scenario <- factor(df_plot$scenario, levels = scenarios)
    
    # Plot
    ggplot(data = df_plot, aes(x = scenario, y = d_all_mean,
      fill = scenario, colour = scenario)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
      geom_errorbar(aes(x = scenario, ymin = d_all_lci, ymax = d_all_uci,
        colour = scenario), width = 0.5, linetype = "21") +
      facet_wrap(. ~ period, ncol = 2) +
      scale_y_continuous("projected traumatic injury deaths",
        breaks = seq(0, 40000, 5000), labels = comma) +
      theme_bw() +
      theme(legend.position = "none", panel.grid.major.x = element_blank()) +
      scale_fill_manual(values = palette_periods[c(3,4,5)]) +
      scale_colour_manual(values = palette_periods[c(3,4,5)]) +
      geom_bar(aes(x = scenario, y = d_dow_mean),
        stat = "identity", position = "identity", alpha = 0.8)

    # Save
   ggsave(paste(dir_path, 'outputs/', "deaths_dow_by_scenario.png", sep = ""),
      dpi = "print", units = "cm", height = 12, width = 22)

  #...................................
  ## Age-gender pyramid of population and injury deaths projected

    # Generate age-gender proportions of population
      # reshape long
      males <- pop
      males$pop <- males$pop * males$prop_m
      males$sex <- "male"
      females <- pop
      females$pop <- females$pop * (1 - females$prop_m)
      females$sex <- "female"
      pop_dist <- rbind(males, females)
      
      # group population into 10-year bands
      pop_dist$age <- gsub("to", " to ", pop_dist$age)
      x <- data.frame(age_10 = c(rep("0 to 9yo", 4), rep("10 to 19yo", 2),
        ages[7:length(ages)]), age = ages)
      pop_dist <- merge(pop_dist, x, by = "age", all.x= TRUE)
      pop_dist <- aggregate(list(pop = pop_dist$pop), 
        by = pop_dist[, c("sex", "age_10")], FUN = sum)
      pop_dist$prop <- pop_dist$pop / sum(pop_dist$pop)
      pop_dist <- pop_dist[, c("sex", "age_10", "prop")]
      colnames(pop_dist) <- c("sex", "age", "proportion")
        
    # Projected injury deaths
      # aggregate again
      x1 <- aggregate(list(d_all = out$d_all), 
        by = out[, c("run", "scenario", "age", "gender")], FUN = sum)
      tab7 <- aggregate(list(d_all = x1$d_all), 
      by = x1[, c("scenario", "age", "gender")], mean)
      tab7 <- data.frame(tab7[, c("scenario", "age", "gender")], 
      tab7$d_all)
      colnames(tab7) <- c("scenario", "age", "gender", "mean")
      tab7 <- subset(tab7, scenario == "status quo")   
  
      # group age-gender proportion of projected injury deaths into 10-y bands
      tab7 <- merge(tab7, x, by = "age", all.x = TRUE)
      tab7 <- aggregate(list(mean = tab7$mean), 
        by = tab7[, c("gender", "age_10")], FUN = sum)
      tab7$prop <- tab7$mean / sum(tab7$mean)
      tab7 <- tab7[, c("gender", "age_10", "prop")]
      colnames(tab7) <- c("sex", "age", "proportion")
    
    # Plot
    ggplot(pop_dist, aes(x = age, colour = sex,
      y = ifelse(sex == "male", - proportion, proportion))) +
      geom_bar(stat = "identity", alpha = 0, linewidth = 1) +
      scale_colour_manual(values = palette_gen[c(3, 10)]) +
      geom_bar(data = tab7, alpha = 0.7, width = 0.8, aes(x = age, fill = sex, 
      y = ifelse(sex == "male", - proportion, proportion)),stat = "identity")+
      scale_fill_manual(values = palette_gen[c(3, 10)]) +      
      coord_flip() +
      theme_bw() +
      theme(legend.position = "top") +
      scale_y_continuous("percentage", 
        labels = function(z) paste0(abs(z) * 100, "%"))

    # Save
    ggsave(paste(dir_path, 'outputs/', "pop_pyramids_compared.png", sep = ""),
      dpi = "print", units = "cm", height = 15, width = 18)
      

#...............................................................................  
### ENDS
#...............................................................................      
      