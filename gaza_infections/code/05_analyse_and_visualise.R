#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO ANALYSE AND VISUALISE SIMULATIONS FOR INFECTIONS MODEL  --- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Visualising expert-elicited parameter distributions
#...............................................................................

  #...................................      
  ## Format output as needed
  
    # Read distributions if not already in environment
    if (! exists("see")) {see <- read_rds(paste(dir_path,
      'inputs/',"see_distributions.rds", sep=""))}

    # Add probability densities
    x <- apply(see[, grep("pcum_", colnames(see))], 1, diff)
    see[, gsub("pcum", "p", colnames(see)[grep("pcum", colnames(see))])] <- 
      cbind(see$pcum_1, t(x))
    
    # Reshape long
    see_long <- reshape(see, direction = "long", 
      varying = list(x = grep("x_", colnames(see), value = TRUE), 
        pcum = grep("p_", colnames(see), value = TRUE)),
      v.names = c("x", "p"), timevar = "id", idvar = c("expert", cols) )
    see_long$scenario <- factor(see_long$scenario, levels = scenarios)

  #...................................      
  ## Graph probabilities of epidemic
  
    # Select data
    df <- subset(see_long, parameter == "pu")
    
    # Plot
    ggplot(data = df, aes(x = x, y = p, group = expert, colour = scenario,
      linewidth = expert, linetype = expert)) +
      geom_line() +
      scale_linewidth_manual(
        values = c(1, rep(0.5, unique(length(df$expert) - 1))) ) +
      scale_linetype_manual(
        values = c("solid", rep("21", unique(length(df$expert) - 1))) ) +
      scale_colour_manual(
        values = palette_periods[names(palette_periods) %in% scenarios]) +
      facet_grid(disease ~ scenario) +
      theme_bw() +
      scale_x_continuous(
        "probability of an epidemic during 6 months of projection period") +
      scale_y_continuous("expert-elicited probability density") +
      theme(legend.position = "none", strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "see_distributions_pu.png", sep=""),
      dpi = "print", units = "cm", width = 18, height = 20)

  ## Graph CFR of exemplar diseases
  
    # Select data
    df <- subset(see_long, parameter == "cfr")
    
    # Plot
    ggplot(data = df, aes(x = x, y = p, group = expert, colour = scenario,
      linewidth = expert, linetype = expert)) +
      geom_line() +
      scale_linewidth_manual(
        values = c(1, rep(0.5, unique(length(df$expert) - 1))) ) +
      scale_linetype_manual(
        values = c("solid", rep("21", unique(length(df$expert) - 1))) ) +
      scale_colour_manual(
        values = palette_periods[names(palette_periods) %in% scenarios]) +
      facet_grid(scenario ~ disease + subperiod) +
      theme_bw() +
      scale_x_continuous(
        "case-fatality ratio of symptomatic cases", labels = percent) +
      scale_y_continuous("expert-elicited probability density") +
      theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "see_distributions_cfr.png", sep=""),
      dpi = "print", units = "cm", width = 25, height = 15)

  ## Graph R0 of exemplar diseases
  
    # Select data
    df <- subset(see_long, parameter == "r0")
    
    # Plot
    ggplot(data = df, aes(x = x, y = p, group = expert, colour = scenario,
      linewidth = expert, linetype = expert)) +
      geom_line() +
      scale_linewidth_manual(
        values = c(1, rep(0.5, unique(length(df$expert) - 1))) ) +
      scale_linetype_manual(
        values = c("solid", rep("21", unique(length(df$expert) - 1))) ) +
      scale_colour_manual(
        values = palette_periods[names(palette_periods) %in% scenarios]) +
      facet_grid(scenario ~ disease + subperiod, scales = "free_x") +
      theme_bw() +
      scale_x_continuous(
        "basic reproduction number") +
      scale_y_continuous("expert-elicited probability density") +
      theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "see_distributions_r0.png", sep=""),
      dpi = "print", units = "cm", width = 25, height = 15)

    
#...............................................................................   
### Analysing simulations for epidemic infections
#...............................................................................

  #...................................      
  ## Read and format output as needed

   # Read simulation output if not already in environment
    if (! exists("out_epid")) {out_epid <- read_rds(paste(dir_path,
      'outputs/',"out_epid_all_runs.rds", sep=""))}
        
    # Format age
    out_epid$age <- gsub("to", " to ", out_epid$age)

  #...................................      
  ## Tabulate deaths by scenario, disease, age group and subperiod
  tab1 <- aggregate(list(deaths = out_epid$deaths),by = out_epid[, c("scenario",
    "disease", "subperiod", "age")], FUN = function(x) {
      quantile(x, c(0.5, 0.025, 0.975) )} )
  tab1[, grep("deaths", colnames(tab1))] <- apply(
    tab1[, grep("deaths", colnames(tab1))], 2, round, -1)
  write.csv(tab1, paste(dir_path, "outputs/out_tab_epid_all.csv", sep = "/"),
    row.names = FALSE)

  #...................................      
  ## Tabulate deaths by scenario, disease, and subperiod
    
    # Aggregate
    tab2 <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "disease", "subperiod", "run")], FUN = sum)
    tab2 <- aggregate(list(deaths = tab2$deaths),
      by = tab2[, c("scenario", "disease", "subperiod")], 
      FUN = function(x) {quantile(x, c(0.5, 0.025, 0.975) )} )
    tab2[, grep("deaths", colnames(tab2))] <- apply(
      tab2[, grep("deaths", colnames(tab2))], 2, round, -1)
    tab2 <- data.frame(tab2[, c("scenario", "disease", "subperiod")],
      unlist(tab2$deaths))
    colnames(tab2) <- c("scenario", "disease", "subperiod", "median", "lci", 
      "uci")
        
    # Add totals for the entire subperiod
    x <- aggregate(tab2[, c("median", "lci", "uci")], by = 
      tab2[, c("scenario", "disease")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "disease", "subperiod", "median", "lci", "uci")]
    tab2 <- rbind(tab2, x)
    
    # Output raw table
    write.csv(tab2, paste(dir_path, "outputs/out_tab_epid_dis.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab2[, c("median", "lci", "uci")] <- apply(
      tab2[, c("median", "lci", "uci")], 2, format, big.mark = ",")
    tab2[, c("median", "lci", "uci")] <- apply(tab2[, c("median", "lci", "uci")],
      2, function(x) {trimws(as.character(x))} )
    tab2$deaths <- paste(tab2$median, " (", tab2$lci, " to ", tab2$uci, ")", 
      sep = "")
    tab2 <- tab2[, c("scenario", "disease", "subperiod", "deaths")]
  
    # Reshape wide  
    tab2 <- reshape(tab2, direction = "wide", timevar = "scenario",
      times = "deaths", idvar = c("disease", "subperiod"), 
      varying = list(tab2$scenario) )
    tab2 <- tab2[order(tab2$disease, tab2$subperiod), ]
    tab2 <- tab2[, c("disease", "subperiod", scenarios)]
    write.csv(tab2, paste(dir_path, "outputs/out_tab_epid_dis_pretty.csv", 
      sep = "/"), row.names = FALSE)
    
  #...................................      
  ## Tabulate deaths by scenario, age, and subperiod

    # Aggregate
    tab3 <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "age", "subperiod", "run")], FUN = sum)
    tab3 <- aggregate(list(deaths = tab3$deaths),
      by = tab3[, c("scenario", "age", "subperiod")], 
      FUN = function(x) {quantile(x, c(0.5, 0.025, 0.975) )} )
    tab3[, grep("deaths", colnames(tab3))] <- apply(
      tab3[, grep("deaths", colnames(tab3))], 2, round, -1)
    tab3 <- data.frame(tab3[, c("scenario", "age", "subperiod")],
      unlist(tab3$deaths))
    colnames(tab3) <- c("scenario", "age", "subperiod", "median", "lci", 
      "uci")
        
    # Add totals for the entire subperiod
    x <- aggregate(tab3[, c("median", "lci", "uci")], by = 
      tab3[, c("scenario", "age")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "age", "subperiod", "median", "lci", "uci")]
    tab3 <- rbind(tab3, x)
    
    # Output raw table
    write.csv(tab3, paste(dir_path, "outputs/out_tab_epid_age.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab3[, c("median", "lci", "uci")] <- apply(
      tab3[, c("median", "lci", "uci")], 2, format, big.mark = ",")
    tab3[, c("median", "lci", "uci")] <- apply(tab3[, c("median", "lci", "uci")],
      2, function(x) {trimws(as.character(x))} )
    tab3$deaths <- paste(tab3$median, " (", tab3$lci, " to ", tab3$uci, ")", 
      sep = "")
    tab3 <- tab3[, c("scenario", "age", "subperiod", "deaths")]
  
    # Reshape wide  
    tab3 <- reshape(tab3, direction = "wide", timevar = "scenario",
      times = "deaths", idvar = c("age", "subperiod"), 
      varying = list(tab3$scenario) )
    tab3 <- tab3[order(tab3$age, tab3$subperiod), ]
    tab3 <- tab3[, c("age", "subperiod", scenarios)]
    write.csv(tab3, paste(dir_path, "outputs/out_tab_epid_age_pretty.csv", 
      sep = "/"), row.names = FALSE)

  #...................................      
  ## Graph the proportion of runs in which >=100 deaths are observed, by disease
    
    # Aggregate
    df <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "disease", "run")], FUN = sum)
    
    # Compute frequency of runs with >= 100 deaths
    df$atleast100 <- ifelse(df$deaths >= 100, TRUE, FALSE)
    df <- aggregate(list(atleast100 = df$atleast100),
      by = df[, c("disease", "scenario")], FUN = mean)      
    df$scenario <- factor(df$scenario, levels = scenarios)
    
    # Plot
    ggplot(df, aes(y = atleast100, x = scenario, colour = scenario, 
      fill = scenario)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      theme_bw() +
      facet_wrap(. ~ disease, ncol = 4) +
      theme(legend.position = "top", axis.text.x = element_blank(),
        axis.title.x = element_blank(), panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
      scale_colour_manual(values = palette_periods[3:5]) +
      scale_fill_manual(values = palette_periods[3:5]) +
      scale_y_continuous("probability of an epidemic with >= 100 deaths",
        limits = c(0, 0.20))
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "prob_epidemics_100deaths.png", sep=""),
      dpi = "print", units = "cm", width = 20, height = 20)
    
        
#...............................................................................   
### Analysing simulations for stable-transmission infections
#...............................................................................



#...............................................................................   
### ENDS
#...............................................................................
