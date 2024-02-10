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
  ## Investigate the stability of runs
  out_epid <- out_epid[order(out_epid$run, out_epid$scenario, 
    out_epid$disease), ]
  ggplot(data = subset(out_epid, age == "12 to 59mo"), aes(x = run, y = deaths,
    colour = scenario, fill = scenario)) +
    geom_line(alpha = 0.7) +
    facet_grid(scenario ~ disease) +
    theme_bw() +
    scale_colour_manual(values = palette_periods[c(3,4,5)]) +
    scale_fill_manual(values = palette_periods[c(3,4,5)]) +
    theme(legend.position = "top")
  
  ggsave(paste(dir_path, 'outputs/' , "epid_run_distributions.png", sep=""),
    dpi = "print", units = "cm", width = 30, height = 12)
  
  
  #...................................      
  ## Tabulate deaths by scenario, disease, age group and subperiod
  tab1 <- aggregate(list(deaths = out_epid$deaths),by = out_epid[, c("scenario",
    "disease", "subperiod", "age")], FUN = function(x) {
      return(c(mean(x), quantile(x, c(0.5, 0.025, 0.975))) )} )
  tab1 <- data.frame(tab1[, c("scenario", "disease", "subperiod", "age")],
    tab1$deaths)
  colnames(tab1) <- c("scenario", "disease", "subperiod", "age",
    "deaths_mean", "deaths_median", "deaths_lci", "deaths_uci")
  write.csv(tab1, paste(dir_path, "outputs/out_tab_epid_all.csv", sep = "/"),
    row.names = FALSE)

  #...................................      
  ## Tabulate deaths by scenario, disease, and subperiod
    
    # Aggregate
    tab2 <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "disease", "subperiod", "run")], FUN = sum)
    tab2 <- aggregate(list(deaths = tab2$deaths),
      by = tab2[, c("scenario", "disease", "subperiod")], 
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.5, 0.025, 0.975))))})
    tab2[, grep("deaths", colnames(tab2))] <- apply(
      tab2[, grep("deaths", colnames(tab2))], 2, round, 0)
    tab2 <- data.frame(tab2[, c("scenario", "disease", "subperiod")],
      unlist(tab2$deaths))
    colnames(tab2) <- c("scenario", "disease", "subperiod", "mean", "median",
      "lci", "uci")
    tab2 <- subset(tab2, select = -median)
        
    # Add totals for the entire subperiod
    x <- aggregate(tab2[, c("mean", "lci", "uci")], by = 
      tab2[, c("scenario", "disease")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "disease", "subperiod", "mean", "lci", "uci")]
    tab2 <- rbind(tab2, x)
    
    # Output raw table
    write.csv(tab2, paste(dir_path, "outputs/out_tab_epid_dis.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab2[, c("mean", "lci", "uci")] <- apply(
      tab2[, c("mean", "lci", "uci")], 2, format, big.mark = ",")
    tab2[, c("mean", "lci", "uci")] <- apply(tab2[, c("mean", "lci", "uci")],
      2, function(x) {trimws(as.character(x))} )
    tab2$deaths <- paste(tab2$mean, " (", tab2$lci, " to ", tab2$uci, ")", 
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
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.5, 0.025, 0.975))))})
    tab3[, grep("deaths", colnames(tab3))] <- apply(
      tab3[, grep("deaths", colnames(tab3))], 2, round, 0)
    tab3 <- data.frame(tab3[, c("scenario", "age", "subperiod")],
      unlist(tab3$deaths))
    colnames(tab3) <- c("scenario", "age", "subperiod", "mean", "median", "lci", 
      "uci")
    tab3 <- subset(tab3, select = -median)
        
    # Add totals for the entire subperiod
    x <- aggregate(tab3[, c("mean", "lci", "uci")], by = 
      tab3[, c("scenario", "age")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "age", "subperiod", "mean", "lci", "uci")]
    tab3 <- rbind(tab3, x)
    
    # Output raw table
    write.csv(tab3, paste(dir_path, "outputs/out_tab_epid_age.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab3[, c("mean", "lci", "uci")] <- apply(
      tab3[, c("mean", "lci", "uci")], 2, format, big.mark = ",")
    tab3[, c("mean", "lci", "uci")] <- apply(tab3[, c("mean", "lci", "uci")],
      2, function(x) {trimws(as.character(x))} )
    tab3$deaths <- paste(tab3$mean, " (", tab3$lci, " to ", tab3$uci, ")", 
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
  ## Tabulate deaths by scenario and subperiod
    
    # Aggregate
    tab4 <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "subperiod", "run")], FUN = sum)
    tab4 <- aggregate(list(deaths = tab4$deaths),
      by = tab4[, c("scenario", "subperiod")], 
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.5, 0.025, 0.975))))})
    tab4[, grep("deaths", colnames(tab4))] <- apply(
      tab4[, grep("deaths", colnames(tab4))], 2, round, 0)
    tab4 <- data.frame(tab4[, c("scenario", "subperiod")],
      unlist(tab4$deaths))
    colnames(tab4) <- c("scenario", "subperiod", "mean", "median",
      "lci", "uci")
    tab4 <- subset(tab4, select = -median)
        
    # Add totals for the entire subperiod
    x <- aggregate(tab4[, c("mean", "lci", "uci")], by = 
      list(scenario = tab4$scenario), FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "subperiod", "mean", "lci", "uci")]
    tab4 <- rbind(tab4, x)
    
    # Output raw table
    write.csv(tab4, paste(dir_path, "outputs/out_tab_epid.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab4[, c("mean", "lci", "uci")] <- apply(
      tab4[, c("mean", "lci", "uci")], 2, format, big.mark = ",")
    tab4[, c("mean", "lci", "uci")] <- apply(tab4[, c("mean", "lci", "uci")],
      2, function(x) {trimws(as.character(x))} )
    tab4$deaths <- paste(tab4$mean, " (", tab4$lci, " to ", tab4$uci, ")", 
      sep = "")
    tab4 <- tab4[, c("scenario", "subperiod", "deaths")]
  
    # Reshape wide  
    tab4 <- reshape(tab4, direction = "wide", timevar = "scenario",
      times = "deaths", idvar = "subperiod", 
      varying = list(tab4$scenario) )
    tab4 <- tab4[order(tab4$subperiod), ]
    tab4 <- tab4[, c("subperiod", scenarios)]
    write.csv(tab4, paste(dir_path, "outputs/out_tab_epid_pretty.csv", 
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
    plot1 <- ggplot(df, aes(y = atleast100, x = scenario, colour = scenario, 
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
    

  #...................................      
  ## Graph the proportion of runs with >=100(0)(0) epidemic deaths overall
    
    # Aggregate
    df <- aggregate(list(deaths = out_epid$deaths),
      by = out_epid[, c("scenario", "run")], FUN = sum)
    
    # Compute frequency of runs with >= 100(0)(0) deaths
    df$atleast100 <- ifelse(df$deaths >= 100, TRUE, FALSE)
    df$atleast1000 <- ifelse(df$deaths >= 1000, TRUE, FALSE)
    df$atleast10000 <- ifelse(df$deaths >= 10000, TRUE, FALSE)
    df <- aggregate(df[, grep("atleast", colnames(df))],
      by = list(scenario = df$scenario), FUN = mean)      
    df$scenario <- factor(df$scenario, levels = scenarios)
    
    # Reshape long
    df <- reshape(df, direction = "long", v.names = "probability",
      varying = grep("atleast", colnames(df)), idvar = "scenario",
      times = c(100, 1000, 10000), timevar = "n_deaths")
    df$n_deaths <- paste(">=", 
      trimws(as.character(format(df$n_deaths, big.mark = ","))), sep = " ")
    df$n_deaths <- factor(df$n_deaths, levels = c(">= 100", ">= 1,000",
      ">= 10,000"))

    # Plot
    plot2 <- ggplot(df, aes(y = probability, x = n_deaths, colour = scenario, 
      fill = scenario)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      theme_bw() +
      facet_wrap(. ~ scenario) +
      theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank()) +
      scale_colour_manual(values = palette_periods[3:5]) +
      scale_fill_manual(values = palette_periods[3:5]) +
      scale_y_continuous("probability of occurrence") +
      scale_x_discrete(
        "number of deaths (all epidemic-prone pathogens combined)")
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "prob_epidemics_by_size.png", sep=""),
      dpi = "print", units = "cm", width = 30, height = 15)
       
  #...................................      
  ## Combined plot
    
    # Plot
    ggarrange(plot1, NULL, plot2, common.legend = TRUE, 
      labels = c("A", "", "B"), heights = c(1, 0.1, 0.5), ncol = 1, nrow = 3)
    
    # Save
    ggsave(paste(dir_path, 'outputs/' , "prob_epidemics_combi.png", sep=""),
      dpi = "print", units = "cm", width = 20, height = 25)

                
#...............................................................................   
### Analysing simulations for stable-transmission infections
#...............................................................................

  #...................................      
  ## Read and format output as needed

   # Read simulation output if not already in environment
    if (! exists("out_ende")) {out_ende <- read_rds(paste(dir_path,
      'outputs/',"out_ende_all_runs.rds", sep=""))}
        
    # Format age
    out_ende$age <- gsub("to", " to ", out_ende$age)

    # Compute excess deaths
    out_ende$d_excess <- out_ende$d_crisis - out_ende$d_base
    
    # Output columns
    cols <- c(paste("d_base", c("mean", "lci", "uci"), sep = "_"),
      paste("d_crisis", c("mean", "lci", "uci"), sep = "_"),
      paste("d_excess", c("mean", "lci", "uci"), sep = "_"))

    
  #...................................      
  ## Tabulate deaths by scenario, disease, age group and subperiod
  tab1 <- aggregate(list(out_ende[, grep("d_", colnames(out_ende))]),
    by = out_ende[, c("scenario", "disease", "subperiod", "age")], 
    FUN = function(x) {return(c(mean(x), quantile(x, c(0.5, 0.025, 0.975))) )} )
  tab1 <- data.frame(tab1[, c("scenario", "disease", "subperiod", "age")],
    tab1$d_base, tab1$d_crisis, tab1$d_excess)
  colnames(tab1) <- c("scenario", "disease", "subperiod", "age", 
    paste("d_base", c("mean", "median", "lci", "uci"), sep = "_"),
    paste("d_crisis", c("mean", "median", "lci", "uci"), sep = "_"),
    paste("d_excess", c("mean", "median", "lci", "uci"), sep = "_")
  )
  # tab1[, grep("d_", colnames(tab1))] <- apply(
  #   tab1[, grep("d_", colnames(tab1))], 2, round, 0)
  write.csv(tab1, paste(dir_path, "outputs/out_tab_ende_all.csv", sep = "/"),
    row.names = FALSE)

  #...................................      
  ## Tabulate deaths by scenario, disease, and subperiod
    
    # Aggregate
    tab2 <- aggregate(out_ende[, grep("d_", colnames(out_ende))],
      by = out_ende[, c("scenario", "disease", "subperiod", "run")], FUN = sum)
    tab2 <- aggregate(tab2[, grep("d_", colnames(tab2))],
      by = tab2[, c("scenario", "disease", "subperiod")], 
      FUN = function(x) {quantile(x, c(0.5, 0.025, 0.975) )} )
    tab2 <- data.frame(tab2[, c("scenario", "disease", "subperiod")],
      tab2$d_base, tab2$d_crisis, tab2$d_excess)
    colnames(tab2) <- c("scenario", "disease", "subperiod", cols)
    tab2[, grep("d_", colnames(tab2))] <- apply(
      tab2[, grep("d_", colnames(tab2))], 2, round, 0)

    # Add totals for the entire subperiod
    x <- aggregate(tab2[, cols], by = 
      tab2[, c("scenario", "disease")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "disease", "subperiod", cols)]
    tab2 <- rbind(tab2, x)
    
    # Output raw table
    write.csv(tab2, paste(dir_path, "outputs/out_tab_ende_dis.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab2[, cols] <- apply(tab2[, cols], 2, format, big.mark = ",")
    tab2[, cols] <- apply(tab2[, cols],
      2, function(x) {trimws(as.character(x))} )
    for (i in c("base", "crisis", "excess")) {
      tab2[, paste("deaths", i, sep = "_")] <- 
        paste(tab2[, paste("d", i, "mean", sep = "_")], " (", 
          tab2[, paste("d", i, "lci", sep = "_")], " to ",
          tab2[, paste("d", i, "uci", sep = "_")], ")", sep = "")       
    }
    tab2 <- tab2[, c("scenario", "disease", "subperiod",
      "deaths_base", "deaths_crisis", "deaths_excess")]
  
    # Write 
    write.csv(tab2, paste(dir_path, "outputs/out_tab_ende_dis_pretty.csv", 
      sep = "/"), row.names = FALSE)
    
  #...................................      
  ## Tabulate deaths by scenario, age, and subperiod

    # Aggregate
    tab3 <- aggregate(out_ende[, grep("d_", colnames(out_ende))],
      by = out_ende[, c("scenario", "age", "subperiod", "run")], FUN = sum)
    tab3 <- aggregate(tab3[, grep("d_", colnames(tab3))],
      by = tab3[, c("scenario", "age", "subperiod")], 
      FUN = function(x) {quantile(x, c(0.5, 0.025, 0.975) )} )
    tab3 <- data.frame(tab3[, c("scenario", "age", "subperiod")],
      tab3$d_base, tab3$d_crisis, tab3$d_excess)
    colnames(tab3) <- c("scenario", "age", "subperiod", cols)
    tab3[, grep("d_", colnames(tab3))] <- apply(
      tab3[, grep("d_", colnames(tab3))], 2, round, 0)

    # Add totals for the entire subperiod
    x <- aggregate(tab3[, cols], by = 
      tab3[, c("scenario", "age")], FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "age", "subperiod", cols)]
    tab3 <- rbind(tab3, x)
    
    # Output raw table
    write.csv(tab3, paste(dir_path, "outputs/out_tab_ende_age.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab3[, cols] <- apply(tab3[, cols], 2, format, big.mark = ",")
    tab3[, cols] <- apply(tab3[, cols],
      2, function(x) {trimws(as.character(x))} )
    for (i in c("base", "crisis", "excess")) {
      tab3[, paste("deaths", i, sep = "_")] <- 
        paste(tab3[, paste("d", i, "mean", sep = "_")], " (", 
          tab3[, paste("d", i, "lci", sep = "_")], " to ",
          tab3[, paste("d", i, "uci", sep = "_")], ")", sep = "")       
    }
    tab3 <- tab3[, c("scenario", "age", "subperiod",
      "deaths_base", "deaths_crisis", "deaths_excess")]
  
    # Write 
    write.csv(tab3, paste(dir_path, "outputs/out_tab_ende_age_pretty.csv", 
      sep = "/"), row.names = FALSE)
    
    
  #...................................      
  ## Tabulate deaths by scenario and subperiod

    # Aggregate
    tab4 <- aggregate(out_ende[, grep("d_", colnames(out_ende))],
      by = out_ende[, c("scenario", "subperiod", "run")], FUN = sum)
    tab4 <- aggregate(tab4[, grep("d_", colnames(tab4))],
      by = tab4[, c("scenario", "subperiod")], 
      FUN = function(x) {quantile(x, c(0.5, 0.025, 0.975) )} )
    tab4 <- data.frame(tab4[, c("scenario", "subperiod")],
      tab4$d_base, tab4$d_crisis, tab4$d_excess)
    colnames(tab4) <- c("scenario", "subperiod", cols)
    tab4[, grep("d_", colnames(tab4))] <- apply(
      tab4[, grep("d_", colnames(tab4))], 2, round, 0)

    # Add totals for the entire subperiod
    x <- aggregate(tab4[, cols], by = list(scenario = tab4$scenario), FUN = sum)
    x$subperiod <- "total"
    x <- x[, c("scenario", "subperiod", cols)]
    tab4 <- rbind(tab4, x)
    
    # Output raw table
    write.csv(tab4, paste(dir_path, "outputs/out_tab_ende.csv", sep = "/"),
      row.names = FALSE)
    
    # Improve numbers format
    tab4[, cols] <- apply(tab4[, cols], 2, format, big.mark = ",")
    tab4[, cols] <- apply(tab4[, cols],
      2, function(x) {trimws(as.character(x))} )
    for (i in c("base", "crisis", "excess")) {
      tab4[, paste("deaths", i, sep = "_")] <- 
        paste(tab4[, paste("d", i, "mean", sep = "_")], " (", 
          tab4[, paste("d", i, "lci", sep = "_")], " to ",
          tab4[, paste("d", i, "uci", sep = "_")], ")", sep = "")       
    }
    tab4 <- tab4[, c("scenario", "subperiod",
      "deaths_base", "deaths_crisis", "deaths_excess")]
  
    # Write 
    write.csv(tab4, paste(dir_path, "outputs/out_tab_ende_pretty.csv", 
      sep = "/"), row.names = FALSE)
  

#...............................................................................   
### ENDS
#...............................................................................
