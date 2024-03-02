#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO PROJECT GAM AND SAM PREVALENCE IN KIDS  ---------- ##
#...............................................................................



#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Generate baseline weight-for-height Z scores and SAM/GAM prevalence
    # plus proportion of flags
    
    # Add anthropometric indices
    x <- with(df_gm,
      anthro::anthro_zscores(sex = sex, age = age, weight = weight,
          lenhei = height, is_age_in_month = TRUE)
    )
    x <- cbind(df_gm, x)
    
    # Compute proportion of flagged observations
    prop_flags_base <- prop.table(table(x$fwfl))[["1"]]
      
    # Remove flagged WHZ observations (<> 5SD)
    x <- subset(x, fwfl != 1)
      
    # compute SAM and GAM prevalence (weighted means of categories)
    x$sam <- ifelse(x$zwfl < - 3, 1, 0)
    x$gam <- ifelse(x$zwfl < - 2, 1, 0)
        
    sam_base <- weighted.mean(x$sam, x$wt)
    gam_base <- weighted.mean(x$gam, x$wt)
    

  #...................................      
  ## Initialise or read other objects 

    # Read output runs from adult survey analysis, if not already in environment
    if (!exists("out_all") ) {out_all <- read_rds(paste(dir_path, "outputs/", 
      "out_wt_loss_adults_all_runs.rds", sep=""))}
      # rename columns
      colnames(out_all) <- gsub("percent_wt_loss_", "", colnames(out_all))
    
    # Range of relative risks of weight loss (children : adults)
    rr_range <- c(gen_pars[which(gen_pars$parameter == "wt_loss_child_min"), 
      "value_gen"],
      gen_pars[which(gen_pars$parameter == "wt_loss_child_max"), 
      "value_gen"] )
    rr_range <- as.numeric(rr_range)

    # Initialise output of each run
    out <- expand.grid(run = runs$run, scenario = scenarios, 
      period = c("pre-war", "to date", subperiods))
    out[, c("sam", "gam")] <- NA
    out <- out[order(out$run, out$scenario, out$period), ]
    
      # add baseline SAM and GAM
      out$period[ out$period == "baseline"] <- "pre-war"
      out[which(out$period == "pre-war"), "sam"] <- sam_base
      out[which(out$period == "pre-war"), "gam"] <- gam_base
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)
    
    
#...............................................................................  
### Estimating distribution and prevalence of GAM and SAM in children 6-59mo,
  # by scenario and period, for each simulation run
#...............................................................................

for (run_i in 1:nrow(runs)) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
  #...................................      
  ## Select random quantities needed for run

    # Identify random number from [0,1]
    rx <- runs[run_i, "rx"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  
    
    # Select random relative risk of weight loss (children : adults)
    rr <- rr_range[1] + rx * (rr_range[2] - rr_range[1])
  
  #...................................      
  ## For each scenario...
  for (i in scenarios) {
    
    #...and for each subperiod:
    for (j in c("to date", subperiods)) {
     
      # reduce weight of all children (= weight loss in adults +- uncertainty,
        # with uncertainty sampled based on value of rx for run)
      df_gm$weight_ij <- df_gm$weight *
        (1 - out_all[which(out_all$run == run_i & out_all$scenario == i &
          out_all$period == j), "percent_wt_loss"] * rr)
       
      # compute anthropometric indices and flags
      x <- with(df_gm,
        anthro::anthro_zscores(sex = sex, age = age, weight = weight_ij,
          lenhei = height, is_age_in_month = TRUE) )
      x <- cbind(df_gm, x)
            
      # remove flagged WHZ observations (<> 5SD)
      x <- subset(x, fwfl != 1)
      
      # compute SAM and GAM prevalence
      x$sam <- ifelse(x$zwfl < - 3, 1, 0)
      x$gam <- ifelse(x$zwfl < - 2, 1, 0)
          
      sam <- weighted.mean(x$sam, x$wt)
      gam <- weighted.mean(x$gam, x$wt)
      
      # update output with SAM and GAM prevalence
      out[which(out$run == run_i & out$scenario == i & out$period == j),
        c("sam", "gam")] <- c(sam, gam)
      
    }
  }
} # close run_i loop
close(pb)      
    
    
#...............................................................................  
### Outputting and visualising results
#...............................................................................

  #...................................      
  ## Compute average and uncertainty of runs, by scenario and period
    
    # Output raw simulation runs
    write_rds(out, paste(dir_path, "outputs/","out_gam_sam_all_runs.rds", 
      sep=""))

    # Compute mean, median, 95% percentile intervals of all estimated quantities    
    agg <- aggregate(out[, c("sam", "gam")], 
      by = out[, c("scenario", "period")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.50, 0.025, 0.975), 
        na.rm = TRUE)))})

    # Output percentile intervals
    write.csv(agg, paste(dir_path, "outputs/","out_gam_sam.csv", sep=""),
      row.names = FALSE)
    
    # Make a prettier table
    cols <- c("mean", "median", "lci", "uci")
    x <- data.frame(agg[,c("scenario", "period")], sam = agg$sam, gam = agg$gam)
    colnames(x) <- c("scenario", "period", paste("sam", cols, sep = "_"),
      paste("gam", cols, sep = "_"))
    x[, grep("sam", colnames(x))] <- apply(x[,grep("sam", colnames(x))], 2,
      label_percent(accuracy = 0.1) )
    x[, grep("gam", colnames(x))] <- apply(x[,grep("gam", colnames(x))], 2,
      label_percent(accuracy = 0.1) )
    x$sam <- paste(x$sam_mean, " (", x$sam_median, ", ", x$sam_lci, " to ",
      x$sam_uci, ")", sep = "")
    x$gam <- paste(x$gam_mean, " (", x$gam_median, ", ", x$gam_lci, " to ",
      x$gam_uci, ")", sep = "")
    x[, "sam"] <- ifelse(x$period == "pre-war", x$sam_mean, x$sam )
    x[, "gam"] <- ifelse(x$period == "pre-war", x$gam_mean, x$gam )
    x$scenario <- as.character(x$scenario)
    x$scenario <- ifelse(x$period %in% c("pre-war", "to date"), NA, x$scenario)
    x <- rbind(unique(subset(x, is.na(scenario))), subset(x, ! is.na(scenario)))
    x <- x[, c("period", "scenario", "sam", "gam")]
    
    # Save
    write.csv(x, paste(dir_path, "outputs/","out_gam_sam_pretty.csv", sep=""),
      row.names = FALSE)    
      
  #...................................      
  ## Visualise, by scenario and period
    
    # Prepare dataset
    agg <- data.frame(agg[, c("scenario", "period")], unlist(agg[["sam"]]),
      unlist(agg[["gam"]]))
    colnames(agg) <- c("scenario", "period", 
      paste("sam", c("mean", "median", "lci", "uci"), sep = "_"),
      paste("gam", c("mean", "median", "lci", "uci"), sep = "_")
    )
    agg <- subset(agg, select = -c(sam_median, gam_median))
    agg <- reshape(agg, direction = "long", timevar = "indicator",
      varying = list(mean = grep("_mean", colnames(agg), value = TRUE), 
        lci = grep("_lci", colnames(agg), value = TRUE),
        uci = grep("_uci", colnames(agg), value = TRUE)),
      v.names = c("mean", "lci", "uci"),
      times = c("severe acute malnutrition", "global acute malnutrition"))    
    
    # Separate out pre-war and to date periods
    x1 <- subset(agg, period == "pre-war" & scenario == "ceasefire")
    x1$scenario <- ""
    x2 <- subset(agg, period == "to date" & scenario == "ceasefire")
    x2$scenario <- ""
    x3 <- subset(agg, !period %in% c("to date", "pre-war"))
    
    # Limits for plots
    lims <- c(
      max(x3[which(x3$indicator == "global acute malnutrition"), "mean"]),
      max(x3[which(x3$indicator == "severe acute malnutrition"), "mean"] ) )       
    lims <- lims * 1.10
    
    # Plots
      # pre-war, GAM
      plot1gam <- ggplot(data = subset(x1, 
        indicator == "global acute malnutrition"), aes(x = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5,
          colour = palette_periods[1], fill = palette_periods[1]) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none", axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[1])) +
        geom_text(aes(x = scenario, y = mean * 1.05 + 0.02,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20")
        
      # to date, GAM
      plot2gam <- ggplot(data = subset(x2, 
        indicator == "global acute malnutrition"), aes(x = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5,
          colour = palette_periods[2], fill = palette_periods[2]) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none", axis.text.x = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[1])) +
        geom_text(aes(x = scenario, y = mean * 1.05 + 0.02,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20")
        
      # projection scenarios, GAM
      plot3gam <- ggplot(data = subset(x3, 
        indicator == "global acute malnutrition"), aes(x = scenario, 
        fill = scenario, colour = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[1])) +
        geom_text(aes(x = scenario, y = mean * 1.05 + 0.02,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20") +
        scale_colour_manual(values = palette_periods[c(3,4,5)]) +
        scale_fill_manual(values = palette_periods[c(3,4,5)])      

      # pre-war, SAM
      plot1sam <- ggplot(data = subset(x1, 
        indicator == "severe acute malnutrition"), aes(x = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5,
          colour = palette_periods[1], fill = palette_periods[1]) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none", axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[2])) +
        geom_text(aes(x = scenario, y = mean * 1.01 + 0.01,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20")
        
      # to date, SAM
      plot2sam <- ggplot(data = subset(x2, 
        indicator == "severe acute malnutrition"), aes(x = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5,
          colour = palette_periods[2], fill = palette_periods[2]) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none", axis.text.x = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[2])) +
        geom_text(aes(x = scenario, y = mean * 1.01 + 0.01,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20")
        
      # projection scenarios, SAM
      plot3sam <- ggplot(data = subset(x3, 
        indicator == "severe acute malnutrition"), aes(x = scenario, 
        fill = scenario, colour = scenario)) +
        geom_bar(aes(y = mean), stat = "identity", alpha = 0.5) +
        theme_bw() +
        facet_grid(. ~ period) +
        theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.x = element_blank()) +
        scale_y_continuous("prevalence", labels = percent, 
          breaks = seq(0, 1, 0.05), limits = c(0, lims[2])) +
        geom_text(aes(x = scenario, y = mean * 1.01 + 0.01,
          label = scales::percent(mean, accuracy = 0.1) ),
          colour = "grey20") +
        scale_colour_manual(values = palette_periods[c(3,4,5)]) +
        scale_fill_manual(values = palette_periods[c(3,4,5)])      
      
            
      # combined
      plotgam <- ggarrange(plot1gam, plot2gam, plot3gam, ncol = 3, nrow = 1, 
        align = "h", widths = c(1.5, 1, 6) )
      plotsam <- ggarrange(plot1sam, plot2sam, plot3sam, ncol = 3, nrow = 1,
        align = "h", widths = c(1.5, 1, 6) )
      ggarrange(NULL, plotgam, plotsam, nrow = 3, vjust = 0, hjust = 0,
        heights = c(0.05, 1, 1),
        labels = c("", "Global Acute Malnutrition", "Severe Acute Malnutrition"),
        font.label = list(size = 10))
          
    # Save
    ggsave(paste(dir_path, "outputs/", "gam_sam_kids.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)       
