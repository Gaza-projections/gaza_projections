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

    # Add other categories that feature in other scenarios
    colnames(out_main)[! colnames(out_cf) %in% colnames(out_main)]
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
    
    # Aggregate across all runs
    stats <- c("mean", "lci", "uci")
    
    tab1 <- aggregate(list(d_all = out$d_all), 
      by = out[, c("scenario", "period", "age", "gender")],
      FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))})
    tab1 <- data.frame(tab1[, c("scenario", "period", "age", "gender")], 
      tab1$d_all)
    colnames(tab1) <- c("scenario", "period", "age", "gender", stats)

    # Save
    write.csv(tab1, paste(dir_path, "outputs/out_for_aggregation.csv", 
      sep = "/"), row.names = FALSE)
    

#...............................................................................  
### Tabulating the estimates
#...............................................................................

    
        
  #...................................
  ## Tabulate injury deaths by scenario, age and type

    # Aggregate by scenario, age and type
    cols <- c("all", "counted", "uncounted")
    stats <- c("mean", "median", "lci", "uci")
    x1 <- aggregate(out_main[, cols], 
      by = out_main[, c("scenario", "age", "gender")], 
      FUN = function(x) {return(c(mean(x), 
        quantile(x, c(0.5, 0.025, 0.975))) )} )
    tab1 <- data.frame(x1[, c("scenario", "age", "gender")], x1$all, x1$counted,
      x1$uncounted)
    colnames(tab1) <- c("scenario", "age", "gender", 
      paste("all", stats, sep = "_"),
      paste("counted", stats, sep = "_"), paste("uncounted", stats, sep = "_") )
    
    # Add totals by scenario and gender
    x1 <- aggregate(out_main[, cols], 
      by = out_main[, c("run", "scenario", "gender")], FUN = sum )
    x2 <- aggregate(x1[, cols], by = x1[, c("scenario", "gender")], 
      FUN = function(x) {return(c(mean(x), 
        quantile(x, c(0.5, 0.025, 0.975))) )} )
    x2 <- data.frame(x2[, c("scenario", "gender")], x2$all, x2$counted,
      x2$uncounted)
    x2$age <- "all"
    colnames(x2) <- c("scenario", "gender", paste("all", stats, sep = "_"),
     paste("counted", stats, sep = "_"), paste("uncounted", stats, sep = "_"),
     "age")
    x2 <- x2[, colnames(tab1)]
    tab1 <- rbind(tab1, x2)
    
    # Add totals by scenario
    x1 <- aggregate(out_main[, cols], by = out_main[, c("run", "scenario")], 
      FUN = sum )
    x2 <- aggregate(x1[, cols], by = list(scenario = x1$scenario), 
      FUN = function(x) {return(c(mean(x), 
        quantile(x, c(0.5, 0.025, 0.975))) )} )
    x2 <- data.frame(x2[, "scenario"], x2$all, x2$counted,
      x2$uncounted)
    x2$age <- "all"
    x2$gender <- "all"
    colnames(x2) <- c("scenario", paste("all", stats, sep = "_"),
     paste("counted", stats, sep = "_"), paste("uncounted", stats, sep = "_"),
     "age", "gender")
    x2 <- x2[, colnames(tab1)]
    tab1 <- rbind(tab1, x2)
    
    # Write raw out_mainput
    write.csv(tab1, paste(dir_path, "outputs/out_main_tab_all.csv", 
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
      
    # output
    x <- rbind(x1, x2, x3)
    x <- data.frame(x$scenario, x$category, x$d_all)
    colnames(x) <- c("scenario","category", "mean", "lci", "uci")
    write.csv(x, paste(dir_path, "outputs/out_mnh_injuries.csv", 
      sep = "/"), row.names = FALSE)    

    

                
#...............................................................................  
### ENDS
#...............................................................................      
      