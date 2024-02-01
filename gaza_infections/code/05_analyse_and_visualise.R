#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO ANALYSE AND VISUALISE SIMULATIONS FOR INFECTIONS MODEL  --- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Analysing simulations for epidemic infections
#...............................................................................

  #...................................      
  ## Format output as needed
    
    # Format subperiod names
    out_epid$subperiod <- sapply(out_epid$subperiod, 
      function(x) {names(subperiods[which(subperiods == x)])} )

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
      varying = rev(scenarios) )
    tab2 <- tab2[order(tab2$disease, tab2$subperiod), ]
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
      varying = rev(scenarios) )
    tab3 <- tab3[order(tab3$age, tab3$subperiod), ]
    write.csv(tab3, paste(dir_path, "outputs/out_tab_epid_age_pretty.csv", 
      sep = "/"), row.names = FALSE)


#...............................................................................   
### Analysing simulations for stable-transmission infections
#...............................................................................



#...............................................................................   
### ENDS
#...............................................................................
