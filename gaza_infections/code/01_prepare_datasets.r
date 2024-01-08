#...............................................................................
### +++++++++ RECONSTRUCTING POPULATION DENOMINATORS IN SOMALIA ++++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO PREPARE REQUIRED DEMOGRAPHIC AND DISPLACEMENT DATASETS  -- ##
#...............................................................................

                          # LSHTM, SIMAD University (August 2023)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Preparing administrative time series
#...............................................................................

  #.........................................
  ## Prepare administrative units dataset

     # Define strata of Somalia within which to estimate parameters
      admin2$stratum <- admin2$admin0
      admin2[which(admin2$region %in% c("Bakool", "Banadir", "Bay", 
        "Lower Shabelle", "Middle Shabelle")), "stratum"] <- "South-Central 1"
      admin2[which(admin2$region %in% c("Galgaduud", "Gedo", "Hiraan", 
        "Lower Juba", "Middle Juba")), "stratum"] <- "South-Central 2"
      table(admin2$stratum)
      strata <- sort(unique(admin2$stratum))

  #.........................................
  ## Generate time series of district-year-months
  
    # Generate time series
    ts <- f_gen_ts()

    
#...............................................................................   
### Preparing displacement datasets
#...............................................................................
    
  #.........................................
  ## Prepare IDP PRMN dataset
  
    # Select and rename columns of interest
    cols_use <- c("cdistrict", "pdistrict", "creason", "yr", "monthnum",
      "tpeople")
    prmn <- prmn[, cols_use]    
    colnames(prmn) <- c("district_arr", "district_dep", "reason", "y", "m", 
      "n_idp")    
    str(prmn)
    
    # Rectify district nomenclature
    unique(prmn$district_dep)[! unique(prmn$district_dep) %in% 
      unique(admin2$district)]
    unique(prmn$district_arr)[! unique(prmn$district_arr) %in% 
      unique(admin2$district)]
    corrections <- data.frame(
      "wrong" = c("Baidoa", "Kismayo", "Badhan", "Dhahar", "Lasqoray"), 
      "right" = c("Baydhaba", "Kismaayo", "Laasqoray", "Laasqoray", "Laasqoray")
    )
    for (i in c("dep", "arr")) {
      for (j in 1:nrow(corrections)) {
        prmn[which(prmn[, paste("district", i, sep = "_")] == 
          corrections[j, "wrong"]), 
          paste("district", i, sep = "_")] <- corrections[j, "right"]
      }
    }

    # Check that all dates are valid
    table(prmn$y)
    table(prmn$m)
      
    # Eliminate observations...
      # with missing districts of origin or arrival
      table(prmn$district_dep == "" | prmn$district_arr == "" )
      prmn <- subset(prmn, district_dep != "" & district_arr != "")
        
      # with missing years or months
      table(is.na(prmn$m) | is.na(prmn$y) ) # none
      prmn <- subset(prmn, ! is.na(prmn$m) & ! is.na(prmn$y))
        
      # with missing number of IDPs moving
      table(is.na(prmn$n_idp)) # none
      prmn <- subset(prmn, ! is.na(prmn$n_idp))
    
    # Aggregate to make sure there are only unique combinations of 
      # district(depart) - district(arr) - reason - time
    prmn <- aggregate(prmn$n_idp, by = prmn[, colnames(prmn) != "n_idp"], 
      FUN = sum, na.rm = TRUE)
    colnames(prmn)[colnames(prmn) == "x"] <- "n_idp"

    # Rename reason categories
    corrections <- data.frame("old" = names(table(prmn$reason)), 
      "new" = c("insecurity", "drought", "flooding", "other"))
    for (i in 1:nrow(corrections)) {
      prmn[which(prmn$reason == corrections[i, "old"]), "reason"] <- 
        corrections[i, "new"]
    }
    table(prmn$reason)
                
    # Reshape wide (reasons for displacement as columns)
    prmn <- dcast(prmn, district_dep + district_arr + y + m ~ reason, 
      value.var = "n_idp")
    
    # Make sure all district of origin - district of arrival - time 
      # combinations are featured
    x <- expand.grid(admin2$district, admin2$district, sort(unique(ts$tm)) )
    colnames(x) <- c("district_dep", "district_arr", "tm")
    x <- merge(x, unique(ts[, c("y", "m", "tm")]), by = "tm", all.x = TRUE)
    prmn <- merge(x, prmn, by = c("district_dep", "district_arr", "y", "m"),
      all.x = TRUE)
    rm(x)
    
    # Total N of IDPs
    prmn[, corrections$new] <- na.replace(prmn[, corrections$new], 0)
    prmn$n_idp <- rowSums(prmn[, corrections$new])

    # Sort
    prmn$district_dep <- as.character(prmn$district_dep)
    prmn$district_arr <- as.character(prmn$district_arr)
    prmn <- prmn[order(prmn$district_dep, prmn$district_arr), ]
    

  #.........................................
  ## Prepare refugees dataset
  
    # Aggregate by district, year and month     
    refugees <- aggregate(refugees$net_ref, 
      by = refugees[, c("district", "y", "m")], FUN = sum, na.rm = TRUE)
    
    # Make sure all district-year-month combinations are featured
    refugees <- merge(ts, refugees, by = c("district", "y", "m"), all.x = TRUE)
    colnames(refugees)[colnames(refugees) == "x"] <- "net_ref"    
    refugees <- na.replace(refugees, 0)  
      # wide version for reconstruction
      refugees_w <- dcast(refugees[, c("district", "tm", "net_ref")], 
        district ~ tm, value.var = "net_ref")   

  #.........................................
  ## Prepare DTM fitting data
      
    # Compute tm points
    dtm$m <- lubridate::month(dtm$date)
    dtm$y <- lubridate::year(dtm$date)
    dtm <- merge(dtm, unique(ts[, c("tm", "m", "y")]), by = c("m", "y"), 
      all.x = TRUE)
    
    # Rename and select variables
    dtm$n_idp_obs <- dtm$n_idp
    dtm$n_ret_obs <- dtm$n_ret
    dtm <- dtm[, c("district", "tm", "n_idp_obs", "n_ret_obs")]
    
    # Compute fitting target variables
    dtm$prop_ret_obs <- dtm$n_ret_obs / (dtm$n_idp_obs + dtm$n_ret_obs)
    dtm$prop_ret_obs <- ifelse(dtm$prop_ret_obs == 0, 0.001, dtm$prop_ret_obs) 
      # avoid zero probabilities (-infinite log-likelihoods)
    dtm$prop_ret_obs <- ifelse(dtm$prop_ret_obs == 1, 0.999, dtm$prop_ret_obs) 
      # avoid 100% probabilities (infinite log-likelihoods)
    dtm$n_ret_obs <- ifelse(dtm$n_ret_obs == 0, 1, dtm$n_ret_obs) 
      # avoid -infinite log-likelihoods
    dtm$n_idp_obs <- ifelse(dtm$n_idp_obs == 0, 1, dtm$n_idp_obs) 
      # avoid -infinite log-likelihoods

    # Compute weights to be used for RMSE and log-likelihood
    dtm$wt_rmse <- (dtm$n_idp_obs + dtm$n_ret_obs) / 
      (sum(dtm$n_idp_obs) + sum(dtm$n_ret_obs) )
    dtm$wt_ll <- (dtm$n_idp_obs + dtm$n_ret_obs) / 
      mean(dtm$n_idp_obs + dtm$n_ret_obs)
    
    # Sort
    dtm <- dtm[order(dtm$district, dtm$tm), ]
          

#...............................................................................   
### Preparing base population source datasets
#...............................................................................
        
  #.........................................
  ## Prepare population source metadata

    # Identify population sources and relevant metadata
    pop_sources <- data_tab[which(grepl("pop_", data_tab$worksheet) & 
      data_tab$used_in_analysis == "Y"), 
      c("worksheet", "quality_score", "label", "m", "y")]
    
    # Add time point
    pop_sources <- merge(pop_sources, unique(ts[, c("y", "m", "tm")]), 
      by = c("m", "y"), all.x = TRUE)
    
    # Normalise cumulative quality score to [0, 1]
      # (for random selection of one of the sources)
    pop_sources$quality_prob <- cumsum(pop_sources$quality_score) / 
      sum(pop_sources$quality_score)

    # Population source labels for plotting
    pop_labels <- pop_sources$label
    names(pop_labels) <- pop_sources$worksheet
    
    
  #.........................................
  ## Prepare reconstruction time series for each population source
      
    # Generic wide version of time series = empty reconstruction dataset
    ts_w <- ts
    ts_w$nacol <- NA
    ts_w <- dcast(ts_w, district ~ tm, value.var = "nacol")
    ts_w <- ts_w[order(ts_w$district), ]
        
    # Prepare one reconstruction dataset for each population source, 
      # with known population estimates 
    for (i in 1:nrow(pop_sources) ) {
      
      # get population source dataset
      pop_i <- get(pop_sources[i, "worksheet"])
      
      # make sure all districts are featured
      pop_i <- merge(admin2, 
        pop_i, by = "district", all.x = TRUE)[, c("district", "pop")]
      
      # sort
      pop_i <- pop_i[order(pop_i$district), ]
      
      # initialise and populate empty reconstruction dataset
      x <- ts_w
      x[, as.character(pop_sources[i, "tm"])] <- pop_i$pop 
        # only known time point with data
      assign(paste(pop_sources[i, "worksheet"], "_w", sep = ""), x)
    }
    
    # Compare the population sources
      # merge all sources into one
      pop_all <- get(pop_sources[1, "worksheet"])
      colnames(pop_all) <- c("district", pop_sources[1, "worksheet"])
      for (i in 2:nrow(pop_sources)) {
        x <- get(pop_sources[i, "worksheet"])
        colnames(x) <- c("district", pop_sources[i, "worksheet"])
        pop_all <- merge(pop_all, x, by = "district", all = TRUE)
      }  

      # plot all together
      pop_all <- melt(pop_all, value.name = "pop")
      plot <- ggplot(data = pop_all, aes(x = pop, y = district, 
        colour = variable, shape = variable)) +
        geom_point(alpha = 1, size = 2, stroke = 1) +
        scale_colour_manual("source:", values = palette_cb[c(4,6,7,8)],
          labels = pop_sources$label) +
        scale_shape_manual(values = c(21, 22, 23, 24), guide = "none") +
        scale_x_continuous(name = "population", trans = "log", 
          breaks = c(10000, 20000, 50000, 100000, 200000, 500000, 1000000,
            2000000), labels = label_number(big.mark = ",")) +
        theme_bw() +
        theme(legend.position = "top") +
        guides(colour = guide_legend(override.aes = 
          list(shape = c(21, 22, 23, 24))) )
      ggsave(paste(dir_path, "/output/som_out_pop_sources_compared.png", 
        sep =""), dpi = "print", units = "cm", height = 30, width = 20)


#...............................................................................   
### Preparing SMART survey household observations (for < 5y population model)
#...............................................................................
      
  #.........................................
  ## Keep only needed variables
      
  hh_obs <- hh_obs[, c("region", "district", "year", "n_u5", "n",
    "qualityScore")]

    
#...............................................................................
### ENDS
#...............................................................................

