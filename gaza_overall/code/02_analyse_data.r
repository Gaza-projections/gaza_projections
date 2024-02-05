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
    df$dr_lci <- df$mean * 1000 * 365 / (df$lci * 30.41 * 3)
    df$dr_uci <- df$mean * 1000 * 365 / (df$uci * 30.41 * 3)

    
#...............................................................................  
### Visualising the estimates
#...............................................................................

  #...................................
  ## Tabulate baseline and excess deaths by scenario, subperiod and cause
    
        
    
  #...................................
  ## Plot age-specific death rate by scenario and subperiod

    # Prepare data    
    df_plot <- aggregate(df[, c("dr_mean", "dr_lci", "dr_uci")],
      by = df[, c("scenario", "age", "subperiod")], FUN = sum)

    # Plot
    ggplot(data = df_plot, aes(x = age, y = dr_mean, colour = scenario)) +
      geom_line() +
      scale_y_continuous("annualised age-specific death rate (per 1000)")
      
    
  #...................................
  ## Plot crude death rate by scenario and subperiod
    
    
    
    
    
#...............................................................................
### ENDS
#...............................................................................

