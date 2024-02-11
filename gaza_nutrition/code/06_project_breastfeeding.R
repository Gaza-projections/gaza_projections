#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO PROJECT EXCLUSIVE BREASTFEEDING PREVALENCE  -------- ##
#...............................................................................

                          # Francesco Checchi, Zeina Jamaluddine (January 2024)


#...............................................................................  
### Approximating projected breastfeeding prevalence during projection period
#...............................................................................

  #...................................      
  ## Preliminary steps

    # Compute median and 50% percentile interval of relative risks elsewhere
    x <- quantile(bf$rr, c(0.75, 0.50, 0.25) )

    # Initialise output
    ebf <- data.frame(scenario = scenarios, median = NA, min = NA, max = NA)

  
  #...................................      
  ## Compute projections for Gaza
    
    # Apply regional patterns to baseline in Gaza
    x <- exclusive_bf * x
  
    # Add +-5% uncertainty
    ebf$median <- round(x/0.05)*0.05
    ebf$min <- ebf$median - 0.05
    ebf$max <- ebf$median + 0.05
    
    # Output
    write.csv(ebf, paste(dir_path, "outputs/","out_exclusive_bf.csv", 
      sep=""), row.names = FALSE)
    
#...............................................................................  
### ENDS
#...............................................................................