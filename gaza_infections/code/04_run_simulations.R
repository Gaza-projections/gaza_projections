#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO RUN AND ANALYSE SIMULATIONS FOR INFECTIONS MODEL  ------ ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Initialising objects needed for the simulation
#...............................................................................

  #...................................      
  ## Initialise other objects


#...............................................................................   
### Running epidemic (SEIR model simulations)
#...............................................................................

# (run loop starts here)    
for (run_i in 1:max(runs$run)) {

  #...................................      
  ## Preparatory steps

    # Update progress bar
    setTxtProgressBar(pb, run_i)
  
  #...................................      
  ## Set values of parameters for each scenario and epidemic-prone disease
  for (i in scenarios) {
    
    # Get parameter values for this run and scenario
    sim_pars <- sim[[run_i]][[i]]
    
    # For each disease...
    for (u in diseases_epid) {

      # Start fresh timeline
      timeline_ui <- timeline
      
      # Get parameter values for this run, scenario and disease
      
          
            
    }
  }
  
  

  } # (close j - scenario loop)   

} # (close i - run loop)
close(pb)    
      
      
