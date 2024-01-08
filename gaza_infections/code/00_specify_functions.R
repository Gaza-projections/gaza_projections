#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ------- R SCRIPT TO DEFINE SPECIFIC FUNCTIONS NEEDED FOR ANALYSIS  ------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


# #...............................................................................   
# ### Function to compute deaths due to each epidemic-prone infection during
#     # the projection period, for all age groups: one bootstrap run
# #...............................................................................
# 
# f_d_epid <- function(pars_i = pars_i, diseases_in = diseases_in) {
# 
#   #.........................................
#   ## Implement SIR model
#   
#   
#   
#   
#     
# }


#...............................................................................   
### Function to implement one run of a Susceptible-Infectious-Recovered model
    # during the projection period, for all ages, an infection and a scenario,
    # tracking cumulative infection attack rate and deaths over period
#...............................................................................

f_seir <- function(disease = "diphtheria", scenario = "central",
  initial_conditions = initial_conditions, timeline = timeline,
  contact_matrix = contact_matrix, demography_vector = demography_vector,
  see = see, epidemic_pars = epidemic_pars) {

  #...................................      
  ## Pick random starting date for epidemic
  
    # Select random starting day within the projection period
      # select random starting day
      epid_start <- sample(timeline$time, 1)
      
      # update in timeline
      timeline$time_epid <- timeline$time - epid_start
      timeline[which(timeline$time_epid < 0), "time_epid"] <- 0
      
      # accordingly, the starting month out of 1-6 is...
      rmonth <- lubridate::month(timeline[which(timeline$time == epid_start), 
        "date"]) - lubridate::month(timeline[which(timeline$time == 1), 
        "date"])
    
      #...and the number of days to simulate in subperiods 1 and 2 are
      time_end_subperiod1 <- 0
      if (epid_start <= time_periods["end_subperiod1"]) 
        {time_end_subperiod1 <- timeline[which(timeline$time == 
        time_periods["end_subperiod1"]), "time_epid"] }
      time_end_subperiod2 <- max(timeline$time) - max(epid_start,
        time_periods["end_subperiod1"] + 1)

      
  #...................................      
  ## Select random subperiod-specific value of R0 from elicited EDF

    # Create dataframe to hold simulated values
    r0_sim <- data.frame(subperiod = c("subperiod1", "subperiod2"), r0 = NA)
    
    # For each sub-period...
    for (i in c("subperiod1", "subperiod2")) { 
      
      # sample an elicited value of R0
      x <- see[[disease]][[scenario]][[i]][["r0"]]
      r0_sim[which(r0_sim$subperiod == i), "r0"] <- x#################
    }  
# TO DO - NEED TO SAMPLE SEE, with same quantile for both subperiods

    
  #...................................      
  ## Set random age- and subperiod-specific values of prop. of severe cases
      # from elicited EDF
      # assume single sampled value applies to the most vulnerable age group
      # and scale all the others accordingly
      
    # Create dataframe to hold simulated values
    p_sev_sim <- expand.grid(rownames(initial_conditions), 
      c("subperiod1", "subperiod2"))
    colnames(p_sev_sim) <- c("age_group", "subperiod")
    p_sev_sim$p_sev <- NA
      
    # Get average value of proportion of severe cases, by age
    p_sev_age <- unlist(epidemic_pars[which(epidemic_pars$disease == disease & 
        epidemic_pars$parameter == "p_sev"), 
      grep("value_a", colnames(epidemic_pars))])
   
    # For each sub-period...
    for (i in c("subperiod1", "subperiod2")) {   
      # sample an elicited value of proportion of severe cases
      x <- see[[disease]][[scenario]][[i]][["p_sev"]] #################### 
# TO DO - NEED TO SAMPLE SEE, with same quantile for both subperiods
        
      # use this sampled value to scale all the age-specific values  
      p_sev_sim[which(p_sev_sim$subperiod == i), "p_sev"] <- 
        x *  p_sev_age / max(p_sev_age)
    }
      
    # Set random age- and subperiod-specific values of case-fatality ratio
      # from elicited EDF
      # assume value applies to the most vulnerable age group
      # and scale all the others accordingly
      
      # create dataframe to hold simulated values
      cfr_sim <- expand.grid(rownames(initial_conditions), 
        c("subperiod1", "subperiod2"))
      colnames(cfr_sim) <- c("age_group", "subperiod")
      cfr_sim$cfr <- NA
      
      # get average value of CFR, by age
      cfr_age <- unlist(epidemic_pars[which(epidemic_pars$disease == disease & 
          epidemic_pars$parameter == "cfr"), 
        grep("value_a", colnames(epidemic_pars))])
      
    for (i in c("subperiod1", "subperiod2")) {   
      # sample an elicited value of proportion of severe cases
      x <- see[[disease]][[scenario]][[i]][["cfr"]] #################### 
# TO DO - NEED TO SAMPLE SEE, with same quantile for both subperiods
        
      # use this sampled value to scale all the age-specific values  
      cfr_sim[which(cfr_sim$subperiod == i), "cfr"] <- 
        x *  cfr_age / max(cfr_age)
    }
      
  #...................................      
  ## Prepare other inputs
    
    # Set periods of pre-infectiousness and infectiousness
    pre_tau <- epidemic_pars[which(epidemic_pars$disease == disease & 
    epidemic_pars$parameter == "pre_tau"), "value_gen"]
    tau <- epidemic_pars[which(epidemic_pars$disease == disease & 
        epidemic_pars$parameter == "tau"), "value_gen"]
    
    # Set starting compartment sizes
    initial_conditions$I <- 1e-6 # 1 per million per age group infected
    initial_conditions$S <- unlist(susc[[disease]][[scenario]][rmonth, ages] - 
      initial_conditions$I)
    initial_conditions$R <- 1 - initial_conditions$I - initial_conditions$S
    
    # Set starting relative proportions of susceptibles / not susceptibles to
      # severe disease
    initial_vd$S
#######
        
    # Prepare the population to model as affected by the epidemic
    model_population <- epidemics::population(
      name = "Gaza",
      contact_matrix = contact_matrix,
      demography_vector = demography_vector,
      initial_conditions = as.matrix(initial_conditions)
    )

  #...................................      
  ## Simulate epidemic over two sub-periods and collect cumulative infections
    
    # Run epidemic till the end of sub-period 1, with sub-period 1 R0 value
      # but only if the epidemic starts within sub-period 1
    if (epid_start <= time_periods["end_subperiod1"]) {
      
      # run to end of sub-period 1
      run_subperiod1 <- epidemics::model_default_cpp(
        population = model_population,
        transmissibility = r0_sim[1, "r0"] / tau,
        infectiousness_rate = 1 / pre_tau,
        recovery_rate = 1 / tau,
        time_end = time_end_subperiod1,
        increment = 1
      )
      
      # update initial conditions after sub-period 1
      x <- subset(run1, time == max(run1$time))
      initial_conditions$S <- x[which(x$compartment == "susceptible"), "value"]
      initial_conditions$E <- x[which(x$compartment == "exposed"), "value"]
      initial_conditions$I <- x[which(x$compartment == "infectious"), "value"]
      initial_conditions$R <- x[which(x$compartment == "recovered"), "value"]
      initial_conditions$V <- x[which(x$compartment == "vaccinated"), "value"]
    }
    
    # Run epidemic till the end of sub-period 2, with sub-period 2 R0 value
    run_subperiod2 <- epidemics::model_default_cpp(
      population = model_population,
      transmissibility = r0_sim[2, "r0"] / tau,
      infectiousness_rate = 1 / pre_tau,
      recovery_rate = 1 / tau,
      time_end = time_end_subperiod2,
      increment = 1
    )
      
    # Collect cumulative infections by sub-period
    out <- data.frame()
    for (i in c("subperiod1", "subperiod2")) {
      # sum infections by sub-period
      x <- new_infections(get(paste("run", i, sep = "_")))
      x <- aggregate(x$new_infections, by = list(x$demography_group), FUN = sum)
      colnames(x) <- c("age_group", "infected")            
      x$subperiod <- i
      out <- rbind(out, x)
    }
     
  #...................................      
  ## Compute cumulative severe cases and deaths, by sub-period
    
    # Add proportion of severe cases and CFR values
    out <- merge(out, p_sev_sim, by = c("age_group", "subperiod"), all.x = TRUE)
    out <- merge(out, cfr_sim, by = c("age_group", "subperiod"), all.x = TRUE)
    
    # Multiply to obtain deaths
    out$deaths <- out$infected * out$p_sev * out$cfr

### need to correct for protection against disease    
        
    # Return result
    return(out)
}



#...............................................................................   
### Function to distribute contact matrix into finer target age strata
    # complies with 'socialmixr', 'epidemics' packages
#...............................................................................

f_target <- function(input_matrix = contact_matrix, ages_wanted = ages) {
  
  #...................................      
  ## Generate the desired target contact matrix (all but its values)
    # Figure out the lower bounds of each age group, in years
    ages_lower <- c()
    for (i in 1:length(ages) ) {
      if (grepl("mo", ages[i])) {ages_lower[i] <- 
        parse_number(unlist(strsplit(ages[i], split = "to"))[1]) / 12}
      if (grepl("yo", ages[i])) {ages_lower[i] <- 
        parse_number(unlist(strsplit(ages[i], split = "to"))[1])}
    }
    
    # Set row names for matrix
    target_names <- levels(cut(0:100, c(ages_lower, 100), include.lowest = TRUE, 
      right = FALSE))
    target_names[length(target_names)] <- 
      paste(ages_lower[length(ages_lower)], "+", sep = "")
    
    # Generate target contact matrix
    target_matrix <- matrix(data = NA, nrow = length(ages_lower),
      ncol = length(ages_lower), 
      dimnames = list("contact.age.group" = target_names,
      target_names) )
    
    # Dataframe of target age groups and labels
    target <- data.frame(target_min = ages_lower, 
      target_max = c(ages_lower[2:length(ages_lower)], 100), 
      target_label = rownames(target_matrix))
          
  #...................................      
  ## Map target age groups to input age groups
    # Figure out lower bounds of input age groups
    input_min <- sapply(do.call(rbind.data.frame,
      strsplit(rownames(input_matrix), split = ",") ), parse_number)
    input_min <- as.vector(input_min[, 1])

    # Figure out upper bounds of input age groups
    input_max <- sapply(do.call(rbind.data.frame,
      strsplit(rownames(input_matrix), split = ",") ), parse_number)
    input_max <- as.vector(input_max[, 2])
    input_max[length(input_max)] <- 100
    
    # Dataframe of input age groups and labels
    input <- data.frame(input_min = input_min, input_max = input_max, 
      input_label = rownames(input_matrix))
    
    # Map input to target age groups
    x <- findInterval(ages_lower, input_min)
    target$input_min <- input_min[x]
    target <- merge(target, input, by = "input_min", all.x = TRUE)
    
  #...................................      
  ## Split input matrix into desired target contact matrix      
    # Compute fraction that target age groups represent, relative to input  
    target$fraction <- (target$target_max - target$target_min) / 
      (target$input_max - target$input_min)

    # Generate target matrix of fractions for each cell
    fraction_matrix <- target$fraction %*% t(target$fraction)
    rownames(fraction_matrix) <- rownames(target_matrix)
    colnames(fraction_matrix) <- rownames(target_matrix)
    
    # For each target matrix cell, first populate with input matrix values...    
    for (i in rownames(target_matrix)) {
      for (j in colnames(target_matrix)) {
        target_matrix[i, j] <- input_matrix[
          target[which(target$target_label == i), "input_label"],
          target[which(target$target_label == j), "input_label"]
        ]
      }
    }
  
    #...then multiply each value by those in the fraction matrix
    target_matrix <- target_matrix * fraction_matrix
    
    # Output target matrix
    return(target_matrix)
} 



  


  
#...............................................................................
### ENDS
#...............................................................................