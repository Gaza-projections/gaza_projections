#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ------- R SCRIPT TO DEFINE SPECIFIC FUNCTIONS NEEDED FOR ANALYSIS  ------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 



#...............................................................................   
### Function to implement one run of a Susceptible-Infectious-Recovered model
    # during the projection period, for all ages, an infection and a scenario,
    # tracking cumulative infection attack rate and deaths over period
    # uses Epiverse-TRACE 'epidemics' package
#...............................................................................

f_seir <- function(disease = "diphtheria", scenario = "escalation",
  initial_conditions = initial_conditions, timeline_ui = timeline_ui,
  contact_matrix = contact_matrix, demography_vector = demography_vector,
  sim_pars_u = sim_pars_u) {

  #...................................      
  ## Decide whether an outbreak will occur during the projection period
    
    # Sample from a binomial distribution of probability of an outbreak
    y <- rbinom(1, 1, prob = mean(timeline_ui$pu) )
  
    # If 0, no outbreak: end function here
    if (y == 0) {
      
      # Return output with 0 infections, cases and deaths
      out[, c("infections", "symptomatic", "deaths")] <- 0
      return(out)
###########NEED TO MODIFY  
    }
  
# If p = 1, an outbreak occurs; function continues
if (y == 1) {
      
  #...................................      
  ## Select random starting date for epidemic, and compute resulting periods
    # starting time is not dependent on random run variable: it's totally random
      
    # Sample random starting day within the projection period
      # sample random starting day
      time_epid_start <- unique(timeline_ui[which(timeline_ui$time == 
        as.integer(runif(1, 0, 1) * max(timeline_ui$time))), "time"])
      
      # update in timeline
      timeline_ui$time_epid <- timeline_ui$time - time_epid_start + 1
      timeline_ui[which(timeline_ui$time_epid < 0), "time_epid"] <- 0
      
      # accordingly, the starting month out of 1-6 is...
      rmonth <- unique(timeline_ui[which(timeline_ui$time_epid == 1), "month"])
    
      #...and the number of days to simulate in subperiods 1 and 2 are
      days_subperiod1 <- max(0, max(timeline_ui[which(timeline_ui$subperiod == 
          "subperiod1"), "time"]) - time_epid_start)
      days_subperiod2 <- min(max(timeline_ui$time) - 
          max(timeline_ui[which(timeline_ui$subperiod== "subperiod1"), "time"]), 
          max(timeline_ui$time) - time_epid_start)

  ## Set starting compartment sizes, based on susceptibility model
    
    # Get starting susceptibility by age
    x <- timeline_ui[which(timeline_ui$time)]
      
    # Set starting compartment sizes
    initial_conditions$I <- 1e-6 # 1 per million per age group infected
    initial_conditions$S <- unlist(si_list[[disease]][[scenario]][rmonth, ages]- 
      initial_conditions$I)
    initial_conditions$R <- 1 - initial_conditions$I - initial_conditions$S
    
    # Prepare the population to model as affected by the epidemic
    model_population <- epidemics::population(
      name = "Gaza",
      contact_matrix = contact_matrix,
      demography_vector = demography_vector,
      initial_conditions = as.matrix(initial_conditions)
    )
        
  #...................................      
  ## Simulate epidemic over subperiod 1
    # (but only if the epidemic starts within sub-period 1)
  if (time_epid_start <= time_periods["end_subperiod1"]) {

    # Recognise parameters for the disease, specific to the subperiod
      # basic reproduction number
      r0 <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod1"), 
        "r0"])

      # infectiousness period
      tau <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod1"), 
        "tau"])
      
      # pre-infectiousness period
      pre_tau <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod1"), 
        "pre_tau"])
      
    # Run SEIR model to end of sub-period 1
    run_subperiod1 <- epidemics::model_default_cpp(
      population = model_population,
      transmissibility = r0 / tau,
      infectiousness_rate = 1 / pre_tau,
      recovery_rate = 1 / tau,
      time_end = days_subperiod1,
      increment = 1
    )
    
    # Update initial conditions after sub-period 1
    x <- subset(run_subperiod1, time == max(run_subperiod1$time))
    initial_conditions$S <- x[which(x$compartment == "susceptible"), "value"]
    initial_conditions$E <- x[which(x$compartment == "exposed"), "value"]
    initial_conditions$I <- x[which(x$compartment == "infectious"), "value"]
    initial_conditions$R <- x[which(x$compartment == "recovered"), "value"]
    initial_conditions$V <- x[which(x$compartment == "vaccinated"), "value"]
  }

  #...................................      
  ## Simulate epidemic over subperiod 2

    # Recognise parameters for the disease, specific to the subperiod
      # basic reproduction number
      r0 <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod2"), 
        "r0"])

      # infectiousness period
      tau <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod2"), 
        "tau"])
      
      # pre-infectiousness period
      pre_tau <- mean(timeline_ui[which(timeline_ui$subperiod == "subperiod2"), 
        "pre_tau"])
      
    # Run epidemic till the end of sub-period 2
    run_subperiod2 <- epidemics::model_default_cpp(
      population = model_population,
      transmissibility = r0 / tau,
      infectiousness_rate = 1 / pre_tau,
      recovery_rate = 1 / tau,
      time_end = days_subperiod2,
      increment = 1
    )
      
  #...................................      
  ## Assemble output into a timeline of susceptibles and new infections

    # Collect subperiod 1
    run_full <- subset(run_subperiod1, compartment == "susceptible")
    run_full$subperiod <- "subperiod1"
    run_full <- merge(run_full, new_infections(run_subperiod1),
      by = c("time", "demography_group"), all.x = TRUE)
    
    # Collect and add subperiod 2
    x <- subset(run_subperiod2, compartment == "susceptible")
    x <- merge(x, new_infections(run_subperiod2),
      by = c("time", "demography_group"), all.x = TRUE)
    x$time <- x$time + days_subperiod1 + 1
    x$subperiod <- "subperiod2"
    run_full <- rbind(run_full, x)
    
    # Add to timeline
    run_full <- merge(run_full, data.frame(demography_group =
      names(demography_vector), pop = demography_vector), 
      by = "demography_group", all.x = TRUE)
    run_full$time <- run_full$time + 1
    run_full$time_epid <- run_full$time
    run_full <- merge(run_full, timeline_ui[, c("time_epid", "month")], 
      by = "time_epid", all.x = TRUE)    
#####HERE    
  #...................................      
  ## Compute symptomatic cases and deaths
    
    # Symptomatic cases
    timeline_ui$symptomatic <- timeline_ui$new_infections * timeline_ui$pd
    
    # Compute deaths, adjusted for changing susceptibility to infection/disease
    timeline_ui$deaths <- timeline_ui$symptomatic * timeline_ui$cfr * 
      timeline_ui$sd / (timeline_ui$susceptibles / timeline_ui$pop)
        
  #...................................      
  ## Collect and return results
    # Aggregate by sub-period
    out <- aggregate(timeline_ui[, c("new_infections", "symptomatic", "deaths")],
      by = timeline_ui[, c("subperiod", "age")], FUN = sum, na.rm = TRUE)
    
    # Return results      
    return(out)
  }
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