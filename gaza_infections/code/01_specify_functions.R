#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## ------- R SCRIPT TO DEFINE SPECIFIC FUNCTIONS NEEDED FOR ANALYSIS  ------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................   
### Function to calculate the calibration and information scores for each 
  # elicitation expert, leading to combined scores and weights; based on 
  # https://ocw.tudelft.nl/course-readings/2-3-2-empirical-probability-vectors/
#...............................................................................

f_score <- function(see_f = see) {

  #...................................      
  ## Preparatory steps
    
    # Restrict data to calibration questions only
    see_f <- subset(see_f, parameter == "cal")

  #...................................      
  ## Compute each expert's calibration score
    
    # Identify where the 'right' answer falls relative to each set of estimates
    see_f$s <- apply(see_f, 1, function (x) 
      {findInterval(x[["answer"]], 
        as.numeric(x[c("value10", "value50", "value90")]), 
        rightmost.closed = TRUE) + 1 }) 

    # Tabulate proportion of calibration answers falling within each quartile,
      # by expert (= 'empirical' probability vector)
    prob_s <- table(see_f[, c("expert", "s")])
    prob_s <- prob_s / rowSums(prob_s)
    prob_s <- data.frame(prob_s)
    colnames(prob_s) <- c("expert", "quartile", "s")
    
    # Figure out theoretical probability vector and add it to dataframe
    x <- length(unique(prob_s$expert))
    prob_p <- c(rep(0.10, x), rep(0.40, x), rep(0.40, x), rep(0.10, x)) 
      # <10%, 10-49.9%, 50.0-89.9%, >=90%, repeated for each expert
    prob_s$p <- prob_p
    
    # Compute the Kullback-Leibler divergence of s and p
    prob_s$l <- prob_s$s * log(prob_s$s / prob_s$p)
      # replace NaNs with 0s (legitimate!)
      prob_s$l <- ifelse(prob_s$l == "NaN", 0, prob_s$l)
    
    # Aggregate so as to get Kullback-Leibler statistic for each expert
    experts <- aggregate(list(l = prob_s$l), by = list(expert = prob_s$expert),
      FUN = sum)

    # Compute each expert's calibration score
    n_q <- nrow(see_f) / x # number of calibration questions
    experts$cal <-  dchisq(2 * n_q * experts$l, df = 3)   
    

  #...................................      
  ## Compute each expert's information score
    
    # Initialise output
    out <- data.frame()
    
    # Compute the experts' information score, for each question
    for (i in unique(see_f$question)) {
      # question responses
      see_i <- see_f[which(see_f$question == i), ]
      
      # extremes of range      
      lo <- min(see_i$value10, see_i$answer)
      hi <- max(see_i$value90, see_i$answer)
      
      # apply a 10% overshoot rule
      lo <- lo - 0.1 * (hi - lo)
      hi <- hi + 0.1 * (hi - lo)
      
      # compute information score for the question, for each expert
      see_i$inf <- 0.10 * log(0.10 / (see_i$value10 - lo)) + 
        0.40 * log(0.40 / (see_i$value50 - see_i$value10)) + 
        0.40 * log(0.40 / (see_i$value90 - see_i$value50)) + 
        0.10 * log(0.10 / (hi - see_i$value90)) + 
        log(hi - lo)
  
      # add to output
      out <- rbind(out, see_i[, c("expert", "question", "inf")])
    }
    
    # Compute mean information score per expert, and add to results
    out <- aggregate(list(inf = out$inf), by = list(expert = out$expert),
      FUN = mean)
    experts <- merge(experts, out, by = "expert")
    
  #...................................      
  ## Compute each expert's combined score and performance weight
    
    # Compute combined scores
    experts$score <- experts$cal * experts$inf
        
    # Compute normalised performance weights
    experts$wt <- experts$score / sum(experts$score)
    
    # Output
    return(experts[, c("expert", "cal", "inf", "score", "wt")])
}



#...............................................................................   
### Function to implement one run of a Susceptible-Infectious-Recovered model
    # during the projection period, for all ages, an infection and a scenario,
    # tracking cumulative infection attack rate and deaths over period
    # uses Epiverse-TRACE 'epidemics' package
#...............................................................................

f_seir <- function(disease_f, scenario_f, timeline_f,
  initial_conditions_f = initial_conditions,
  contact_matrix_f = contact_matrix, demography_vector_f = demography_vector) {

  #...................................      
  ## Decide whether an outbreak will occur during the projection period
    
    # Sample from a binomial distribution of probability of an outbreak
    y <- rbinom(1, 1, prob = mean(timeline_f$pu) )
  
    # If 0, no outbreak: end function here
    if (y == 0) {
      
      # Return output with 0 infections, cases and deaths
      timeline_f[, c("new_infections", "symptomatic", "deaths")] <- 0
      out_f <-aggregate(timeline_f[,c("new_infections","symptomatic","deaths")],
        by = timeline_f[, c("subperiod", "age")], FUN = sum)
      return(out_f)
    }
  
# If p = 1, an outbreak occurs; function continues
if (y == 1) {
      
  #...................................      
  ## Select random starting date for epidemic, and compute resulting periods
    # starting time is not dependent on random run variable: it's totally random
      
    # Sample random starting day within the projection period
      # sample random starting day
      time_epid_start <- unique(timeline_f[which(timeline_f$time == 
        as.integer(runif(1, 0, 1) * max(timeline_f$time))), "time"])
      
      # update in timeline
      timeline_f$time_epid <- timeline_f$time - time_epid_start + 1
      timeline_f[which(timeline_f$time_epid < 0), "time_epid"] <- 0
      
      # accordingly, the starting month out of 1-6 is...
      rmonth <- unique(timeline_f[which(timeline_f$time_epid == 1), "month"])
    
      #...and the number of days to simulate in subperiods 1 and 2 are
      days_subperiod1 <- max(0, max(timeline_f[which(timeline_f$subperiod == 
        "months 1 to 3"), "time"]) - time_epid_start)
      days_subperiod2 <- min(max(timeline_f$time) - 
        max(timeline_f[which(timeline_f$subperiod== "months 1 to 3"), "time"]), 
        max(timeline_f$time) - time_epid_start)
      
  ## Set starting compartment sizes, based on susceptibility model
    
    # Get starting susceptibility by age
    x <- timeline_f[which(timeline_f$time == time_epid_start), 
      c("demography_group", "si")]
    x$demography_group <- factor(x$demography_group, 
      levels = names(demography_vector_f))
    x <- x[order(x$demography_group), ]
      
    # Set starting compartment sizes
    initial_conditions_f$I <- 1e-6 # 1 per million per age group infected
    initial_conditions_f$S <- x$si - initial_conditions_f$I
    initial_conditions_f$R <- 1 - 
      initial_conditions_f$I - initial_conditions_f$S
    
    # Prepare the population to model as affected by the epidemic
    model_population <- epidemics::population(
      name = "Gaza",
      contact_matrix = contact_matrix_f,
      demography_vector = demography_vector_f,
      initial_conditions = as.matrix(initial_conditions_f)
    )
        
  #...................................      
  ## Simulate epidemic over subperiod 1
    # (but only if the epidemic starts within sub-period 1)
  if (time_epid_start <= time_periods["end_subperiod1"]) {

    # Recognise parameters for the disease_f, specific to the subperiod
      # basic reproduction number
      r0 <- mean(timeline_f[which(timeline_f$subperiod == "months 1 to 3"), 
        "r0"])

      # infectiousness period
      tau <- mean(timeline_f[which(timeline_f$subperiod == "months 1 to 3"), 
        "tau"])
      
      # pre-infectiousness period
      pre_tau <- mean(timeline_f[which(timeline_f$subperiod == "months 1 to 3"), 
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
    initial_conditions_f$S <- x[which(x$compartment == "susceptible"), "value"]
    initial_conditions_f$E <- x[which(x$compartment == "exposed"), "value"]
    initial_conditions_f$I <- x[which(x$compartment == "infectious"), "value"]
    initial_conditions_f$R <- x[which(x$compartment == "recovered"), "value"]
    initial_conditions_f$V <- x[which(x$compartment == "vaccinated"), "value"]
  }

  #...................................      
  ## Simulate epidemic over subperiod 2

    # Recognise parameters for the disease_f, specific to the subperiod
      # basic reproduction number
      r0 <- mean(timeline_f[which(timeline_f$subperiod == "months 4 to 6"), 
        "r0"])

      # infectiousness period
      tau <- mean(timeline_f[which(timeline_f$subperiod == "months 4 to 6"), 
        "tau"])
      
      # pre-infectiousness period
      pre_tau <- mean(timeline_f[which(timeline_f$subperiod == "months 4 to 6"), 
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

    # Collect subperiod 1, if run
    run_full <- c()
    if (time_epid_start <= time_periods["end_subperiod1"]) {
      x <- subset(run_subperiod1, compartment == "susceptible")
      x <- merge(x, new_infections(run_subperiod1),
        by = c("time", "demography_group"), all.x = TRUE)
      x$time <- x$time + time_epid_start
      run_full <- rbind(run_full, x)
    }
          
    # Collect and add subperiod 2
    x <- subset(run_subperiod2, compartment == "susceptible")
    x <- merge(x, new_infections(run_subperiod2),
      by = c("time", "demography_group"), all.x = TRUE)
    x$time <- x$time + days_subperiod1 + time_epid_start
    run_full <- rbind(run_full, x)
    run_full <- run_full[, 
      c("time", "demography_group", "value", "new_infections")]
    colnames(run_full)[colnames(run_full) == "value"] <- "susceptibles"
    
    # Add to timeline
    timeline_f <- merge(timeline_f, run_full, 
      by = c("time", "demography_group"), all.x = TRUE)  
    
    # Correct new infections column
      # replace NA values (days without epidemic)
      timeline_f$new_infections <- na.replace(timeline_f$new_infections, 0)
    
      # some days have a negative output (VERY small number, approx. 0)
      timeline_f$new_infections <- ifelse(timeline_f$new_infections < 0, 0,
        timeline_f$new_infections)

    # Correct susceptibles column
      # some days have a negative output (VERY small number, approx. 0)
      timeline_f$susceptibles <- ifelse(timeline_f$susceptibles < 0, 0,
        timeline_f$susceptibles)
      
      # update susceptibles with real-time data    
      timeline_f$susceptibles <- ifelse(is.na(timeline_f$susceptibles),
      timeline_f$pop * timeline_f$si, timeline_f$susceptibles)  
    
  #...................................      
  ## Compute symptomatic cases and deaths
    
    # Symptomatic cases
    timeline_f$symptomatic <- timeline_f$new_infections * timeline_f$pd
    
    # Compute deaths, adjusted for changing susceptibility to infection/disease
      # compute change in susceptibles at all time points, by age
      timeline_f$change_susceptibles <- NA
      for (j in unique(timeline_f$age) ) {
        timeline_f[which(timeline_f$age == j), "change_susceptibles"] <-
          timeline_f[which(timeline_f$age == j), "susceptibles"] -
          timeline_f[which(timeline_f$age == j & timeline_f$time == 0), 
            "susceptibles"]
      }
      
      # compute adjustment factor for (changing) ratio of susceptibility to
        # disease and infection (# 0.000001 added to avoid division by 0)
      timeline_f$adjust <- 
        ((timeline_f[which(timeline_f$time == 0), "sd"] + 0.000001) * 
           timeline_f$pop + timeline_f$change_susceptibles) / 
        ((timeline_f[which(timeline_f$time == 0), "si"]  + 0.000001) * 
           timeline_f$pop + timeline_f$change_susceptibles)
      
      # compute deaths, adjusting 
      timeline_f$deaths <- timeline_f$symptomatic * timeline_f$cfr * 
        timeline_f$adjust

          
  #...................................      
  ## Collect and return results
    # Aggregate by sub-period
    out_f <- aggregate(timeline_f[,c("new_infections", "symptomatic", "deaths")],
      by = timeline_f[, c("subperiod", "age")], FUN = sum)
    
    # Return results      
    return(out_f)
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