#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO SPECIFY EXISTING MODELS OF WEIGHT LOSS VS INTAKE  ------ ##
#...............................................................................




#...............................................................................  
### Function to estimate fat mass (Kg), based on weight (Kg), height (m),
  # age and gender; from Hall KD, Sacks G, Chandramohan D, et al.
  # Quantification of the effect of energy imbalance on bodyweight.
  # Lancet 2011; 378: 826–37. (See Web Appendix, Equation 4)
#...............................................................................

f_fat <- function(data = df_ad) {
  
  # Read parameters
  gender <- data[["gender"]]
  age <- as.numeric(data[["age"]])
  weight <- as.numeric(data[["weight"]])
  height <- as.numeric(data[["height"]])
  
  # Males
  if (gender == "m") {
    out <- (weight / 100) * 
      (0.14 * age + 37.31 * log(weight / 
      (height^2)) - 103.94)
  }
  
  # Females  
  if (gender == "f") {
    out <- (weight / 100) * 
      (0.14 * age + 39.96 * log(weight / 
      (height^2)) - 102.01)
  }
  
  # Return output
  return(out)
}


#...............................................................................  
### Function to estimate resting metabolic rate (rmr)
  # from Mifflin MD, St Jeor ST, Hill LA, Scott BJ, Daugherty SA, Koh YO. A new
  # predictive equation for resting energy expenditure in healthy individuals.
  # Am J Clin Nutr. 1990; 51(2): 241-7.
#...............................................................................

f_rmr <- function(data = df_ad) {

  # Read parameters
  gender <- data[["gender"]]
  age <- as.numeric(data[["age"]])
  weight <- as.numeric(data[["weight"]])
  height <- as.numeric(data[["height"]]) * 100 #(in cm)
    
  # Females
  if (gender == "f") {out <- 9.99 * weight + 6.25 * height - 4.92 * age - 161}
  
  # Males  
  if (gender == "m"){out <- 9.99 * weight + 6.25 * height - 4.92 * age + 5}
  
  # Return output
  return(out)
  
}


#...............................................................................  
### Function encoding Model 2: Hall KD, Sacks G, Chandramohan D, et al.
  # Quantification of the effect of energy imbalance on bodyweight.
  # Lancet 2011; 378: 826–37. (See Web Appendix, Equation 14)
  # NOTE: parameter values converted from Joules to Kcal
#...............................................................................

f_hall <- function(data = df, f_fat_f = f_fat) {  

  #...................................      
  ## Define parameters
    # Fixed parameters
    nu_f <- 179.254
    nu_l <- 229.446
    gamma_f <- 3.10707
    gamma_l <- 21.9885
    rho_f <- 9440.727
    rho_l <- 1816.44
    beta_at <- 0.14
    beta_tef <- 0.10
    forbes <- 10.4
    pal <- 1.5      
  
    # Read starting and time-changing parameters from data
    wt_start <- as.numeric(data[["weight"]])
    weight <- as.numeric(data[["wt_now"]])
    rmr <- as.numeric(data[["rmr"]])
    f_start <- as.numeric(data[["f_start"]])
    change_intake <- as.numeric(data[["change_intake"]])  
    height <- as.numeric(data[["height"]])
    gender <- data[["gender"]]
    age <- as.numeric(data[["age"]])
    
  #...................................      
  ## Compute components of main equation
    
    # alpha
#    alpha <- forbes / f_start  # simplification for modest weight gain (unused)
      # first work out fat mass now
      if (gender == "m") {f_now <- (weight / 100) * (0.14 * age + 37.31 * 
        log(weight / (height^2)) - 103.94)
      }
      if (gender == "f") {f_now <- (weight / 100) * (0.14 * age + 39.96 * 
        log(weight / (height^2)) - 102.01)
      }
        
      # then work out alpha  
      alpha <- forbes / f_now
    
    # beta
    beta <- beta_at + beta_tef
#####TO DO: maybe beta_at should be at 0 after initial 14 days of wt loss

    # delta
    delta <- ((1 - beta_tef) * pal - 1) * rmr / weight
    
    # rho
    rho <- (nu_f + rho_f + alpha * nu_l + alpha * rho_l)/ 
      ((1 - beta) * (1 + alpha)) 
  
    # tau
    tau <- (nu_f + rho_f + alpha * (nu_l + rho_l) ) / 
      (gamma_f + delta + alpha * (gamma_l + delta)) 
    
  #...................................      
  ## Compute and output weight change per time step (main equation)
    
    # Weight change for this time step
    change_wt <- change_intake / rho - (weight - wt_start) / tau

    # Output
    return(change_wt)
      
}  


#...............................................................................  
### ENDS
#...............................................................................
     