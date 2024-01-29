#...............................................................................
### +++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INFECTIONS ++++++++++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO GENEATE DUMMY INPUT DATASETS (JUST FOR CODE TESTING)  ---- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 





#...............................................................................  
### Creating dummy input datasets
#...............................................................................

  #...................................      
  ## Proportion susceptible to infection 
    # - from immunity model
  si <- data.frame(month = 1:6)
  si[, ages] <- 0
  si_modelled <- list()
  for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
    for (j in scenarios) {
      
      # if scenario is optimistic
      if (j == "ceasefire") {
        x <- si
        x[, ages] <- 0.20
        si_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is central
      if (j == "status quo") {
        x <- si
        x[, ages] <- 0.40
        si_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is pessimistic
      if (j == "escalation") {
        x <- si
        x[, ages] <- 0.60
        si_modelled[[i]][[j]] <- x
      }  
        
    }
  }  

  #...................................      
  ## Proportion susceptible to severe disease 
    # - from immunity model
  sd <- data.frame(month = 1:6)
  sd[, ages] <- 0  
  sd_modelled <- list()
  for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
    for (j in scenarios) {
      
      # if scenario is optimistic
      if (j == "ceasefire") {
        x <- sd
        x[, ages] <- 0.20
        sd_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is central
      if (j == "status quo") {
        x <- sd
        x[, ages] <- 0.30
        sd_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is pessimistic
      if (j == "escalation") {
        x <- sd
        x[, ages] <- 0.40
        sd_modelled[[i]][[j]] <- x
      }  
        
    }
  }  

  
  #...................................      
  ## Dummy empirical distributions of probability of outbreak
  pu <- data.frame(pu = seq(0, 1, by = 0.05))
  see_pu <- list()
  for (i in scenarios) {
    for (j in diseases_epid) {
      p <- pu
      x <- rnorm(1, 0.5, 0.2)
      if (i == "ceasefire") {x <- x * 0.8}
      if (i == "escalation") {x <- x * 1.2}      
      p$p <- dnorm(pu$pu, x, 0.15)
      p$p_cum <- cumsum(p$p) / sum(p$p)
      see_pu[[i]][[j]] <- p
    }
  }
  
  
  #...................................      
  ## Dummy empirical distributions of R0, by period
    
    # Dummy empirical distributions of R0 for measles, by period
    r0_measles <- data.frame(r0 = seq(5, 15, 0.5))
    r0_measles$p <- dnorm(r0_measles$r0, 10, 2)
    r0_measles$p_cum <- cumsum(r0_measles$p) / sum(r0_measles$p)

    # Dummy empirical distributions of R0 for cholera, by period
    r0_cholera <- data.frame(r0 = seq(1, 3, 0.1))
    r0_cholera$p <- dnorm(r0_cholera$r0, 1.5, 0.2)
    r0_cholera$p_cum <- cumsum(r0_cholera$p) / sum(r0_cholera$p)
    
    # Overall
    see_r0 <- list()
    for (i in scenarios) {
      for (j in c("measles", "cholera")) {
        if (j == "measles") {x <- r0_measles}
        if (j == "cholera") {x <- r0_cholera}
        
        if (i == "ceasefire") {x$r0 <- x$r0 * 0.8}
        if (i == "escalation") {x$r0 <- x$r0 * 1.2}
        
        for (k in c("subperiod1", "subperiod2") ) {
          see_r0[[i]][[j]][[k]] <- x
        }
      }
    }
  
  #...................................      
  ## Dummy empirical distributions of CFR, by period
    
    # Dummy empirical distributions of cfr for measles, by period
    cfr_measles <- data.frame(cfr = seq(0.01, 0.10, 0.01))
    cfr_measles$p <- dnorm(cfr_measles$cfr, 0.05, 0.02)
    cfr_measles$p_cum <- cumsum(cfr_measles$p) / sum(cfr_measles$p)

    # Dummy empirical distributions of cfr for cholera, by period
    cfr_cholera <- data.frame(cfr = seq(0.01, 0.20, 0.01))
    cfr_cholera$p <- dnorm(cfr_cholera$cfr, 0.06, 0.015)
    cfr_cholera$p_cum <- cumsum(cfr_cholera$p) / sum(cfr_cholera$p)
    
    # Overall
    see_cfr <- list()
    for (i in scenarios) {
      for (j in c("measles", "cholera")) {
        if (j == "measles") {x <- cfr_measles}
        if (j == "cholera") {x <- cfr_cholera}
        
        if (i == "ceasefire") {x$cfr <- x$cfr * 0.8}
        if (i == "escalation") {x$cfr <- x$cfr * 1.2}
        
        for (k in c("subperiod1", "subperiod2") ) {
          see_cfr[[i]][[j]][[k]] <- x
        }
      }
    }
  
  #...................................      
  ## Overall SEE data
  see_data <- list(see_pu, see_r0, see_cfr)
  names(see_data) <- c("pu", "r0", "cfr")   

  
#...............................................................................  
### ENDS
#...............................................................................

  
  