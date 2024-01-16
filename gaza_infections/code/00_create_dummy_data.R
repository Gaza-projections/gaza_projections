#...............................................................................  
### Creating dummy input datasets (just for developing code)
#...............................................................................

  #...................................      
  ## Proportion unprotected from infection and severe disease 
    # - from immunity model
  s <- data.frame(month = 1:6)
  s[, ages] <- 0
  s_modelled <- list()
  for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
    for (j in scenarios) {
      
      # if scenario is optimistic
      if (j == "best") {
        x <- s
        x[, ages] <- 0.20
        s_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is central
      if (j == "central") {
        x <- s
        x[, ages] <- 0.40
        s_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is pessimistic
      if (j == "worst") {
        x <- s
        x[, ages] <- 0.60
        s_modelled[[i]][[j]] <- x
      }  
        
    }
  }  

  #...................................      
  ## Proportion unprotected from infection but protected against severe disease 
    # - from immunity model
  vd <- data.frame(month = 1:6)
  vd[, ages] <- 0  
  vd_modelled <- list()
  for (i in diseases[which(diseases$immunity_source == "model"), "disease"]) {
    for (j in scenarios) {
      
      # if scenario is optimistic
      if (j == "best") {
        x <- vd
        x[, ages] <- 0.40
        vd_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is central
      if (j == "central") {
        x <- vd
        x[, ages] <- 0.30
        vd_modelled[[i]][[j]] <- x
      }  
      
      # if scenario is pessimistic
      if (j == "worst") {
        x <- vd
        x[, ages] <- 0.20
        vd_modelled[[i]][[j]] <- x
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
      if (i == "best") {x <- x * 0.8}
      if (i == "worst") {x <- x * 1.2}      
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
        
        if (i == "best") {x$r0 <- x$r0 * 0.8}
        if (i == "worst") {x$r0 <- x$r0 * 1.2}
        
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
        
        if (i == "best") {x$cfr <- x$cfr * 0.8}
        if (i == "worst") {x$cfr <- x$cfr * 1.2}
        
        for (k in c("subperiod1", "subperiod2") ) {
          see_cfr[[i]][[j]][[k]] <- x
        }
      }
    }
  
  #...................................      
  ## Overall SEE data
  see_data <- list(see_pu, see_r0, see_cfr)
  names(see_data) <- c("pu", "r0", "cfr")   
