#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - INJURIES +++++++++++ ###
#...............................................................................

#...............................................................................
## -- R SCRIPT TO READ DATASETS AND PARAMETERS, AND PREPARE FOR ANALYSIS  --- ##
#...............................................................................

                                # LSHTM (January 2024)
                                # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Reading in required parameters
#...............................................................................

  #...................................      
  ## Read parameters from main parameters file

    # Identify file name
    filename <- paste(dir_path, 'inputs/', 
      "gaza_injuries_parameters.xlsx", sep="")
    
    # Read general parameters
    gen_pars <- data.frame(readxl::read_excel(filename, sheet = "general"))

    # Read proportion surviving injuries by time, out of all who die of injuries
    surv <- data.frame(readxl::read_excel(filename, sheet = "surv"))

  #...................................      
  ## Read in or set other parameters
    
    # Number of runs
    runs <-as.integer(gen_pars[which(gen_pars$parameter == "runs"),"value_gen"])
    
    # Identify important dates
    date_crisis <- dmy(gen_pars[which(gen_pars$parameter == "date_crisis"), 
      "value_gen"])
    date_start <- dmy(gen_pars[which(gen_pars$parameter == "date_start"), 
      "value_gen"])
    date_mid <- dmy(gen_pars[which(gen_pars$parameter == "date_mid"), 
      "value_gen"])
    date_end <- dmy(gen_pars[which(gen_pars$parameter == "date_end"), 
      "value_gen"])
    
      # duration of period in days
      days_period <- as.integer(date_end - date_start)
    
    # Identify subperiods
    subperiods <- c("months 1 to 3", "months 4 to 6")
    
    # Identify scenarios
    scenarios <- c("ceasefire", "status quo", "escalation")
    
    # Identify age groups
    ages <- c("0mo", "1 to 11mo", "12 to 59mo", "5 to 9yo", "10 to 14yo", 
      "15 to 19yo", "20 to 29yo", "30 to 39yo", "40 to 49yo", "50 to 59yo", 
      "60 to 69yo", "70 to 79yo", "80 to 100yo") # groups we want

    # Identify population
    x <- gen_pars[which(gen_pars$parameter == "pop"), 
      grep("value_a", colnames(gen_pars))]
    pop <- data.frame(age = gsub("value_a", "", names(x)), pop = unlist(x) )

      # total population
      pop_tot <- sum(pop$pop)
      
    # Identify CFR of injuries who do not die immediately, by scenario
    cfr_cf <- as.numeric(gen_pars[which(gen_pars$parameter == "cfr_cf"),
      "value_gen"])
    cfr_sq <- as.numeric(gen_pars[which(gen_pars$parameter == "cfr_sq"),
      "value_gen"])
    cfr_es <- as.numeric(gen_pars[which(gen_pars$parameter == "cfr_es"),
      "value_gen"])
    
#...............................................................................  
### Reading in and preparing data
#...............................................................................

  #...................................      
  ## Daily deaths, UNRWA deaths, and injuries count

    # Identify file name
    filename <- paste(dir_path, 'inputs/', "gaza_injuries_data.xlsx", sep="")
    
    # Read data
    daily <- data.frame(readxl::read_excel(filename, sheet = "daily"))
    
    # Check that all dates are featured
    table(diff(daily$date))
    range(daily$date)

    # Add general and UNRWA population sizes
    daily$pop <- sum(pop$pop)
    daily$pop_unrwa <- as.numeric(gen_pars[
      which(gen_pars$parameter == "pop_unrwa"), "value_gen"])
    
    # Compute cumulative UNRWA deaths and death rate
    daily$d_unrwa <- na.replace(daily$d_unrwa, 0)
    daily$d_unrwa_cum <- cumsum(daily$d_unrwa)
    daily$dr_unrwa_cum <- daily$d_unrwa_cum * 1000 / daily$pop_unrwa
    
    # Compute cumulative general population deaths and injuries
    
    x1 <- na.omit(daily[, c("date", "d")])
    x1$d_cum <- cumsum(x1$d)
    x2 <- na.omit(daily[, c("date", "i")])
    x2$i_cum <- cumsum(x2$i)
    
    # Compute difference in days between each death / injury observation
    x1$t_since_d <- c(1, diff(x1$date))
    x2$t_since_i <- c(1, diff(x2$date))
    
    # Add to overall database
    daily <- merge(daily, x1[, c("date", "d_cum", "t_since_d")], 
      by = "date", all.x = TRUE)
    daily <- merge(daily, x2[, c("date", "i_cum", "t_since_i")], 
      by = "date", all.x = TRUE)
    
    # Compute person-time since last death observation
    daily$ptime <- daily$pop * daily$t_since_d
  
    # Add time and reformat date
    daily$time <- 1:nrow(daily)
    daily$date <- as.Date(daily$date)  
    
    # Compute general population death rate (per 1000)
    daily$dr <- daily$d * 1000 / daily$ptime
    
    # Compute general population cumulative death rate (per 1000)
    daily$dr_cum <- daily$d_cum * 1000 / daily$pop
  
    # Compute ratio of general and UNRWA cumulative death rate
    daily$pc <- daily$dr_cum / daily$dr_unrwa_cum
    range(daily$pc, na.rm = TRUE)
    
    # Interpolate daily deaths to fill NA values
    x <- na.omit(daily[, c("date", "d_cum")])
    daily$d_cum_ipol <- approx(x = x$date, y = x$d_cum, xout <- daily$date)$y
    daily$d_ipol <- c(daily[1, "d_cum_ipol"], diff(daily$d_cum_ipol))
    
    # Interpolate daily injuries to fill NA values
    x <- na.omit(daily[, c("date", "i_cum")])
    daily$i_cum_ipol <- approx(x = x$date, y = x$i_cum, xout <- daily$date)$y
    daily$i_ipol <- c(daily[1, "i_cum_ipol"], diff(daily$i_cum_ipol))
    
    
    
  #...................................      
  ## IDPs in shelters
    
    # Read data
    idps <- data.frame(readxl::read_excel(filename, sheet = "idps"))
    
    # Fix date
    idps$date <- as.Date(idps$date)
    
    # Figure out proportion in shelters
    idps$prop_sh <- idps$n_sh / pop_tot
    
    # Add to daily file
    daily <- merge(daily, idps[, c("date", "prop_sh")], by = "date", 
      all.x = TRUE)
    
    # Interpolate missing values
    daily$prop_sh_ipol <- approx(x = daily$date, y = daily$prop_sh,
      xout = daily$date, method = "linear", rule = 2)$y
        
  #...................................      
  ## Deaths projected for all disease modules for period to date
   
    # Read file
    proj <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
      "gaza_noninjury_to_date.xlsx", sep="")))
    
    # Reshape long and aggregate across all modules and dates
    proj <- reshape(proj, direction = "long",varying = c("d_female", "d_male"),
      idvar = c("month_start", "age", "module"), timevar = "gender", 
      v.names = "d_other")
    proj$gender <- ifelse(proj$gender == 1, "female", "male")   
    proj <- aggregate(list(d_other = proj$d_other), 
      by = proj[, c("age", "gender")], FUN = sum)
    
    # Convert to daily deaths
    proj$d_other <- proj$d_other / as.integer(date_start - date_crisis)
    
    # Other preparations
    proj$age <- factor(proj$age, levels = ages)
    proj$gender <- factor(proj$gender, levels = c("male", "female") )
    proj <- proj[order(proj$age, proj$gender), ]

  #...................................      
  ## MoH line list of deaths, with age and gender

    # Read data
    moh_d <- data.frame(readxl::read_excel(filename, sheet = "moh_list"))
    moh_d$gender <- factor(moh_d$gender, levels = c("male", "female"))
    
  #...................................      
  ## Data on all deaths and unexploded ordnance deaths from 2014 war
    
    # Read data    
    ord <- data.frame(readxl::read_excel(filename, sheet = "ordnance"))
    ord <- ord[, c("variable", "value")]
     
    

    
#...............................................................................
### ENDS
#...............................................................................

