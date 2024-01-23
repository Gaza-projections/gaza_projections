#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO ESTIMATE FOOD AID THAT HAS ARRIVED TO GAZA --------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Preparing necessary objects
#...............................................................................

  #...................................      
  ## Make an assumption about food aid during time not covered by the
    # trucks dataset
  
    # Compute median trucks/day for the last two weeks of available data
    df_tr <- df_tr[order(df_tr$date), ]
    x <- df_tr[(nrow(df_tr)-13) : nrow(df_tr), c("n_trucks", "n_trucks_food")]
    median_trucks <- apply(x[, c("n_trucks", "n_trucks_food")], 2, 
      function(k) {median(k, na.rm = TRUE)})
    
    # Initialise additional rows to bridge gap to start of projection period
    x <- data.frame(
      date = (max(df_tr$date) + 1) : (date_start - 1),
      n_trucks = NA, n_trucks_food = NA)
    x$date <- as.Date(x$date)
    df_tr <- rbind(df_tr, x)
    
    # Assign last available two-week average to dates with missing numbers
    df_tr[which(is.na(df_tr$n_trucks)), "n_trucks"] <- median_trucks["n_trucks"]
    df_tr[which(is.na(df_tr$n_trucks_food)), "n_trucks_food"] <- 
      median_trucks["n_trucks_food"]
     
  #...................................      
  ## Initialise outputs of each run
    
    # Output per day since crisis started
    out_day <- expand.grid(run = runs$run, date = date_crisis:(date_start - 1))
    out_day$date <- as.Date(out_day$date)
    out_day$aid <- NA
    
    # Output per week since crisis started
    weekno <- as.numeric(df_tr$date - df_tr$date[1]) %/% 7
    out_week <- expand.grid(run = runs$run, week = unique(df_tr$date[1] + 
      7 * weekno))
    out_week$aid <- NA
    
    # output per month since crisis started
    month_start <- unique(paste(year(df_tr$date), month(df_tr$date), sep = "-", 
      day(df_tr$date[1])) )
    out_month <- expand.grid(run = runs$run, 
      month = month_start[1:(length(month_start) - 1)])
    out_month$month <- lubridate::ymd(as.character(out_month$month))
    out_month$aid <- NA

  #...................................      
  ## Initialise or read other objects 

    # Initialise variables in the truck database
    df_tr$aid <- NA
    df_tr$week <- df_tr$date[1] + 7 * weekno
    x <- lubridate::ymd(month_start)
    df_tr$month <- cut(df_tr$date, breaks = x, 
      include.lowest = TRUE, right = FALSE)
    
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = max(runs$run), style = 3)

    # Range of number of people covered for 1 day per MT of food aid (based on 
      # recommended daily intake)
    people_per_mt_range <- c(
      gen_pars[which(gen_pars$parameter == "people_per_mt_min"), "value_gen"],
      gen_pars[which(gen_pars$parameter == "people_per_mt_max"), "value_gen"] )
    people_per_mt_range <- as.numeric(people_per_mt_range)

    # Range of metric tonnage per truck
    mt_per_truck_range <- c(
      gen_pars[which(gen_pars$parameter == "mt_per_truck_min"), "value_gen"],
      gen_pars[which(gen_pars$parameter == "mt_per_truck_max"), "value_gen"] )
    mt_per_truck_range <- as.numeric(mt_per_truck_range)

        
#...............................................................................  
### Estimating caloric intake from food aid from the crisis' start to date
#...............................................................................

for (run_i in 1:nrow(runs)) {  

    # Update progress bar
    setTxtProgressBar(pb, run_i)  
  
  #...................................      
  ## Select random quantities needed for run

    # Identify random number from [0,1]
    rx <- runs[run_i, "rx"]
      # rx is the extent along the positive-negative spectrum, so 1-rx = inverse  
    
    # Select number of people covered for 1 day per MT of food aid
    people_per_mt <- people_per_mt_range[1] + 
      (1 - rx) * (people_per_mt_range[2] - people_per_mt_range[1])
    
    # Select tonnage of truck
    mt_per_truck <- mt_per_truck_range[1] + 
      (1 - rx) * (mt_per_truck_range[2] - mt_per_truck_range[1])
    
  #...................................      
  ## Compute and output daily intake equivalent of food aid per day
    
    # Daily intake from food aid
    df_tr$aid <- df_tr$n_trucks_food * mt_per_truck * people_per_mt * 
      intake_target / pop
    
    # Daily output
    x <- out_day[which(out_day$run == run_i), c("run", "date")]
    x <- merge(x, df_tr[, c("date", "aid")], by = "date", all.x = TRUE)
    out_day[which(out_day$run == run_i), ] <- x[, c("run", "date", "aid")]
    
    # Aggregate to weekly output
    out_week[which(out_week$run == run_i), c("week", "aid")] <-
      aggregate(df_tr$aid, by = list(df_tr$week), FUN = mean)
    
    # Aggregate to monthly output
    out_month[which(out_month$run == run_i), c("month", "aid")] <-
      aggregate(df_tr$aid, by = list(month = df_tr$month), FUN = mean)
    
} # close run_i loop
close(pb)      
     

#...............................................................................  
### Visualising and outputting estimates
#...............................................................................

  #...................................      
  ## Output daily estimates for subsequent scripts
  aid_to_date <- out_day
  write_rds(aid_to_date,
    paste(dir_path, "outputs/", "out_food_aid_to_date.rds", sep=""))
    
  #...................................      
  ## Visualise weekly estimates
    
    # Prepare data for plotting
    df <- aggregate(out_week$aid, by = list(date = out_week$week), FUN = 
      function(x) quantile(x, c(0.50, 0.025, 0.975)) )
    df <- data.frame(df$date, unlist(df$x))
    colnames(df) <- c("date", "median", "lci", "uci")
    
    # Plot
    ggplot(data = df, aes(x = date)) +
      geom_bar(aes(y = median), stat = "identity", fill = palette_cb[1],
        colour = palette_cb[1], alpha = 0.5) +
      geom_errorbar(aes(ymin = lci, ymax = uci), colour = palette_cb[1],
        width = 3, linetype = "21") +
      theme_bw() +
      scale_x_date("week starting", breaks = df$date, 
        date_labels = "%d-%b-%Y") +
      scale_y_continuous(expand = c(0, 50),
        name = "daily caloric equivalent of food aid trucked in (Kcal)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())
    
    ggsave(paste(dir_path, "outputs/", "estimated_food_aid.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)  
######TO DO: Colour forecast weeks differently

#...............................................................................  
### ENDS
#...............................................................................    