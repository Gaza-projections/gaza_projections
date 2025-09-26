#...............................................................................
### ++++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NCDs +++++++++++++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO VISUALISE OUTPUT AND PRODUCE GRAPHS  ------------- ##
#...............................................................................


#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  pacman::p_load(
    flextable,   # To write tables in .docx format
    ggplot2,     # Data visualization
    ggpubr,      # Arranging multiple plots into a single plot
    lubridate,   # Makes it easier to work with dates and times
    readxl,      # Read Excel files
    reshape2,    # For converting between wide and long data structure
    scales,      # Scaling and formatting data for visualizations
    tidyverse,   # Tidyverse suite of packages
    viridis,     # Colour palettes
    zoo)         # For computing running means
  
  ##...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
    
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))
    
    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
                      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/gaza_NCDs/codes", "", dir_path)
    
    # Initialise random numbers
    set.seed(123)

    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
      
      # specific palette for the pre-war period, crisis to date period and 
          # three scenarios
      periods <-c("counterfactual", "to date", "ceasefire", "status quo", 
        "escalation")
      palette_periods <- c("azure4", palette_gen[c(15, 12, 8, 4)])
      names(palette_periods) <- periods
      show_col(palette_periods)

  ##...................................      
  ## Set some parameters
    
    # Scenarios
    scenarios <- c("ceasefire", "status quo", "escalation")    
    
    # Dates
    date_crisis <- as.Date("2023-10-07")
    date_start <- as.Date("2024-02-07")
    date_mid <- as.Date("2024-05-07")
    date_end <- as.Date("2024-08-06")

                
#...............................................................................
### Plotting scenario projections versus counterfactual, by NCD
#...............................................................................

  ##...................................      
  ## Prepare dataset
    
    # Read output
    NCD <- read_excel(paste0(dir_path, 
      "gaza_overall/inputs/gaza_overall_data.xlsx"))
    NCD <- subset(NCD, theme == "NCD")
    
    # Prepare baseline dataset
    NCD_base <- NCD[, !names(NCD) %in% c("d_excess_mean", "d_excess_lci", 
      "d_excess_uci", "d_excess_median", "d_crisis_mean", "d_crisis_lci", 
      "d_crisis_uci", "d_crisis_median")]
    NCD_base <- transform(NCD_base, d_crisis_excess = "baseline")
    NCD_base <- rename(NCD_base, mean = d_base_mean, lci = d_base_lci,
      uci = d_base_uci)

    # Prepare excess dataset
    NCD_excess <- NCD[, !names(NCD) %in% c("d_base_mean", "d_base_lci", 
      "d_base_uci", "d_base_median", "d_crisis_mean", "d_crisis_lci", 
      "d_crisis_uci", "d_crisis_median")]
    NCD_excess <- rename(NCD_excess, mean = d_excess_mean, lci = d_excess_lci,
      uci = d_excess_uci)
    NCD_excess <- transform(NCD_excess,d_crisis_excess = "excess")

    # Assemble dataset
    df1 <- bind_rows(NCD_excess, NCD_base)
    df1$category <- df1$d_crisis_excess
    df1$category <- gsub("d_", "", df1$category)
    df1$disease[df1$disease == "breast cancer"] <- 
      "cancer (breast,colorectal,lung)"
    df1$disease[df1$disease == "colorectal cancer"] <- 
      "cancer (breast,colorectal,lung)"
    df1$disease[df1$disease == "lung cancer"] <- 
      "cancer (breast,colorectal,lung)"
    df1$disease[df1$disease == "hemorrhagic stroke"] <- 
      "stroke (hemorrhagic,ischaemic)"
    df1$disease[df1$disease == "ischaemic stroke"] <- 
      "stroke (hemorrhagic,ischaemic)"

    # Aggregate subperiods
    df1 <- aggregate(df1[, c("mean", "lci", "uci")],
      by = df1[, c("scenario", "disease", "category" )], FUN = sum)
    df1$colour <- df1$scenario

    # Tease out baseline and add these deaths to each scenario for plotting
    x <- subset(df1, category == "baseline")
    x <- rbind(x, x, x)
    x$scenario <- sort(rep(scenarios, 3))
    df1 <- rbind(subset(df1, category != "baseline"), x)
    df1$colour <- ifelse(df1$category == "baseline", "pre-war", df1$scenario)
    df1$scenario <- factor(df1$scenario, levels = scenarios)

    # Add correct error bars for excess
    x <- x[, c("scenario", "disease", "mean")]
    colnames(x)[colnames(x) == "mean"] <- "mean_base"
    df1 <- merge(df1, x, by = c("scenario", "disease"), all.x = TRUE)
    df1$lci_tot <- df1$lci + df1$mean_base
    df1$uci_tot <- df1$uci + df1$mean_base
    df1[which(df1$colour == "pre-war"), c("lci_tot", "uci_tot")] <- NA
    df1$colour <- factor(df1$colour, levels = c(scenarios, "pre-war"))
    df1$labels <- ifelse(df1$colour == "pre-war", NA, as.integer(df1$mean))
    df1 <- df1[!duplicated(df1[c('scenario', 'disease', 'category', 
      'mean', 'colour')]), ]


  ##...................................      
  ## Plot
    
    # Plot
    plot <- ggplot(data = df1, aes(y = mean, x = scenario, colour = colour, 
      fill = colour)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.5) +  
      geom_errorbar(aes(ymin = lci_tot, ymax = uci_tot), width = 0.2, 
        linetype = "21") +
      theme_bw() +
      facet_wrap(disease ~ ., scales = "free_y") +
      scale_colour_manual(values = palette_periods[c(1, 3, 4, 5)]) +
      scale_fill_manual(values = palette_periods[c(1, 3, 4, 5)]) +
      scale_y_continuous("number of deaths") +
      theme(legend.position = "none", panel.grid.major.x = element_blank(),
            axis.title.y = element_blank()) +
      geom_text(data = subset(df1, !is.na(labels)), aes(y = mean + mean_base, 
        x = scenario, label = labels), nudge_x = 0.3, 
        nudge_y = (df1$mean[!is.na(df1$labels)] + 
            df1$mean_base[!is.na(df1$labels)]) * 0.05, size = 3.5)

    # Save
    ggsave(paste0(dir_path, "/Gaza_NCDs/outputs/ncd_excess.png"), width = 20, 
      height = 20, units = "cm", bg = "white")


#...............................................................................
### Plotting counterfactual and total NCD deaths, by month
#...............................................................................

  ##...................................      
  ## Prepare dataset

    # Some values
    ncds <- c("Bcancer", "Ccancer", "CKD", "DM1", "HS", "IHD", "IS", "Lcancer")  
    months_start <- as.Date(c("2023-10-07", "2023-11-07", "2023-12-07",
      "2024-01-07", "2024-02-07", "2024-03-07", "2024-04-07", "2024-05-07",
      "2024-06-07", "2024-07-07"))
    x <- "gaza_NCDs/outputs/Simulation Runs Raw Results (1000)/"
    scenarios <- c("Escalation", "Status Quo", "Ceasefire")
        
    # Read baseline output
    base <- vector(length = length(ncds), mode = "list")
    names(base) <- ncds
    for (i in ncds) {
      df <- read.csv(paste0(dir_path, x, "baseline_", i, ".csv"), header = F)
      base[[i]] <- data.frame(
        ncd = rep(i, length(months_start)),
        month_start = months_start,
        mean = rowMeans(df),
        lci = apply(df, 1, quantile, 0.025),
        uci = apply(df, 1, quantile, 0.975)
      )
    }
    base <- do.call(rbind, base)
    
    # Read total output
    total <- expand.grid(ncds, scenarios,  months_start)
    colnames(total) <- c("ncd", "scenario", "month_start")
    total <- total[order(total$ncd, total$scenario, total$month_start), ]
    total[, c("mean", "lci", "uci")] <- NA
    for (i in ncds) {
      print(i)
      for (j in scenarios) {
        df <- read_xlsx(paste0(dir_path, x, "scenario_total_", i, ".xlsx"),
          sheet = j, col_names = paste0("sim", 1:1000), range = "A1:ALL10",
          progress = T)
        df_ij <- data.frame(
          mean = rowMeans(df),
          lci = apply(df, 1, quantile, 0.025),
          uci = apply(df, 1, quantile, 0.975)
        )
        total[which(total$ncd == i & total$scenario == j), 
          c("mean", "lci", "uci")] <- df_ij
      }
    }
    
    # Sum up across all NCDs
    base <- aggregate(base[, c("mean", "lci", "uci")],
      by = list(month_start = base$month_start), FUN = sum)
    total <- aggregate(total[, c("mean", "lci", "uci")],
      by = total[, c("scenario", "month_start")], FUN = sum)
    
    # Append the datasets
    total$scenario <- tolower(total$scenario)
    todate <- subset(total, scenario == "status quo" & 
        month_start < as.Date("2024-02-07"))
    todate$scenario <- "to date"
    total <- subset(total, month_start >= as.Date("2024-02-07"))
    base$scenario <- "counterfactual"
    base <- base[, colnames(total)]
    df <- rbind(base, todate, total)
    
    # Additional preparations
    df$scenario <- factor(df$scenario, levels = c("counterfactual", "to date",
      "ceasefire", "status quo", "escalation"))
    df <- df[order(df$scenario, df$month_start), ]
    df1 <- subset(df, month_start %in% as.Date(c("2024-01-07", "2024-02-07")) &
      scenario != "counterfactual")
    df1 <- rbind(df1[1, ], df1[1, ], df1[1, ], df1[2:4, ])
    df1$scenario <- rep(c("escalation", "status quo", "ceasefire"), 2)  
  
        
  ##...................................      
  ## Plot

    # Plot
    ggplot(df, aes(x = month_start, y = mean, colour = scenario)) +
      theme_bw() +
      geom_point() +
      geom_line() +
      scale_x_date("month starting", breaks = months_start,
        date_labels = "%d %b %Y", expand = expansion (add = c(10, 10))) +
      scale_y_continuous("projected NCD deaths", limits = c(0, 800),
        expand = expansion(add = c(0, 50)), breaks = seq(0, 800, 100)) +
      scale_colour_manual("", values = palette_periods) +
      geom_line(data = df1, aes(x = month_start, y = mean, group = scenario),
        linetype = "11", colour = palette_periods[2]) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1), panel.grid.major.x = element_blank())

    # Save
    ggsave(paste0(dir_path, "/Gaza_NCDs/outputs/ncd_monthly.png"), width = 20, 
      height = 12, units = "cm", bg = "white")
    write.csv(df, paste0(dir_path, "/Gaza_NCDs/outputs/ncd_monthly.csv"), 
      row.names = F)
    
    
#...............................................................................
### ENDS
#...............................................................................
            