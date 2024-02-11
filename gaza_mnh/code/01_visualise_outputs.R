#...............................................................................
### + GAZA CRISIS: HEALTH IMPACT PROJECTIONS - MATERNAL & NEONATAL HEALTH ++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO VISUALISE OUTPUT AND PRODUCE GRAPHS  ------------- ##
#...............................................................................

                          # Francesco Checchi, Zeina Jamaluddine (January 2024)

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

  #...................................      
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
    dir_path <- gsub("/code", "", dir_path)
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
      # general palette
      palette_gen <- viridis(16)
      show_col(palette_gen)
          
      # specific palette for the pre-war period, crisis to date period and 
        # three scenarios
      periods <-c("pre-war", "to date", "ceasefire", "status quo", "escalation")
      
      palette_periods <- c("azure4", palette_gen[c(2, 12, 8, 4)])
      names(palette_periods) <- periods
      show_col(palette_periods)


#...............................................................................
### Producing tables and graphs for the report and annex
#...............................................................................

  #...................................      
  ## Prepare for plotting

    # Read the LiST output
      # maternal deaths
      df_m <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
      "gaza_mnh_list_outputs.xlsx", sep=""), sheet = "maternal"))
      
      # neonatal deaths
      df_n <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
        "gaza_mnh_list_outputs.xlsx", sep=""), sheet = "neonatal"))
      
      # stillbirths
      df_s <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
        "gaza_mnh_list_outputs.xlsx", sep=""), sheet = "stillbirths"))

    # Read the cumulative LiST output
    df_cum <- data.frame(readxl::read_excel(paste(dir_path, 'inputs/', 
      "gaza_mnh_list_outputs.xlsx", sep=""), sheet = "cum"))
        
    # Set some parameters
      # scenarios
      scenarios <- c("ceasefire", "status quo", "escalation")    
    
      # dates
      date_crisis <- as.Date("2023-10-07")
      date_start <- as.Date("2024-02-07")
      date_mid <- as.Date("2024-05-07")
      date_end <- as.Date("2024-08-06")
      
  #...................................      
  ## Graph of baseline and excess deaths - maternal, neonatal and stillbirths

    # Assemble dataset
    df <- rbind(df_m, df_n, df_s)
    df$category <- df$d_crisis_excess
    df$category <- gsub("d_", "", df$category)
    
    # Aggregate subperiods
    df <- aggregate(df[, c("mean", "lci", "uci")], 
      by = df[, c("scenario", "disease", "category")], FUN = sum)
    
    # Tease out baseline and add these deaths to each scenario for plotting
    x <- subset(df, category == "baseline")
    x <- rbind(x, x, x)
    x$scenario <- sort(rep(scenarios, 3))
    df <- rbind(subset(df, category != "baseline"), x)
    df <- subset(df, category != "crisis")
    df$colour <- ifelse(df$category == "baseline", "pre-war", df$scenario)
    df$scenario <- factor(df$scenario, levels = scenarios)

    # Add correct error bars for excess
    x <- x[, c("scenario", "disease", "mean")]
    colnames(x)[colnames(x) == "mean"] <- "mean_base"
    df <- merge(df, x, by = c("scenario", "disease"), all.x = TRUE)
    df$lci_tot <- df$lci + df$mean_base
    df$uci_tot <- df$uci + df$mean_base
    df[which(df$colour == "pre-war"), c("lci_tot", "uci_tot")] <- NA
    df$colour <- factor(df$colour, levels = c(scenarios, "pre-war"))
    df$labels <- ifelse(df$colour == "pre-war", NA, as.integer(df$mean))
    
    # Plot
      # maternal
      df1 <- subset(df, disease == "maternal")
      plot_m <- ggplot(data = df1,
        aes(y = mean, x = scenario, colour = colour, fill = colour)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
      geom_errorbar(aes(ymin = lci_tot, ymax = uci_tot), width = 0.2,
        linetype = "21") +
      theme_bw() +
      facet_wrap(disease ~ .) +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_y_continuous("number of deaths / stillbirths") +
      theme(legend.position = "none", panel.grid.major.x = element_blank()) +
      geom_text(aes(y = mean + mean_base, x = scenario, label = labels),
        nudge_x = 0.3, nudge_y = (df1$mean + df1$mean_base) * 0.05, size = 3.5)
 
      # neonatal and stillbirth
      df1 <- subset(df, disease != "maternal")
      plot_ns <- ggplot(data = df1,
        aes(y = mean, x = scenario, colour = colour, fill = colour)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
      geom_errorbar(aes(ymin = lci_tot, ymax = uci_tot), width = 0.2,
        linetype = "21") +
      theme_bw() +
      facet_wrap(disease ~ .) +
      scale_colour_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_fill_manual(values = palette_periods[c(1,3,4,5)]) +
      scale_y_continuous("number of deaths / stillbirths") +
      theme(legend.position = "none", panel.grid.major.x = element_blank(),
        axis.title.y = element_blank()) +
      geom_text(aes(y = mean + mean_base, x = scenario, label = labels),
        nudge_x = 0.3, nudge_y = (df1$mean + df1$mean_base) * 0.05, size = 3.5)
    
      # combine
      ggarrange(plot_m, plot_ns, nrow = 1, ncol = 2, align = "h",
        widths = c(1, 2) )
      
    # Save
    ggsave(paste(dir_path, "outputs/", "mnh_excess_deaths.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)         
    
           
  #...................................      
  ## Graph of cumulative deaths - maternal, neonatal and stillbirths

    # Fix column names
    colnames(df_cum) <- c("date", periods, "disease")
    
    # Reshape long
    df_long <- data.frame()
    for (i in periods) {
      x <- na.omit(subset(df_cum[, c("date", "disease", i)]))
      x$scenario <- i
      x$deaths <- x[, i]
      df_long <- rbind(df_long, x[, c("date", "disease", "scenario", "deaths")])
    }
    df_long$date <- as.Date(df_long$date)
    df_long$scenario <- factor(df_long$scenario, levels = periods)
    df_long <- subset(df_long, date <= date_end)
    
    # Plot
    ggplot(data = df_long) +
      geom_line(aes(x = date, y = deaths, colour = scenario,
      group = scenario), linewidth = 1) +
      geom_point(aes(x = date, y = deaths, colour = scenario,
      shape = scenario), size = 2) +
      theme_bw() +
      scale_colour_manual(values = palette_periods) +
      scale_fill_manual(values = palette_periods) +
      scale_shape_manual(values = c(15,16,17,18,19)) +
      scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
      facet_wrap(. ~ disease, scales = "free_y") +
      scale_y_continuous("cumulative deaths / stillbirths") +
      theme(legend.position = "top", legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(aes(xintercept = date_start), 
        linetype = "11", colour = "grey70", linewidth = 0.75) +
      geom_vline(aes(xintercept = date_mid), 
        linetype = "11", colour = "grey70", linewidth = 0.75) +
      annotate("text", x = date_start + (date_mid - date_start) / 2, y = 0.075, 
        label = stringr::str_wrap("projection months 1-3", 10)) +
      annotate("text", x = date_mid + (date_end - date_mid) / 2, y = 0.075, 
        label = stringr::str_wrap("projection months 4-6", 10))
 
    # Save
    ggsave(paste(dir_path, "outputs/", "mnh_cum_deaths.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 30)         
    
    
    
#...............................................................................
### ENDS
#...............................................................................

         