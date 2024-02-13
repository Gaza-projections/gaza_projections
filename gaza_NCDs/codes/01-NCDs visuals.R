#...............................................................................
### + GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NCD ++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO VISUALISE OUTPUT AND PRODUCE GRAPHS  ------------- ##
#...............................................................................

# Francesco Checchi, Zeina Jamaluddine (Febuary 2024)

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
dir_path <- gsub("/code", "", dir_path)

# Initialise random numbers
set.seed(123)

#...............................................................................

#...................................      
## Prepare for plotting
# Set working directory to where this file is stored
NCD <- read_excel("20240212_long_modules_new.xlsx")

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


NCD <- subset(NCD, theme == "NCD")

NCD_base <- NCD[, !names(NCD) %in% c("d_excess_mean", "d_excess_lci", "d_excess_uci", "d_excess_median",
                                     "d_crisis_mean", "d_crisis_lci", "d_crisis_uci", "d_crisis_median")]

NCD_base <- transform(NCD_base,d_crisis_excess = "baseline")

NCD_base <- rename(NCD_base,
                          mean= d_base_mean,
                          lci= d_base_lci,
                          uci= d_base_uci)
                

NCD_excess <- NCD[, !names(NCD) %in% c("d_base_mean", "d_base_lci", "d_base_uci", "d_base_median",
                                     "d_crisis_mean", "d_crisis_lci", "d_crisis_uci", "d_crisis_median")]

NCD_excess <- rename(NCD_excess,
                   mean= d_excess_mean,
                   lci= d_excess_lci,
                   uci= d_excess_uci)

NCD_excess <- transform(NCD_excess,d_crisis_excess = "excess")



# Set some parameters
# scenarios
scenarios <- c("ceasefire", "status quo", "escalation")    

# dates
date_crisis <- as.Date("2023-10-07")
date_start <- as.Date("2024-02-07")
date_mid <- as.Date("2024-05-07")
date_end <- as.Date("2024-08-06")


# Assemble dataset
df1 <- bind_rows(NCD_excess, NCD_base)
df1$category <- df1$d_crisis_excess
df1$category <- gsub("d_", "", df1$category)


df1$disease[df1$disease == "breast cancer"] <- "cancer (breast,colorectal,lung)"
df1$disease[df1$disease == "colorectal cancer"] <- "cancer (breast,colorectal,lung)"
df1$disease[df1$disease == "lung cancer"] <- "cancer (breast,colorectal,lung)"

df1$disease[df1$disease == "hemorrhagic stroke"] <- "stroke (hemorrhagic,ischaemic)"
df1$disease[df1$disease == "ischaemic stroke"] <- "stroke (hemorrhagic,ischaemic)"

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

df1 <- df1[!duplicated(df1[c('scenario', 'disease', 'category', 'mean', 'colour')]), ]


plot <- ggplot(data = df1, 
               aes(y = mean, x = scenario, colour = colour, fill = colour)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +  
  geom_errorbar(aes(ymin = lci_tot, ymax = uci_tot), width = 0.2, linetype = "21") +
  theme_bw() +
  facet_wrap(disease ~ ., scales = "free_y") +
  scale_colour_manual(values = palette_periods[c(1, 3, 4, 5)]) +
  scale_fill_manual(values = palette_periods[c(1, 3, 4, 5)]) +
  scale_y_continuous("number of deaths") +
  theme(legend.position = "none", panel.grid.major.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text(data = subset(df1, !is.na(labels)),
            aes(y = mean + mean_base, x = scenario, label = labels),
            nudge_x = 0.3, nudge_y = (df1$mean[!is.na(df1$labels)] + df1$mean_base[!is.na(df1$labels)]) * 0.05, size = 3.5)

print(plot)

# Save

ggsave("/output/NCD_excess.png", plot, width = 20, height = 20, units = "cm", bg = "white")
