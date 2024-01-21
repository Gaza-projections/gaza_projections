#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO PROJECT GAM AND SAM PREVALENCE IN KIDS  ---------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### XXX
#...............................................................................


#......... Pre-war ...............    

# Calculate z-scores
whz_original <- addWGSR(data = data, sex = "sex", firstPart = "wt",
                        secondPart = "ht", index = "wfh")
names(whz_original)[ncol(whz_original)] <- "wfhz_original"


#......... Best ...............    

# Reduced weight by X% 1-6 
data$wta <- data$wt * 0.95
whz_a <- addWGSR(data = data, sex = "sex", firstPart = "wta",
                  secondPart = "ht", index = "wfh")
names(whz_a)[ncol(whz_a)] <- "wfhza"

# Reduced weight by X% 1-3 months
data$wta13 <- data$wt * 0.95
whz_a13 <- addWGSR(data = data, sex = "sex", firstPart = "wta13",
                 secondPart = "ht", index = "wfh")
names(whz_a13)[ncol(whz_a13)] <- "wfhza13"

# Reduced weight by X% 4-6 months
data$wta46 <- data$wt * 0.95
whz_a46 <- addWGSR(data = data, sex = "sex", firstPart = "wta46",
                   secondPart = "ht", index = "wfh")
names(whz_a46)[ncol(whz_a46)] <- "wfhza46"

#......... Middle ...............    

# Reduced weight by X% 1-6 
data$wtb <- data$wt * 0.90
whz_b <- addWGSR(data = data, sex = "sex", firstPart = "wtb",
                 secondPart = "ht", index = "wfh")
names(whz_b)[ncol(whz_b)] <- "wfhzb"

# Reduced weight by X% 1-3 months
data$wtb13 <- data$wt * 0.90
whz_b13 <- addWGSR(data = data, sex = "sex", firstPart = "wtb13",
                   secondPart = "ht", index = "wfh")
names(whz_b13)[ncol(whz_b13)] <- "wfhzb13"

# Reduced weight by X% 4-6 months
data$wtb46 <- data$wt * 0.90
whz_b46 <- addWGSR(data = data, sex = "sex", firstPart = "wtb46",
                   secondPart = "ht", index = "wfh")
names(whz_b46)[ncol(whz_b46)] <- "wfhzb46"

#......... Worst ...............    

# Reduced weight by X% 1-6 
data$wtc <- data$wt * 0.88
whz_c <- addWGSR(data = data, sex = "sex", firstPart = "wtc",
                 secondPart = "ht", index = "wfh")
names(whz_c)[ncol(whz_c)] <- "wfhzc"

# Reduced weight by X% 1-3 months
data$wtc13 <- data$wt * 0.88
whz_c13 <- addWGSR(data = data, sex = "sex", firstPart = "wtc13",
                   secondPart = "ht", index = "wfh")
names(whz_c13)[ncol(whz_c13)] <- "wfhzc13"

# Reduced weight by X% 4-6 months
data$wtc46 <- data$wt * 0.90
whz_c46 <- addWGSR(data = data, sex = "sex", firstPart = "wtc46",
                   secondPart = "ht", index = "wfh")
names(whz_c46)[ncol(whz_c46)] <- "wfhzc46"



#......... Combine the datasets and clean ...............    
# Combine the two datasets using dplyr::bind_cols
whz_combined <- dplyr::bind_cols(whz_original, whz_a, whz_b, whz_c, whz_a13, whz_b13, whz_c13, whz_a46, whz_b46, whz_c46)

# Replace impossible values
whz_combined$wfhz_original[whz_combined$wfhz_original > 5 | whz_combined$wfhz_original < -5] <- NA
whz_combined$wfhza[whz_combined$wfhza > 5 | whz_combined$wfhza < -5] <- NA
whz_combined$wfhza[whz_combined$wfhza13 > 5 | whz_combined$wfhza13 < -5] <- NA
whz_combined$wfhza[whz_combined$wfhza46 > 5 | whz_combined$wfhza46 < -5] <- NA

whz_combined$wfhzb[whz_combined$wfhzb > 5 | whz_combined$wfhzb < -5] <- NA
whz_combined$wfhza[whz_combined$wfhzb13 > 5 | whz_combined$wfhzb13 < -5] <- NA
whz_combined$wfhza[whz_combined$wfhzb46 > 5 | whz_combined$wfhzb46 < -5] <- NA

whz_combined$wfhzc[whz_combined$wfhzc > 5 | whz_combined$wfhzc < -5] <- NA
whz_combined$wfhza[whz_combined$wfhzc13 > 5 | whz_combined$wfhzc13 < -5] <- NA
whz_combined$wfhza[whz_combined$wfhzc46 > 5 | whz_combined$wfhzc46 < -5] <- NA

#......... SAM MAM ...............    

# Generate SAM_MAM pre war
whz_combined$wasting_grouping<- cut(whz_combined$wfhz_original,
                                         breaks = c(-Inf, -3, -2, Inf),
                                         labels = c("SAM", "MAM", "Normal"),
                                         include.lowest = TRUE)
# Generate SAM_MAm Best 
whz_combined$wasting_a <- cut(whz_combined$wfhza,
                              breaks = c(-Inf, -3, -2, Inf),
                              labels = c("SAM", "MAM", "Normal"),
                              include.lowest = TRUE)

whz_combined$wasting_a13 <- cut(whz_combined$wfhza13,
                              breaks = c(-Inf, -3, -2, Inf),
                              labels = c("SAM", "MAM", "Normal"),
                              include.lowest = TRUE)

whz_combined$wasting_a46 <- cut(whz_combined$wfhza46,
                              breaks = c(-Inf, -3, -2, Inf),
                              labels = c("SAM", "MAM", "Normal"),
                              include.lowest = TRUE)

# Generate SAM_MAM Middle  

whz_combined$wasting_b <- cut(whz_combined$wfhzb,
                              breaks = c(-Inf, -3, -2, Inf),
                              labels = c("SAM", "MAM", "Normal"),
                              include.lowest = TRUE)

whz_combined$wasting_b13 <- cut(whz_combined$wfhzb13,
                                breaks = c(-Inf, -3, -2, Inf),
                                labels = c("SAM", "MAM", "Normal"),
                                include.lowest = TRUE)

whz_combined$wasting_b46 <- cut(whz_combined$wfhzb46,
                                breaks = c(-Inf, -3, -2, Inf),
                                labels = c("SAM", "MAM", "Normal"),
                                include.lowest = TRUE)


# Generate SAM_MAM Worst 
whz_combined$wasting_c <- cut(whz_combined$wfhzc,
                              breaks = c(-Inf, -3, -2, Inf),
                              labels = c("SAM", "MAM", "Normal"),
                              include.lowest = TRUE)

whz_combined$wasting_c13 <- cut(whz_combined$wfhzc13,
                                breaks = c(-Inf, -3, -2, Inf),
                                labels = c("SAM", "MAM", "Normal"),
                                include.lowest = TRUE)

whz_combined$wasting_c46 <- cut(whz_combined$wfhzc46,
                                breaks = c(-Inf, -3, -2, Inf),
                                labels = c("SAM", "MAM", "Normal"),
                                include.lowest = TRUE)


# Table of prevalence for wasting original
prevalence_table_original <- whz_combined %>%
  group_by(wasting_grouping) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_grouping)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100) 

# Table of prevalence for wasting Best
prevalence_table_wasting_a <- whz_combined %>%
  group_by(wasting_a) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_a)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_a13 <- whz_combined %>%
  group_by(wasting_a13) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_a13)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_a46 <- whz_combined %>%
  group_by(wasting_a46) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_a46)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage



# Table of prevalence for wasting Middle 
prevalence_table_wasting_b <- whz_combined %>%
  group_by(wasting_b) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_b)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_b13 <- whz_combined %>%
  group_by(wasting_b13) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_b13)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_b46 <- whz_combined %>%
  group_by(wasting_b46) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_b46)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage





# Table of prevalence for wasting Worst 
prevalence_table_wasting_c <- whz_combined %>%
  group_by(wasting_c) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_c)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_c13 <- whz_combined %>%
  group_by(wasting_c13) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_c13)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage

prevalence_table_wasting_c46 <- whz_combined %>%
  group_by(wasting_c46) %>%
  summarise(Count = n()) %>%
  filter(!is.na(wasting_c46)) %>%
  mutate(Percentage = Count / sum(Count, na.rm = TRUE) * 100)  # Calculate percentage




# Display tables
print(prevalence_table_original)
print(prevalence_table_wasting_a)
print(prevalence_table_wasting_a13)
print(prevalence_table_wasting_a46)

print(prevalence_table_wasting_b)
print(prevalence_table_wasting_b13)
print(prevalence_table_wasting_b46)

print(prevalence_table_wasting_c)
print(prevalence_table_wasting_c13)
print(prevalence_table_wasting_c46)

###
sam_wasting_a <- sum(whz_combined$wasting_a == "SAM") / nrow(whz_combined) * 100
sam_wasting_b <- sum(whz_combined$wasting_b == "SAM") / nrow(whz_combined) * 100
sam_wasting_c <- sum(whz_combined$wasting_c == "SAM") / nrow(whz_combined) * 100

mam_wasting_a <- sum(whz_combined$wasting_a == "MAM") / nrow(whz_combined) * 100
mam_wasting_b <- sum(whz_combined$wasting_b == "MAM") / nrow(whz_combined) * 100
mam_wasting_c <- sum(whz_combined$wasting_c == "MAm") / nrow(whz_combined) * 100

# Best 
plot1 <- ggplot(whz_combined, aes(x = wfhz_original, fill = "WFH Original")) +
  geom_histogram(alpha = 0.5, bins = 30, aes(y = ..density..), position = "identity", color = "#69b3a2", size = 0.2) +
  geom_histogram(data = whz_combined, aes(x = wfhza, fill = "WFH a", y = ..density..),
                 alpha = 0.5, bins = 30, position = "identity", color = "#404080", size = 0.2) +
  scale_fill_manual(values = c("WFH Original" = "#69b3a2", "WFH a" = "#404080")) +
  labs(x = "Weight-for-Height z score", y = "Density", title = "Best (weight X% reduction)") +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +  # Set x-axis ticks
  coord_cartesian(ylim = c(0, 0.5)) +  # Set y-axis limit
  theme_minimal() +
  theme(
    panel.border = element_blank(),  # Remove panel border
    panel.grid = element_blank(),    # Remove gridlines
    legend.position = "top",         # Move legend to the top
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  annotate("text", x = -5, y = 0.41, label = paste("SAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_a == "SAM") / nrow(prevalence_table_wasting_a) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.44, label = paste("MAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_a == "MAM") / nrow(prevalence_table_wasting_a) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.47, label = paste("GAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_a == "GAM") / nrow(prevalence_table_wasting_a) * 100)), color = "#8B008B", size = 3, fontface = "bold", hjust = 0, vjust = 0)+
  geom_vline(xintercept = -2, linetype = "dotted", color = "black", size = 0.7)   

# Middle
plot2 <- ggplot(whz_combined, aes(x = wfhz_original, fill = "WFH Original")) +
  geom_histogram(alpha = 0.5, bins = 30, aes(y = ..density..), position = "identity", color = "#69b3a2", size = 0.2) +
  geom_histogram(data = whz_combined, aes(x = wfhzb, fill = "WFH b", y = ..density..),
                 alpha = 0.5, bins = 30, position = "identity", color = "#404080", size = 0.2) +
  scale_fill_manual(values = c("WFH Original" = "#69b3a2", "WFH b" = "#404080")) +
  labs(x = "Weight-for-Height z score", y = "Density", title = "Middle (weight X% reduction)") +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +  # Set x-axis ticks
  coord_cartesian(ylim = c(0, 0.5)) +  # Set y-axis limit
  theme_minimal() +
  theme(
    panel.border = element_blank(),  # Remove panel border
    panel.grid = element_blank(),    # Remove gridlines
    legend.position = "top",         # Move legend to the top
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  annotate("text", x = -5, y = 0.41, label = paste("SAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_b == "SAM") / nrow(prevalence_table_wasting_b) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.44, label = paste("MAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_b == "MAM") / nrow(prevalence_table_wasting_b) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.47, label = paste("GAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_b == "GAM") / nrow(prevalence_table_wasting_b) * 100)), color = "#8B008B", size = 3, fontface = "bold", hjust = 0, vjust = 0)+
  geom_vline(xintercept = -2, linetype = "dotted", color = "black", size = 0.7)   

# Worst
plot3 <- ggplot(whz_combined, aes(x = wfhz_original, fill = "WFH Original")) +
  geom_histogram(alpha = 0.5, bins = 30, aes(y = ..density..), position = "identity", color = "#69b3a2", size = 0.2) +
  geom_histogram(data = whz_combined, aes(x = wfhzc, fill = "WFH c", y = ..density..),
                 alpha = 0.5, bins = 30, position = "identity", color = "#404080", size = 0.2) +
  scale_fill_manual(values = c("WFH Original" = "#69b3a2", "WFH c" = "#404080")) +
  labs(x = "Weight-for-Height z score", y = "Density", title = "Worst (weight X% reduction)") +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) +  # Set x-axis ticks
  coord_cartesian(ylim = c(0, 0.5)) +  # Set y-axis limit
  theme_minimal() +
  theme(
    panel.border = element_blank(),  # Remove panel border
    panel.grid = element_blank(),    # Remove gridlines
    legend.position = "top",         # Move legend to the top
    legend.title = element_blank(),  # Remove legend title
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  annotate("text", x = -5, y = 0.41, label = paste("SAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_c == "SAM") / nrow(prevalence_table_wasting_c) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.44, label = paste("MAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_c == "MAM") / nrow(prevalence_table_wasting_c) * 100)), size = 3, hjust = 0, vjust = 0) +
  annotate("text", x = -5, y = 0.47, label = paste("GAM:", sprintf("%.1f%%", sum(whz_combined$prevalence_table_wasting_c == "GAM") / nrow(prevalence_table_wasting_c) * 100)), color = "#8B008B", size = 3, fontface = "bold", hjust = 0, vjust = 0)+
  geom_vline(xintercept = -2, linetype = "dotted", color = "black", size = 0.7)   

# Arrange the plots in a single row with a smaller title size
combined_plot <- plot_grid(
  plot1 + theme(legend.position="none"),
  plot2 + theme(legend.position="none"),
  plot3 + theme(legend.position="none"),
  nrow = 1,
  align = "v"  # Align plots vertically
)

# Adjust the title size in the combined plot
combined_plot <- combined_plot +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10)  # Adjust title size (e.g., size = 10)
  )

# Display the combined plot
print(combined_plot)
