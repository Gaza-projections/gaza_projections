library(tidyverse)
library(ggplot2)
library(readxl)

###### Injuries Methods visualization ######


# Read data for the methods section of the injuries 
severity <- read_excel("inputs/Visualizing_Injuries_methods.xlsx")
injuries <- read_excel("inputs/Visualizing_Injuries_methods.xlsx", sheet = 3)

###### Injuries severity ######

# Rename column
severity <- rename(severity, Type = ...1)

# Create the plot
severity_figure <- ggplot(severity, aes(
  y = factor(Type, levels = c("Unknown Severity Injuries", "Mild Injuries", "Moderate Injuries", "Critical Injuries")),
  x = Injured, fill = Type
)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 5000)) +
  labs(
    title = "Cumulative Injuries by Type (reported on 12/12/23)",
    x = "",
    y = "",
    fill = "Injury Type"
  ) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#75D054FF", "#FDE725FF")) +  # Lighter red, orange, and yellow
  theme(legend.text = element_text(size = 9)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 4))

# Print the plot
print(severity_figure)

# Save the plot
ggsave("output/SeverityFigure.png", severity_figure, width = 22, height = 15, units = "cm")

###### Cumulative injuries ######

# Convert Date to Date format
injuries <- mutate(injuries, Date = as.Date(Date))

# Create a vector of lighter colors for the lines
light_green_palette <- c("#75D054FF", "#75D054FF")  
light_red_palette <- c("#440154FF", "#440154FF")      # 

# CUMULATIVE True Numbers
cumulative_injuries_deaths <- ggplot(injuries, aes(x = Date)) +
  geom_line(aes(y = `Cumulative Injured Persons`, color = "Cumulative Injuries"), linetype = "solid", size = 2) +
  geom_line(aes(y = `Cumulative death Persons`, color = "Cumulative Deaths"), linetype = "solid", size = 2) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b %Y") +
  labs(
    x = "",
    y = "",
    title = "Mortality and injuries (MoH Gaza)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",  # Move legend to the top
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10),  # Adjust legend text size (change to your desired size)
    legend.background = element_rect(fill = "white"),  # White background for the legend
    aspect.ratio = 1,  # Set aspect ratio to make it a square
    title = element_text(size = 11)  # Adjust title text size (change to your desired size)
  ) +
  scale_color_manual(
    values = c("Cumulative Injuries" = light_green_palette[2], "Cumulative Deaths" = light_red_palette[2]),
    breaks = c("Cumulative Injuries", "Cumulative Deaths")
  )

print(cumulative_injuries_deaths)

# Save the plot
ggsave("outputs/Cumulative_injuries.png", severity_figure, width = 22, height = 15, units = "cm")
