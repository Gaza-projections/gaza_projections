# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(viridis)

# Set color palette
palette_gen <- viridis(16)
palette <- c("#2A788EFF", "#1F988BFF", "red")

# Read the data
trucks <- read_excel("inputs/trucks.xlsx")

# Data transformation
trucks_long <- pivot_longer(trucks, cols = c(`cum_food_truck`, `cum_150`, `cum_180`),
                            names_to = "truck_type", values_to = "cumulative_sum")

trucks_long <- mutate(trucks_long, date = as.Date(date))

# Line Plot
cumulative_truck_entry <- ggplot(trucks_long, aes(color = truck_type, y = cumulative_sum, x = date)) +
  geom_line(size = 1.5) +  # Increase line thickness
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b %Y") +
  labs(x = "Date", y = "", title = "Cumulative Trucks",
       colour = "") +
  scale_colour_manual(values = palette,
                      breaks = c("cum_180", "cum_150", "cum_food_truck"),  # Change the order here
                      labels = c("180 trucks/day", "150 trucks/day", "Food Trucks 7Oct-31 Jan")) +
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust the angle and alignment of x-axis labels
    axis.title = element_text(size = 11),  # Increase the size of axis titles
    plot.title = element_text(size = 14, face = "bold"),  # Increase the size and boldness of the plot title
    aspect.ratio = 1,  # Set aspect ratio to make it a square
    panel.grid.major = element_line(color = "grey80"),  # Add grid lines for better readability
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a border around the plot
    legend.position = "right", 
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

print(cumulative_truck_entry)

ggsave(paste(dir_path, "outputs/trucks.png", sep = ""),
       dpi = "print", units = "cm", height = 15, width = 22, bg = "white")
