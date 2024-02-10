library(readxl)
library(viridis)

################# Figure 1
################# NOTE to FC Figure 1 could NOT do them stacked with color difference based on scenarios. and the prewar at the bottom/ I can if we do excess and baseline with 2 colors.  I like showing the difference in scenarios colors
################# NOTE to FC I could not for Figure 1 do the y axis for stillbirth 600 so it is similar to neonatal. I dont want to do maternal 600. do we need to do something else?

################# Figure 2
################# NOTE to FC I could not export Figure 2 I can do it manually from the graph


######### MNH figure 1 ######### 

# General palette
palette_gen <- viridis(16)

# Specific palette for the pre-war period, crisis to date period, and three scenarios
periods <- c("pre-war", "to date", "status quo", "escalation", "ceasefire")
palette_periods <- c("azure4", palette_gen[c(2, 8, 4, 12)])
names(palette_periods) <- periods



# Define the order of scenarios
scenario_order <- c("ceasefire", "status quo", "escalation")

# Read the CSV file (replace with your actual data of the final reduction)

MNH <- read.csv("outputs/out_final_estimates.csv")

# Keep only rows where the disease is maternal, neonatal, or stillbirth
MNH_sub <- subset(MNH, disease %in% c("maternal", "neonatal", "stillbirth"))
MNH_sub$scenario[MNH_sub$scenario == "baseline"] <- "pre-war"

MNH_sub$scenario <- factor(MNH_sub$scenario, levels = c("pre-war", "ceasefire", "status quo", "escalation"))

# Set "pre-war" values to grey
MNH_sub$fill <- ifelse(MNH_sub$scenario == "pre-war", "azure4", as.character(factor(MNH_sub$scenario, levels = c("ceasefire", "status quo", "escalation"))))

# Manually add data for stillbirth as it was not in the reduction process
stillbirth_data <- data.frame(
  age = rep(0, 4),
  scenario = c("pre-war", "ceasefire", "status quo", "escalation"),
  subperiod = rep("1-6 months", 4),
  disease = rep("stillbirth", 4),
  mean = c(340, 380, 453, 499),  # Replace with your actual values
  lci = rep(NA, 4),              # Set CI to NA for stillbirth
  uci = rep(NA, 4),              # Set CI to NA for stillbirth
  module = rep("MNH", 4),        # Replace with your actual values
  male = rep(NA, 4),             # Replace with your actual values
  female = rep(NA, 4),           # Replace with your actual values
  total = rep(NA, 4),            # Replace with your actual values
  risk = rep(NA, 4),             # Replace with your actual values
  surv = rep(NA, 4),             # Replace with your actual values
  adj = rep(NA, 4),              # Replace with your actual values
  dr_mean = rep(NA, 4),          # Replace with your actual values
  dr_lci = rep(NA, 4),           # Replace with your actual values
  dr_uci = rep(NA, 4),           # Replace with your actual values
  fill = c("pre-war", "ceasefire", "status quo", "escalation")   # Use specific colors for scenarios
)

# Append stillbirth_data to MNH_sub
MNH_sub <- rbind(MNH_sub, stillbirth_data)

# Load the dplyr package
library(dplyr)

# Sum and collapse the values for maternal and neonatal by scenario
MNH_sub_summarized <- MNH_sub %>%
  group_by(scenario, disease) %>%
  summarise(mean = sum(mean))

# Create a single plot with facets for the summarized data
combined_plot_summarized <- ggplot(MNH_sub_summarized, 
                                   aes(x = scenario, y = mean, fill = scenario)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = palette_periods, 
                    breaks = c("pre-war", "ceasefire", "status quo", "escalation"),
                    labels = c("Pre-War", "Ceasefire", "Status Quo", "Escalation")) +
  stat_summary(fun = "sum", geom = "text", aes(label = sprintf("%.0f", ..y..)),
               position = position_stack(vjust = 0.5), size = 3) +  # Add sum of each column
  labs(title = "Maternal, Newborn in 6 months",
       x = "Scenario",
       y = "Number of Deaths",
       fill = "Scenario") +
  facet_wrap(~disease, scales = "free_y", ncol = 4, 
             labeller = labeller(disease = c("maternal" = "Maternal", "neonatal" = "Neonatal", "stillbirth" = "Stillbirth"))) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print the combined plot for summarized data
print(combined_plot_summarized)

ggsave("outputs/MNH.png", combined_plot_summarized, width = 30, height = 15, units = "cm", bg = "white")



######### MNH figure 2 ######### 
par(mfrow = c(1, 3), oma = c(5, 0, 0, 0), xpd = NA)

# general palette
palette_gen <- viridis(16)
# specific palette for the pre-war period, crisis to date period, and three scenarios
#periods <- c("pre-war", "to date",  "escalation" ,"status quo", "ceasefire")
periods <- c("pre-war", "to date", "ceasefire", "status quo", "escalation")

palette_periods <- c("azure4", palette_gen[c(2, 12, 8, 3, 4)])  # 
names(palette_periods) <- periods

# Function to plot cumulative mortality for a given sheet and title
plot_cumulative_mortality <- function(sheet, title) {
 
cumulative_mortality <- read_excel("outputs/cumulative mortality.xlsx", sheet = sheet)
  
  plot(cumulative_mortality$C, type = "l", lty = 2, col = viridis(7, option = "D", end = 0.8),
       xlab = "Months", ylab = title, xaxt = "n", lwd = 3)  # Adjust line thickness here
  
  axis(1, at = 1:12, labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep"))
  
  lines(cumulative_mortality$prewar, type = "l", col = palette_periods["pre-war"], lwd = 3)  # Change color here
  
  lines(cumulative_mortality$`Oct-Jan`, pch = 18, col = palette_periods["to date"], lwd = 3)  #
  lines(cumulative_mortality$A, pch = 18, type = "l", col = palette_periods["ceasefire"], lty = 2, lwd = 3)  # Change color here
  lines(cumulative_mortality$B, pch = 18, type = "l", col = palette_periods["status quo"], lty = 2, lwd = 3)  # Change color here
  lines(cumulative_mortality$C, pch = 18, type = "l", col = palette_periods["escalation"], lty = 2, lwd = 3)  # Change color here
  
   lines(cumulative_mortality$`Oct-Jan`, pch = 18, col = palette_periods["to date"], lwd = 3)  #
}

# Plot for Maternal Mortality
plot_cumulative_mortality("maternal", "Cumulative Maternal Mortality")

# Plot for Neonatal Mortality
plot_cumulative_mortality("neonatal", "Cumulative Neonatal Mortality")

# Plot for Stillbirths
plot_cumulative_mortality("stillbirth", "Cumulative Stillbirths")



# Legend
legend(-30,+1150, ncol = 5, legend = c("Pre-war", "War Oct-Jan", "Ceasefire", "Status quo", "Escalation"),
       col = palette_periods, lty = c(1, 1, 2, 2, 2), lwd = 3, cex = 0.9)

