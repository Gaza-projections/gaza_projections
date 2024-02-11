library(readxl)
library(viridis)
library(ggplot2)

####### Figure NCD #######

# Read the CSV file (replace with   data of the final reduction)
NCD <- read.csv("inputs/out_final_estimates.csv")
NCD_sub <- subset(NCD, module %in% c("NCDs"))

# Specific palette for the pre-war period, crisis to date period, and three scenarios
palette_gen <- viridis(16)
periods <- c("pre-war", "to date", "status quo", "escalation", "ceasefire")
palette_periods <- c("azure4", palette_gen[c(2, 8, 4, 12)])
names(palette_periods) <- periods
scenario_order <- c("ceasefire", "status quo", "escalation")

NCD_sub$scenario <- factor(NCD_sub$scenario, levels = c("pre-war", "ceasefire", "status quo", "escalation"))
# Set "pre-war" values to grey
NCD_sub$fill <- ifelse(NCD_sub$scenario == "pre-war", "azure4", as.character(factor(NCD_sub$scenario, levels = c("ceasefire", "status quo", "escalation"))))

# Create a new variable for combined diseases
NCD_sub$combined_disease <- ifelse(NCD_sub$disease %in% c("breast cancer", "lung cancer", "colorectal cancer"), "Cancer",
                                   ifelse(NCD_sub$disease %in% c("ischemic stroke", "haemorrhagic stroke"), "Stroke", NCD_sub$disease))

scenario_order <- c("ceasefire", "status quo", "escalation")
NCD_sub$scenario[NCD_sub$scenario == "baseline"] <- "pre-war"


# Relabel specific diseases within the factor levels
NCD_sub$combined_disease <- fct_collapse(NCD_sub$combined_disease,
                                         "Diabetes Type 1" = "diabetes type 1",
                                         "Ischemic Heart Disease" = "ischemic heart disease",
                                         "Chronic Kidney Disease" = "chronic kidney disease"
)
# Sum and collapse the values for maternal and neonatal by scenario
NCD_sub_summarized <- NCD_sub %>%
  group_by(scenario, combined_disease) %>%
  summarise(mean = sum(mean))


# Create a single plot with facets for the summarized data
combined_plot_summarized <- ggplot(NCD_sub_summarized, 
                                   aes(x = scenario, y = mean, fill = scenario)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = palette_periods, 
                    breaks = c("pre-war", "ceasefire", "status quo", "escalation"),
                    labels = c("Pre-War", "Ceasefire", "Status Quo", "Escalation")) +
  stat_summary(fun = function(x) sum(x), geom = "text", aes(label = sprintf("%.0f", ..y..)),
               position = position_stack(vjust = 0.5), size = 3) +  # Add sum of each column
  labs(title = "NCDs in 6 months",
       x = "Scenario",
       y = "Number of Deaths",
       fill = "Scenario") +
  facet_wrap(~combined_disease, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")  # Move legend to the top

# Print the combined plot for summarized data
print(combined_plot_summarized)

ggsave("outputs/NCD.png", combined_plot_summarized, width = 20, height = 20, units = "cm", bg = "white")
