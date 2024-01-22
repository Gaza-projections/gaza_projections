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





# People covered by 1 MT
people_mt <- 1660

# Calories per person per day WFP: 1 MT = 1660 people for 2100 kcal
calories <- 2100

# Average MT per truck
mt_truck_14 <- 14
mt_truck_16 <- 16

# Total population
population <- 2226544

    
    
 # Calculate total calories entering Gaza for each day
trucks_data$total_kcal_14 <- trucks_data$food_truck * mt_truck_14 * people_mt * calories
trucks_data$total_kcal_16 <- trucks_data$food_truck * mt_truck_16 * people_mt * calories

# Calculate calories per person per day for each day
trucks_data$kcal_person_day_14 <- trucks_data$total_kcal_14 / population
trucks_data$kcal_person_day_16 <- trucks_data$total_kcal_16 / population

# Print the result
print(trucks_data$kcal_person_day_14)
print(trucks_data$kcal_person_day_16)

# Calculate monthly average calories
monthly_avg_kcal <- aggregate(cbind(kcal_person_day_14, kcal_person_day_16) ~ month_year, trucks_data, mean, na.rm = TRUE)

# Print the result
print(monthly_avg_kcal)




  






###### FORPLOTTING WEEKLY!!!  STILLL FIXING IT 

# Assuming you have the necessary data frames and variables defined

# Filter data from October 7, 2023, onwards
trucks_data_filtered <- subset(trucks_data, date_column >= as.Date("2023-10-07"))

# Extract continuous week information from the date starting from October 7, 2023
trucks_data_filtered$week <- as.integer(difftime(trucks_data_filtered$date_column, as.Date("2023-10-07"), units = "days") / 7) + 1

# Weekly average calories for truck type 14
weekly_avg_kcal_14 <- aggregate(kcal_person_day_14 ~ week, trucks_data_filtered, mean, na.rm = TRUE)

# Weekly average calories for truck type 16
weekly_avg_kcal_16 <- aggregate(kcal_person_day_16 ~ week, trucks_data_filtered, mean, na.rm = TRUE)

# Weekly difference between 14 MT and 16 MT
weekly_diff_kcal <- aggregate((kcal_person_day_14 - kcal_person_day_16) ~ week, trucks_data_filtered, sum, na.rm = TRUE)

# Combine data for plotting
weekly_data <- merge(weekly_avg_kcal_14, weekly_avg_kcal_16, by = "week")
weekly_data <- merge(weekly_data, weekly_diff_kcal, by = "week")

# Plot
# Plot
ggplot(data = weekly_data) +
  geom_bar(aes(x = factor(week), y = kcal_person_day_16, fill = "16 MT"), stat = "identity", color = NA) +
  geom_bar(aes(x = factor(week), y = kcal_person_day_14, fill = "14 MT"), stat = "identity", color = NA, position = "stack") +
  geom_hline(yintercept = 2213, linetype = "dotted", color = "red") +  # Adding the red dotted line
  labs(title = "Weekly Average Caloric Intake from Assistance/Aid",
       x = "Week",
       y = "Average Kcal per Person per Day") +
  scale_fill_manual(values = c("#5E4FA2", "#2E7BB4"), name = "Truck Type",
                    labels = c("16 MT ", "14 MT ")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(size = 12),
        legend.text = element_text(size = 10), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"))


