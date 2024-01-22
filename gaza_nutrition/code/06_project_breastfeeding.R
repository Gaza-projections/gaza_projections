#...............................................................................
### ++++++++++ GAZA CRISIS: HEALTH IMPACT PROJECTIONS - NUTRITION ++++++++++ ###
#...............................................................................

#...............................................................................
## --------- R SCRIPT TO PROJECT EXCLUSIVE BREASTFEEDING PREVALENCE  -------- ##
#...............................................................................

                          # LSHTM (January 2024)
                          # francesco.checchi@lshtm_ac.uk 


#...............................................................................  
### Preparing necessary objects
#...............................................................................

# Data
countries <- c("Iraq (war)", "Lebanon (war)", "Lebanon (explosion)", "Syria (war)", "Syria refugees (in lebanon)")
pre_crisis <- c(NA, 48, 59.6, 42.6, 42.6)
crisis <- c(NA, 9, 7.7, 31.8, 25)
difference <- c(7, 39.0, 51.9, 10.9, 17.6)

# Calculate average difference
average_difference <- mean(difference, na.rm = TRUE)

# Confidence interval
conf_interval <- t.test(difference, na.rm = TRUE)$conf.int

# Apply to prewar level of 41.6 EBF in Gaza
EBF_current <- 41.6 - (41.6 * (average_difference / 100))
EBF_crisis_interval <- c(EBF_current - (EBF_current * (conf_interval[2] / 100)),
                         EBF_current - (EBF_current * (conf_interval[1] / 100)))

# Output
cat("Average Difference:", round(average_difference, 2), "\n")
cat("Predicted Crisis Level for Prewar 41.6 (% decrease):", round(EBF_current, 2), "\n")
cat("Predicted Crisis Interval for Prewar 41.6 (% decrease):", round(EBF_crisis_interval[1], 2), "to", round(EBF_crisis_interval[2], 2), "\n")

# currently 25-30% exclusively breastfed
# best case 25-30% exclusively breastfed
# worst case 15-25% exclusively breastfed

