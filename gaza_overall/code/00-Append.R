######################### Append all datasets  #########################

# Load required libraries
library(readxl)
library(dplyr)
library(writexl)
library(tidyr)

setwd("C:/Users/zeina/London School of Hygiene and Tropical Medicine/Gaza Public Health Projections_Group - Documents/General/public health projections/04 - projections - in progress/11_02_2024_outputs")

#...............................................................................
######## INFECTIONS ########
#...............................................................................


####  Epidemic #### 
epidemic <- read.csv("out_tab_epid_all.csv")

epidemic <- rename(epidemic,
                  d_excess_mean= deaths_mean,
                  d_excess_median= deaths_median,
                  d_excess_lci= deaths_lci,
                  d_excess_uci=deaths_uci)


epidemic <- mutate(epidemic,
                      theme = "infection")

#### Endemic baseline #### 
endemic <- read.csv("out_tab_ende_all.csv")


endemic <- mutate(endemic,
                   theme = "infection")

# Append the two datasets
combined_infection  <- bind_rows(endemic, epidemic)

#...............................................................................
######## INJURIES ########
#...............................................................................


####  1-3 months#### 

injuries1_3 <- read_excel("Deaths_total_with_uncounted.xlsx", sheet = "Excess Death Age M1-M3")
injuries1_3 <- subset(injuries1_3, age != "Total")


# Create additional columns
injuries1_3 <- mutate(injuries1_3,
                      subperiod = "months 1 to 3",
                      theme = "injuries",
                      disease = "trauma")

# Rename columns
injuries1_3 <- rename(injuries1_3,
                      d_excess_mean_ceasefire = Ceasefire,
                      d_excess_lci_ceasefire = `Ceasefire lower CI`,
                      d_excess_uci_ceasefire = `Ceasefire upper CI`,
                      d_excess_mean_statusquo = `Status Quo`,
                      d_excess_lci_statusquo = `Status Quo lower CI`,
                      d_excess_uci_statusquo = `Status Quo upper CI`,
                      d_excess_mean_escalation = Escalation,
                      d_excess_lci_escalation = `Escalation lower CI`,
                      d_excess_uci_escalation = `Escalation upper CI`)

# Reshape to long format
injuries1_3_long <- pivot_longer(injuries1_3,
                                 cols = c("d_excess_mean_ceasefire", "d_excess_lci_ceasefire", "d_excess_uci_ceasefire",
                                          "d_excess_mean_statusquo", "d_excess_lci_statusquo", "d_excess_uci_statusquo",
                                          "d_excess_mean_escalation", "d_excess_lci_escalation", "d_excess_uci_escalation"), 
                                 names_to = c(".value", "scenario"), 
                                 names_pattern = "(d_excess_mean|d_excess_lci|d_excess_uci)_(.+)",
                                 values_to = "Value")



####  4-6 months#### 

injuries4_6 <- read_excel("Deaths_total_with_uncounted.xlsx", sheet = "Excess Death Age M4-M6")
injuries4_6 <- subset(injuries4_6, age != "Total")


# Create additional columns
injuries4_6 <- mutate(injuries4_6,
                      subperiod = "months 4 to 6",
                      theme = "injuries",
                      disease = "trauma")

# Rename columns
injuries4_6 <- rename(injuries4_6,
                      d_excess_mean_ceasefire = Ceasefire,
                      d_excess_lci_ceasefire = `Ceasefire lower CI`,
                      d_excess_uci_ceasefire = `Ceasefire upper CI`,
                      d_excess_mean_statusquo = `Status Quo`,
                      d_excess_lci_statusquo = `Status Quo lower CI`,
                      d_excess_uci_statusquo = `Status Quo upper CI`,
                      d_excess_mean_escalation = Escalation,
                      d_excess_lci_escalation = `Escalation lower CI`,
                      d_excess_uci_escalation = `Escalation upper CI`)

# Reshape to long format
injuries4_6_long <- pivot_longer(injuries4_6,
                                 cols = c("d_excess_mean_ceasefire", "d_excess_lci_ceasefire", "d_excess_uci_ceasefire",
                                          "d_excess_mean_statusquo", "d_excess_lci_statusquo", "d_excess_uci_statusquo",
                                          "d_excess_mean_escalation", "d_excess_lci_escalation", "d_excess_uci_escalation"), 
                                 names_to = c(".value", "scenario"), 
                                 names_pattern = "(d_excess_mean|d_excess_lci|d_excess_uci)_(.+)",
                                 values_to = "Value")


# Append the two datasets
combined_injuries  <- bind_rows(injuries1_3_long, injuries4_6_long)


#...............................................................................
######## MNH ########
#...............................................................................

combined_MNH <- read_excel("MNH.xlsx",sheet = "wide")



#...............................................................................
############ NCD ############
#...............................................................................

####  Bcancer #### 
#... excess 
excess_Bcancer <- read_excel("Bcancer/excess_death_13-46M_by_age_.xlsx")
excess_Bcancer <- subset(excess_Bcancer, Age != "Total")
names(excess_Bcancer)[names(excess_Bcancer) == "Age"] <- "age"
excess_Bcancer <- excess_Bcancer[, !names(excess_Bcancer) %in% c("BcancerCeasefire_All_mean", "BcancerCeasefire_All_lb", "BcancerCeasefire_All_ub",
                                                                 "BcancerStatus Quo_All_mean", "BcancerStatus Quo_All_lb", "BcancerStatus Quo_All_ub",
                                                                  "BcancerEscalation_All_mean", "BcancerEscalation_All_lb", "BcancerEscalation_All_ub")]
# Rename columns
excess_Bcancer <- rename(excess_Bcancer,  
                         d_excess_mean_ceasefire_1to3= `BcancerCeasefire_Month1-3_mean`, 
                         d_excess_lci_ceasefire_1to3= `BcancerCeasefire_Month1-3_lb`, 
                         d_excess_uci_ceasefire_1to3= `BcancerCeasefire_Month1-3_ub`, 
                         d_excess_mean_ceasefire_4to6= `BcancerCeasefire_Month4-6_mean`, 
                         d_excess_lci_ceasefire_4to6= `BcancerCeasefire_Month4-6_lb`, 
                         d_excess_uci_ceasefire_4to6= `BcancerCeasefire_Month4-6_ub`,
                         
                         d_excess_mean_statusquo_1to3= `BcancerStatus Quo_Month1-3_mean`, 
                         d_excess_lci_statusquo_1to3= `BcancerStatus Quo_Month1-3_lb`, 
                         d_excess_uci_statusquo_1to3= `BcancerStatus Quo_Month1-3_ub`, 
                         d_excess_mean_statusquo_4to6= `BcancerStatus Quo_Month4-6_mean`, 
                         d_excess_lci_statusquo_4to6= `BcancerStatus Quo_Month4-6_lb`, 
                         d_excess_uci_statusquo_4to6= `BcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_excess_mean_escalation_1to3= `BcancerEscalation_Month1-3_mean`, 
                         d_excess_lci_escalation_1to3= `BcancerEscalation_Month1-3_lb`, 
                         d_excess_uci_escalation_1to3= `BcancerEscalation_Month1-3_ub`, 
                         d_excess_mean_escalation_4to6= `BcancerEscalation_Month4-6_mean`, 
                         d_excess_lci_escalation_4to6= `BcancerEscalation_Month4-6_lb`, 
                         d_excess_uci_escalation_4to6= `BcancerEscalation_Month4-6_ub`)


# Reshape to long format
excess_Bcancer_long <- pivot_longer(excess_Bcancer,
                             cols = c(
                               starts_with("d_excess")
                             ),
                             names_to = c(".value", "scenario", "subperiod"),
                             names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                             values_to = "Value")

excess_Bcancer_long <- transform(excess_Bcancer_long,
                          theme = "NCD",
                          disease = "Bcancer")

excess_Bcancer_long <- rename(excess_Bcancer_long,
                    d_excess_mean= mean,
                    d_excess_lci= lci,
                    d_excess_uci= uci)

#....crisis 
crisis_Bcancer <- read_excel("Bcancer/scenario_total_13-46M_by_age_.xlsx")
crisis_Bcancer <- subset(crisis_Bcancer, Age != "Total")

names(crisis_Bcancer)[names(crisis_Bcancer) == "Age"] <- "age"

variable_names <- names(crisis_Bcancer)
print(variable_names)

crisis_Bcancer <- crisis_Bcancer[, !names(crisis_Bcancer) %in% c("BcancerCeasefire_All_mean", "BcancerCeasefire_All_lb", "BcancerCeasefire_All_ub",
                                                                 "BcancerStatus Quo_All_mean", "BcancerStatus Quo_All_lb", "BcancerStatus Quo_All_ub",
                                                                 "BcancerEscalation_All_mean", "BcancerEscalation_All_lb", "BcancerEscalation_All_ub")]

# Rename columns
crisis_Bcancer <- rename(crisis_Bcancer,  
                         d_crisis_mean_ceasefire_1to3= `BcancerCeasefire_Month1-3_mean`, 
                         d_crisis_lci_ceasefire_1to3= `BcancerCeasefire_Month1-3_lb`, 
                         d_crisis_uci_ceasefire_1to3= `BcancerCeasefire_Month1-3_ub`, 
                         d_crisis_mean_ceasefire_4to6= `BcancerCeasefire_Month4-6_mean`, 
                         d_crisis_lci_ceasefire_4to6= `BcancerCeasefire_Month4-6_lb`, 
                         d_crisis_uci_ceasefire_4to6= `BcancerCeasefire_Month4-6_ub`,
                         
                         d_crisis_mean_statusquo_1to3= `BcancerStatus Quo_Month1-3_mean`, 
                         d_crisis_lci_statusquo_1to3= `BcancerStatus Quo_Month1-3_lb`, 
                         d_crisis_uci_statusquo_1to3= `BcancerStatus Quo_Month1-3_ub`, 
                         d_crisis_mean_statusquo_4to6= `BcancerStatus Quo_Month4-6_mean`, 
                         d_crisis_lci_statusquo_4to6= `BcancerStatus Quo_Month4-6_lb`, 
                         d_crisis_uci_statusquo_4to6= `BcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_crisis_mean_escalation_1to3= `BcancerEscalation_Month1-3_mean`, 
                         d_crisis_lci_escalation_1to3= `BcancerEscalation_Month1-3_lb`, 
                         d_crisis_uci_escalation_1to3= `BcancerEscalation_Month1-3_ub`, 
                         d_crisis_mean_escalation_4to6= `BcancerEscalation_Month4-6_mean`, 
                         d_crisis_lci_escalation_4to6= `BcancerEscalation_Month4-6_lb`, 
                         d_crisis_uci_escalation_4to6= `BcancerEscalation_Month4-6_ub`)


# Reshape to long format
crisis_Bcancer_long <- pivot_longer(crisis_Bcancer,
                                    cols = c(
                                      starts_with("d_crisis")
                                    ),
                                    names_to = c(".value", "scenario", "subperiod"),
                                    names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                    values_to = "Value")


crisis_Bcancer_long <- rename(crisis_Bcancer_long,
                              d_crisis_mean= mean,
                              d_crisis_lci= lci,
                              d_crisis_uci= uci)


#....baseline 
baseline_Bcancer <- read_excel("Bcancer/baseline_total_13-46M_by_age_.xlsx")
baseline_Bcancer <- subset(baseline_Bcancer, Age != "Total")

names(baseline_Bcancer)[names(baseline_Bcancer) == "Age"] <- "age"


baseline_Bcancer <- baseline_Bcancer[, !names(baseline_Bcancer) %in% c("Bcancer_All_mean", "Bcancer_All_lb", "Bcancer_All_ub")]
                                                                 
# Rename columns
baseline_Bcancer <- rename(baseline_Bcancer,  
                         d_base_mean_1to3= `Bcancer_Month1-3_mean`, 
                         d_base_mean_4to6= `Bcancer_Month4-6_mean`, 
                         d_base_lci_1to3= `Bcancer_Month1-3_lb`, 
                         d_base_lci_4to6= `Bcancer_Month4-6_lb`, 
                         d_base_uci_1to3= `Bcancer_Month1-3_ub`, 
                         d_base_uci_4to6= `Bcancer_Month4-6_ub`) 
                         
baseline_Bcancer_long <- pivot_longer(baseline_Bcancer,
                                    cols = c(
                                      starts_with("d_base")
                                    ),
                                    names_to = c(".value", "subperiod"),
                                    names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                    values_to = "Value")                   

baseline_Bcancer_long <- rename(baseline_Bcancer_long,
                              d_base_mean= mean,
                              d_base_lci= lci,
                              d_base_uci= uci)


Bcancer_merged_data <- merge(excess_Bcancer_long, crisis_Bcancer_long, by = c("age", "scenario", "subperiod"))
Bcancer <- merge(Bcancer_merged_data, baseline_Bcancer_long, by = c("age", "subperiod"))
Bcancer <- Bcancer[, !names(Bcancer) %in% c("...1.x", "...1.y", "...1")]


####  Lcancer #### 
#... excess 
excess_Lcancer <- read_excel("Lcancer/excess_death_13-46M_by_age_.xlsx")
excess_Lcancer <- subset(excess_Lcancer, Age != "Total")
names(excess_Lcancer)[names(excess_Lcancer) == "Age"] <- "age"
excess_Lcancer <- excess_Lcancer[, !names(excess_Lcancer) %in% c("LcancerCeasefire_All_mean", "LcancerCeasefire_All_lb", "LcancerCeasefire_All_ub",
                                                                 "LcancerStatus Quo_All_mean", "LcancerStatus Quo_All_lb", "LcancerStatus Quo_All_ub",
                                                                 "LcancerEscalation_All_mean", "LcancerEscalation_All_lb", "LcancerEscalation_All_ub")]
# Rename columns
excess_Lcancer <- rename(excess_Lcancer,  
                         d_excess_mean_ceasefire_1to3= `LcancerCeasefire_Month1-3_mean`, 
                         d_excess_lci_ceasefire_1to3= `LcancerCeasefire_Month1-3_lb`, 
                         d_excess_uci_ceasefire_1to3= `LcancerCeasefire_Month1-3_ub`, 
                         d_excess_mean_ceasefire_4to6= `LcancerCeasefire_Month4-6_mean`, 
                         d_excess_lci_ceasefire_4to6= `LcancerCeasefire_Month4-6_lb`, 
                         d_excess_uci_ceasefire_4to6= `LcancerCeasefire_Month4-6_ub`,
                         
                         d_excess_mean_statusquo_1to3= `LcancerStatus Quo_Month1-3_mean`, 
                         d_excess_lci_statusquo_1to3= `LcancerStatus Quo_Month1-3_lb`, 
                         d_excess_uci_statusquo_1to3= `LcancerStatus Quo_Month1-3_ub`, 
                         d_excess_mean_statusquo_4to6= `LcancerStatus Quo_Month4-6_mean`, 
                         d_excess_lci_statusquo_4to6= `LcancerStatus Quo_Month4-6_lb`, 
                         d_excess_uci_statusquo_4to6= `LcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_excess_mean_escalation_1to3= `LcancerEscalation_Month1-3_mean`, 
                         d_excess_lci_escalation_1to3= `LcancerEscalation_Month1-3_lb`, 
                         d_excess_uci_escalation_1to3= `LcancerEscalation_Month1-3_ub`, 
                         d_excess_mean_escalation_4to6= `LcancerEscalation_Month4-6_mean`, 
                         d_excess_lci_escalation_4to6= `LcancerEscalation_Month4-6_lb`, 
                         d_excess_uci_escalation_4to6= `LcancerEscalation_Month4-6_ub`)


# Reshape to long format
excess_Lcancer_long <- pivot_longer(excess_Lcancer,
                                    cols = c(
                                      starts_with("d_excess")
                                    ),
                                    names_to = c(".value", "scenario", "subperiod"),
                                    names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                                    values_to = "Value")

excess_Lcancer_long <- transform(excess_Lcancer_long,
                                 theme = "NCD",
                                 disease = "Lcancer")

excess_Lcancer_long <- rename(excess_Lcancer_long,
                              d_excess_mean= mean,
                              d_excess_lci= lci,
                              d_excess_uci= uci)

#....crisis 
crisis_Lcancer <- read_excel("Lcancer/scenario_total_13-46M_by_age_.xlsx")
crisis_Lcancer <- subset(crisis_Lcancer, Age != "Total")

names(crisis_Lcancer)[names(crisis_Lcancer) == "Age"] <- "age"

variable_names <- names(crisis_Lcancer)
print(variable_names)

crisis_Lcancer <- crisis_Lcancer[, !names(crisis_Lcancer) %in% c("LcancerCeasefire_All_mean", "LcancerCeasefire_All_lb", "LcancerCeasefire_All_ub",
                                                                 "LcancerStatus Quo_All_mean", "LcancerStatus Quo_All_lb", "LcancerStatus Quo_All_ub",
                                                                 "LcancerEscalation_All_mean", "LcancerEscalation_All_lb", "LcancerEscalation_All_ub")]

# Rename columns
crisis_Lcancer <- rename(crisis_Lcancer,  
                         d_crisis_mean_ceasefire_1to3= `LcancerCeasefire_Month1-3_mean`, 
                         d_crisis_lci_ceasefire_1to3= `LcancerCeasefire_Month1-3_lb`, 
                         d_crisis_uci_ceasefire_1to3= `LcancerCeasefire_Month1-3_ub`, 
                         d_crisis_mean_ceasefire_4to6= `LcancerCeasefire_Month4-6_mean`, 
                         d_crisis_lci_ceasefire_4to6= `LcancerCeasefire_Month4-6_lb`, 
                         d_crisis_uci_ceasefire_4to6= `LcancerCeasefire_Month4-6_ub`,
                         
                         d_crisis_mean_statusquo_1to3= `LcancerStatus Quo_Month1-3_mean`, 
                         d_crisis_lci_statusquo_1to3= `LcancerStatus Quo_Month1-3_lb`, 
                         d_crisis_uci_statusquo_1to3= `LcancerStatus Quo_Month1-3_ub`, 
                         d_crisis_mean_statusquo_4to6= `LcancerStatus Quo_Month4-6_mean`, 
                         d_crisis_lci_statusquo_4to6= `LcancerStatus Quo_Month4-6_lb`, 
                         d_crisis_uci_statusquo_4to6= `LcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_crisis_mean_escalation_1to3= `LcancerEscalation_Month1-3_mean`, 
                         d_crisis_lci_escalation_1to3= `LcancerEscalation_Month1-3_lb`, 
                         d_crisis_uci_escalation_1to3= `LcancerEscalation_Month1-3_ub`, 
                         d_crisis_mean_escalation_4to6= `LcancerEscalation_Month4-6_mean`, 
                         d_crisis_lci_escalation_4to6= `LcancerEscalation_Month4-6_lb`, 
                         d_crisis_uci_escalation_4to6= `LcancerEscalation_Month4-6_ub`)


# Reshape to long format
crisis_Lcancer_long <- pivot_longer(crisis_Lcancer,
                                    cols = c(
                                      starts_with("d_crisis")
                                    ),
                                    names_to = c(".value", "scenario", "subperiod"),
                                    names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                    values_to = "Value")


crisis_Lcancer_long <- rename(crisis_Lcancer_long,
                              d_crisis_mean= mean,
                              d_crisis_lci= lci,
                              d_crisis_uci= uci)


#....baseline 
baseline_Lcancer <- read_excel("Lcancer/baseline_total_13-46M_by_age_.xlsx")
baseline_Lcancer <- subset(baseline_Lcancer, Age != "Total")

names(baseline_Lcancer)[names(baseline_Lcancer) == "Age"] <- "age"


baseline_Lcancer <- baseline_Lcancer[, !names(baseline_Lcancer) %in% c("Lcancer_All_mean", "Lcancer_All_lb", "Lcancer_All_ub")]

# Rename columns
baseline_Lcancer <- rename(baseline_Lcancer,  
                           d_base_mean_1to3= `Lcancer_Month1-3_mean`, 
                           d_base_mean_4to6= `Lcancer_Month4-6_mean`, 
                           d_base_lci_1to3= `Lcancer_Month1-3_lb`, 
                           d_base_lci_4to6= `Lcancer_Month4-6_lb`, 
                           d_base_uci_1to3= `Lcancer_Month1-3_ub`, 
                           d_base_uci_4to6= `Lcancer_Month4-6_ub`) 

baseline_Lcancer_long <- pivot_longer(baseline_Lcancer,
                                      cols = c(
                                        starts_with("d_base")
                                      ),
                                      names_to = c(".value", "subperiod"),
                                      names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                      values_to = "Value")                   

baseline_Lcancer_long <- rename(baseline_Lcancer_long,
                                d_base_mean= mean,
                                d_base_lci= lci,
                                d_base_uci= uci)


Lcancer_merged_data <- merge(excess_Lcancer_long, crisis_Lcancer_long, by = c("age", "scenario", "subperiod"))
Lcancer <- merge(Lcancer_merged_data, baseline_Lcancer_long, by = c("age", "subperiod"))
Lcancer <- Lcancer[, !names(Lcancer) %in% c("...1.x", "...1.y", "...1")]

####  Ccancer #### 
#... excess 
excess_Ccancer <- read_excel("Ccancer/excess_death_13-46M_by_age_.xlsx")
excess_Ccancer <- subset(excess_Ccancer, Age != "Total")
names(excess_Ccancer)[names(excess_Ccancer) == "Age"] <- "age"
excess_Ccancer <- excess_Ccancer[, !names(excess_Ccancer) %in% c("CcancerCeasefire_All_mean", "CcancerCeasefire_All_lb", "CcancerCeasefire_All_ub",
                                                                 "CcancerStatus Quo_All_mean", "CcancerStatus Quo_All_lb", "CcancerStatus Quo_All_ub",
                                                                 "CcancerEscalation_All_mean", "CcancerEscalation_All_lb", "CcancerEscalation_All_ub")]
# Rename columns
excess_Ccancer <- rename(excess_Ccancer,  
                         d_excess_mean_ceasefire_1to3= `CcancerCeasefire_Month1-3_mean`, 
                         d_excess_lci_ceasefire_1to3= `CcancerCeasefire_Month1-3_lb`, 
                         d_excess_uci_ceasefire_1to3= `CcancerCeasefire_Month1-3_ub`, 
                         d_excess_mean_ceasefire_4to6= `CcancerCeasefire_Month4-6_mean`, 
                         d_excess_lci_ceasefire_4to6= `CcancerCeasefire_Month4-6_lb`, 
                         d_excess_uci_ceasefire_4to6= `CcancerCeasefire_Month4-6_ub`,
                         
                         d_excess_mean_statusquo_1to3= `CcancerStatus Quo_Month1-3_mean`, 
                         d_excess_lci_statusquo_1to3= `CcancerStatus Quo_Month1-3_lb`, 
                         d_excess_uci_statusquo_1to3= `CcancerStatus Quo_Month1-3_ub`, 
                         d_excess_mean_statusquo_4to6= `CcancerStatus Quo_Month4-6_mean`, 
                         d_excess_lci_statusquo_4to6= `CcancerStatus Quo_Month4-6_lb`, 
                         d_excess_uci_statusquo_4to6= `CcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_excess_mean_escalation_1to3= `CcancerEscalation_Month1-3_mean`, 
                         d_excess_lci_escalation_1to3= `CcancerEscalation_Month1-3_lb`, 
                         d_excess_uci_escalation_1to3= `CcancerEscalation_Month1-3_ub`, 
                         d_excess_mean_escalation_4to6= `CcancerEscalation_Month4-6_mean`, 
                         d_excess_lci_escalation_4to6= `CcancerEscalation_Month4-6_lb`, 
                         d_excess_uci_escalation_4to6= `CcancerEscalation_Month4-6_ub`)


# Reshape to long format
excess_Ccancer_long <- pivot_longer(excess_Ccancer,
                                    cols = c(
                                      starts_with("d_excess")
                                    ),
                                    names_to = c(".value", "scenario", "subperiod"),
                                    names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                                    values_to = "Value")

excess_Ccancer_long <- transform(excess_Ccancer_long,
                                 theme = "NCD",
                                 disease = "Ccancer")

excess_Ccancer_long <- rename(excess_Ccancer_long,
                              d_excess_mean= mean,
                              d_excess_lci= lci,
                              d_excess_uci= uci)

#....crisis 
crisis_Ccancer <- read_excel("Ccancer/scenario_total_13-46M_by_age_.xlsx")
crisis_Ccancer <- subset(crisis_Ccancer, Age != "Total")

names(crisis_Ccancer)[names(crisis_Ccancer) == "Age"] <- "age"

variable_names <- names(crisis_Ccancer)
print(variable_names)

crisis_Ccancer <- crisis_Ccancer[, !names(crisis_Ccancer) %in% c("CcancerCeasefire_All_mean", "CcancerCeasefire_All_lb", "CcancerCeasefire_All_ub",
                                                                 "CcancerStatus Quo_All_mean", "CcancerStatus Quo_All_lb", "CcancerStatus Quo_All_ub",
                                                                 "CcancerEscalation_All_mean", "CcancerEscalation_All_lb", "CcancerEscalation_All_ub")]

# Rename columns
crisis_Ccancer <- rename(crisis_Ccancer,  
                         d_crisis_mean_ceasefire_1to3= `CcancerCeasefire_Month1-3_mean`, 
                         d_crisis_lci_ceasefire_1to3= `CcancerCeasefire_Month1-3_lb`, 
                         d_crisis_uci_ceasefire_1to3= `CcancerCeasefire_Month1-3_ub`, 
                         d_crisis_mean_ceasefire_4to6= `CcancerCeasefire_Month4-6_mean`, 
                         d_crisis_lci_ceasefire_4to6= `CcancerCeasefire_Month4-6_lb`, 
                         d_crisis_uci_ceasefire_4to6= `CcancerCeasefire_Month4-6_ub`,
                         
                         d_crisis_mean_statusquo_1to3= `CcancerStatus Quo_Month1-3_mean`, 
                         d_crisis_lci_statusquo_1to3= `CcancerStatus Quo_Month1-3_lb`, 
                         d_crisis_uci_statusquo_1to3= `CcancerStatus Quo_Month1-3_ub`, 
                         d_crisis_mean_statusquo_4to6= `CcancerStatus Quo_Month4-6_mean`, 
                         d_crisis_lci_statusquo_4to6= `CcancerStatus Quo_Month4-6_lb`, 
                         d_crisis_uci_statusquo_4to6= `CcancerStatus Quo_Month4-6_ub`,
                         
                         
                         d_crisis_mean_escalation_1to3= `CcancerEscalation_Month1-3_mean`, 
                         d_crisis_lci_escalation_1to3= `CcancerEscalation_Month1-3_lb`, 
                         d_crisis_uci_escalation_1to3= `CcancerEscalation_Month1-3_ub`, 
                         d_crisis_mean_escalation_4to6= `CcancerEscalation_Month4-6_mean`, 
                         d_crisis_lci_escalation_4to6= `CcancerEscalation_Month4-6_lb`, 
                         d_crisis_uci_escalation_4to6= `CcancerEscalation_Month4-6_ub`)


# Reshape to long format
crisis_Ccancer_long <- pivot_longer(crisis_Ccancer,
                                    cols = c(
                                      starts_with("d_crisis")
                                    ),
                                    names_to = c(".value", "scenario", "subperiod"),
                                    names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                    values_to = "Value")


crisis_Ccancer_long <- rename(crisis_Ccancer_long,
                              d_crisis_mean= mean,
                              d_crisis_lci= lci,
                              d_crisis_uci= uci)


#....baseline 
baseline_Ccancer <- read_excel("Ccancer/baseline_total_13-46M_by_age_.xlsx")
baseline_Ccancer <- subset(baseline_Ccancer, Age != "Total")

names(baseline_Ccancer)[names(baseline_Ccancer) == "Age"] <- "age"


baseline_Ccancer <- baseline_Ccancer[, !names(baseline_Ccancer) %in% c("Ccancer_All_mean", "Ccancer_All_lb", "Ccancer_All_ub")]

# Rename columns
baseline_Ccancer <- rename(baseline_Ccancer,  
                           d_base_mean_1to3= `Ccancer_Month1-3_mean`, 
                           d_base_mean_4to6= `Ccancer_Month4-6_mean`, 
                           d_base_lci_1to3= `Ccancer_Month1-3_lb`, 
                           d_base_lci_4to6= `Ccancer_Month4-6_lb`, 
                           d_base_uci_1to3= `Ccancer_Month1-3_ub`, 
                           d_base_uci_4to6= `Ccancer_Month4-6_ub`) 

baseline_Ccancer_long <- pivot_longer(baseline_Ccancer,
                                      cols = c(
                                        starts_with("d_base")
                                      ),
                                      names_to = c(".value", "subperiod"),
                                      names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                      values_to = "Value")                   

baseline_Ccancer_long <- rename(baseline_Ccancer_long,
                                d_base_mean= mean,
                                d_base_lci= lci,
                                d_base_uci= uci)


Ccancer_merged_data <- merge(excess_Ccancer_long, crisis_Ccancer_long, by = c("age", "scenario", "subperiod"))
Ccancer <- merge(Ccancer_merged_data, baseline_Ccancer_long, by = c("age", "subperiod"))
Ccancer <- Ccancer[, !names(Ccancer) %in% c("...1.x", "...1.y", "...1")]

####  CKD #### 
#... excess 
excess_CKD <- read_excel("CKD/excess_death_13-46M_by_age_.xlsx")
excess_CKD <- subset(excess_CKD, Age != "Total")
names(excess_CKD)[names(excess_CKD) == "Age"] <- "age"
excess_CKD <- excess_CKD[, !names(excess_CKD) %in% c("CKDCeasefire_All_mean", "CKDCeasefire_All_lb", "CKDCeasefire_All_ub",
                                                     "CKDStatus Quo_All_mean", "CKDStatus Quo_All_lb", "CKDStatus Quo_All_ub",
                                                     "CKDEscalation_All_mean", "CKDEscalation_All_lb", "CKDEscalation_All_ub")]
# Rename columns
excess_CKD <- rename(excess_CKD,  
                     d_excess_mean_ceasefire_1to3= `CKDCeasefire_Month1-3_mean`, 
                     d_excess_lci_ceasefire_1to3= `CKDCeasefire_Month1-3_lb`, 
                     d_excess_uci_ceasefire_1to3= `CKDCeasefire_Month1-3_ub`, 
                     d_excess_mean_ceasefire_4to6= `CKDCeasefire_Month4-6_mean`, 
                     d_excess_lci_ceasefire_4to6= `CKDCeasefire_Month4-6_lb`, 
                     d_excess_uci_ceasefire_4to6= `CKDCeasefire_Month4-6_ub`,
                     
                     d_excess_mean_statusquo_1to3= `CKDStatus Quo_Month1-3_mean`, 
                     d_excess_lci_statusquo_1to3= `CKDStatus Quo_Month1-3_lb`, 
                     d_excess_uci_statusquo_1to3= `CKDStatus Quo_Month1-3_ub`, 
                     d_excess_mean_statusquo_4to6= `CKDStatus Quo_Month4-6_mean`, 
                     d_excess_lci_statusquo_4to6= `CKDStatus Quo_Month4-6_lb`, 
                     d_excess_uci_statusquo_4to6= `CKDStatus Quo_Month4-6_ub`,
                     
                     
                     d_excess_mean_escalation_1to3= `CKDEscalation_Month1-3_mean`, 
                     d_excess_lci_escalation_1to3= `CKDEscalation_Month1-3_lb`, 
                     d_excess_uci_escalation_1to3= `CKDEscalation_Month1-3_ub`, 
                     d_excess_mean_escalation_4to6= `CKDEscalation_Month4-6_mean`, 
                     d_excess_lci_escalation_4to6= `CKDEscalation_Month4-6_lb`, 
                     d_excess_uci_escalation_4to6= `CKDEscalation_Month4-6_ub`)


# Reshape to long format
excess_CKD_long <- pivot_longer(excess_CKD,
                                cols = c(
                                  starts_with("d_excess")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")

excess_CKD_long <- transform(excess_CKD_long,
                             theme = "NCD",
                             disease = "CKD")

excess_CKD_long <- rename(excess_CKD_long,
                          d_excess_mean= mean,
                          d_excess_lci= lci,
                          d_excess_uci= uci)

#....crisis 
crisis_CKD <- read_excel("CKD/scenario_total_13-46M_by_age_.xlsx")
crisis_CKD <- subset(crisis_CKD, Age != "Total")

names(crisis_CKD)[names(crisis_CKD) == "Age"] <- "age"

variable_names <- names(crisis_CKD)
print(variable_names)

crisis_CKD <- crisis_CKD[, !names(crisis_CKD) %in% c("CKDCeasefire_All_mean", "CKDCeasefire_All_lb", "CKDCeasefire_All_ub",
                                                     "CKDStatus Quo_All_mean", "CKDStatus Quo_All_lb", "CKDStatus Quo_All_ub",
                                                     "CKDEscalation_All_mean", "CKDEscalation_All_lb", "CKDEscalation_All_ub")]

# Rename columns
crisis_CKD <- rename(crisis_CKD,  
                     d_crisis_mean_ceasefire_1to3= `CKDCeasefire_Month1-3_mean`, 
                     d_crisis_lci_ceasefire_1to3= `CKDCeasefire_Month1-3_lb`, 
                     d_crisis_uci_ceasefire_1to3= `CKDCeasefire_Month1-3_ub`, 
                     d_crisis_mean_ceasefire_4to6= `CKDCeasefire_Month4-6_mean`, 
                     d_crisis_lci_ceasefire_4to6= `CKDCeasefire_Month4-6_lb`, 
                     d_crisis_uci_ceasefire_4to6= `CKDCeasefire_Month4-6_ub`,
                     
                     d_crisis_mean_statusquo_1to3= `CKDStatus Quo_Month1-3_mean`, 
                     d_crisis_lci_statusquo_1to3= `CKDStatus Quo_Month1-3_lb`, 
                     d_crisis_uci_statusquo_1to3= `CKDStatus Quo_Month1-3_ub`, 
                     d_crisis_mean_statusquo_4to6= `CKDStatus Quo_Month4-6_mean`, 
                     d_crisis_lci_statusquo_4to6= `CKDStatus Quo_Month4-6_lb`, 
                     d_crisis_uci_statusquo_4to6= `CKDStatus Quo_Month4-6_ub`,
                     
                     
                     d_crisis_mean_escalation_1to3= `CKDEscalation_Month1-3_mean`, 
                     d_crisis_lci_escalation_1to3= `CKDEscalation_Month1-3_lb`, 
                     d_crisis_uci_escalation_1to3= `CKDEscalation_Month1-3_ub`, 
                     d_crisis_mean_escalation_4to6= `CKDEscalation_Month4-6_mean`, 
                     d_crisis_lci_escalation_4to6= `CKDEscalation_Month4-6_lb`, 
                     d_crisis_uci_escalation_4to6= `CKDEscalation_Month4-6_ub`)


# Reshape to long format
crisis_CKD_long <- pivot_longer(crisis_CKD,
                                cols = c(
                                  starts_with("d_crisis")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")


crisis_CKD_long <- rename(crisis_CKD_long,
                          d_crisis_mean= mean,
                          d_crisis_lci= lci,
                          d_crisis_uci= uci)


#....baseline 
baseline_CKD <- read_excel("CKD/baseline_total_13-46M_by_age_.xlsx")
baseline_CKD <- subset(baseline_CKD, Age != "Total")

names(baseline_CKD)[names(baseline_CKD) == "Age"] <- "age"


baseline_CKD <- baseline_CKD[, !names(baseline_CKD) %in% c("CKD_All_mean", "CKD_All_lb", "CKD_All_ub")]

# Rename columns
baseline_CKD <- rename(baseline_CKD,  
                       d_base_mean_1to3= `CKD_Month1-3_mean`, 
                       d_base_mean_4to6= `CKD_Month4-6_mean`, 
                       d_base_lci_1to3= `CKD_Month1-3_lb`, 
                       d_base_lci_4to6= `CKD_Month4-6_lb`, 
                       d_base_uci_1to3= `CKD_Month1-3_ub`, 
                       d_base_uci_4to6= `CKD_Month4-6_ub`) 

baseline_CKD_long <- pivot_longer(baseline_CKD,
                                  cols = c(
                                    starts_with("d_base")
                                  ),
                                  names_to = c(".value", "subperiod"),
                                  names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                  values_to = "Value")                   

baseline_CKD_long <- rename(baseline_CKD_long,
                            d_base_mean= mean,
                            d_base_lci= lci,
                            d_base_uci= uci)


CKD_merged_data <- merge(excess_CKD_long, crisis_CKD_long, by = c("age", "scenario", "subperiod"))
CKD <- merge(CKD_merged_data, baseline_CKD_long, by = c("age", "subperiod"))
CKD <- CKD[, !names(CKD) %in% c("...1.x", "...1.y", "...1")]

####  DM1 #### 
#... excess 
excess_DM1 <- read_excel("DM1/excess_death_13-46M_by_age_.xlsx")
excess_DM1 <- subset(excess_DM1, Age != "Total")
names(excess_DM1)[names(excess_DM1) == "Age"] <- "age"
excess_DM1 <- excess_DM1[, !names(excess_DM1) %in% c("DM1Ceasefire_All_mean", "DM1Ceasefire_All_lb", "DM1Ceasefire_All_ub",
                                                     "DM1Status Quo_All_mean", "DM1Status Quo_All_lb", "DM1Status Quo_All_ub",
                                                     "DM1Escalation_All_mean", "DM1Escalation_All_lb", "DM1Escalation_All_ub")]
# Rename columns
excess_DM1 <- rename(excess_DM1,  
                     d_excess_mean_ceasefire_1to3= `DM1Ceasefire_Month1-3_mean`, 
                     d_excess_lci_ceasefire_1to3= `DM1Ceasefire_Month1-3_lb`, 
                     d_excess_uci_ceasefire_1to3= `DM1Ceasefire_Month1-3_ub`, 
                     d_excess_mean_ceasefire_4to6= `DM1Ceasefire_Month4-6_mean`, 
                     d_excess_lci_ceasefire_4to6= `DM1Ceasefire_Month4-6_lb`, 
                     d_excess_uci_ceasefire_4to6= `DM1Ceasefire_Month4-6_ub`,
                     
                     d_excess_mean_statusquo_1to3= `DM1Status Quo_Month1-3_mean`, 
                     d_excess_lci_statusquo_1to3= `DM1Status Quo_Month1-3_lb`, 
                     d_excess_uci_statusquo_1to3= `DM1Status Quo_Month1-3_ub`, 
                     d_excess_mean_statusquo_4to6= `DM1Status Quo_Month4-6_mean`, 
                     d_excess_lci_statusquo_4to6= `DM1Status Quo_Month4-6_lb`, 
                     d_excess_uci_statusquo_4to6= `DM1Status Quo_Month4-6_ub`,
                     
                     
                     d_excess_mean_escalation_1to3= `DM1Escalation_Month1-3_mean`, 
                     d_excess_lci_escalation_1to3= `DM1Escalation_Month1-3_lb`, 
                     d_excess_uci_escalation_1to3= `DM1Escalation_Month1-3_ub`, 
                     d_excess_mean_escalation_4to6= `DM1Escalation_Month4-6_mean`, 
                     d_excess_lci_escalation_4to6= `DM1Escalation_Month4-6_lb`, 
                     d_excess_uci_escalation_4to6= `DM1Escalation_Month4-6_ub`)


# Reshape to long format
excess_DM1_long <- pivot_longer(excess_DM1,
                                cols = c(
                                  starts_with("d_excess")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")

excess_DM1_long <- transform(excess_DM1_long,
                             theme = "NCD",
                             disease = "DM1")

excess_DM1_long <- rename(excess_DM1_long,
                          d_excess_mean= mean,
                          d_excess_lci= lci,
                          d_excess_uci= uci)

#....crisis 
crisis_DM1 <- read_excel("DM1/scenario_total_13-46M_by_age_.xlsx")
crisis_DM1 <- subset(crisis_DM1, Age != "Total")

names(crisis_DM1)[names(crisis_DM1) == "Age"] <- "age"

variable_names <- names(crisis_DM1)
print(variable_names)

crisis_DM1 <- crisis_DM1[, !names(crisis_DM1) %in% c("DM1Ceasefire_All_mean", "DM1Ceasefire_All_lb", "DM1Ceasefire_All_ub",
                                                     "DM1Status Quo_All_mean", "DM1Status Quo_All_lb", "DM1Status Quo_All_ub",
                                                     "DM1Escalation_All_mean", "DM1Escalation_All_lb", "DM1Escalation_All_ub")]

# Rename columns
crisis_DM1 <- rename(crisis_DM1,  
                     d_crisis_mean_ceasefire_1to3= `DM1Ceasefire_Month1-3_mean`, 
                     d_crisis_lci_ceasefire_1to3= `DM1Ceasefire_Month1-3_lb`, 
                     d_crisis_uci_ceasefire_1to3= `DM1Ceasefire_Month1-3_ub`, 
                     d_crisis_mean_ceasefire_4to6= `DM1Ceasefire_Month4-6_mean`, 
                     d_crisis_lci_ceasefire_4to6= `DM1Ceasefire_Month4-6_lb`, 
                     d_crisis_uci_ceasefire_4to6= `DM1Ceasefire_Month4-6_ub`,
                     
                     d_crisis_mean_statusquo_1to3= `DM1Status Quo_Month1-3_mean`, 
                     d_crisis_lci_statusquo_1to3= `DM1Status Quo_Month1-3_lb`, 
                     d_crisis_uci_statusquo_1to3= `DM1Status Quo_Month1-3_ub`, 
                     d_crisis_mean_statusquo_4to6= `DM1Status Quo_Month4-6_mean`, 
                     d_crisis_lci_statusquo_4to6= `DM1Status Quo_Month4-6_lb`, 
                     d_crisis_uci_statusquo_4to6= `DM1Status Quo_Month4-6_ub`,
                     
                     
                     d_crisis_mean_escalation_1to3= `DM1Escalation_Month1-3_mean`, 
                     d_crisis_lci_escalation_1to3= `DM1Escalation_Month1-3_lb`, 
                     d_crisis_uci_escalation_1to3= `DM1Escalation_Month1-3_ub`, 
                     d_crisis_mean_escalation_4to6= `DM1Escalation_Month4-6_mean`, 
                     d_crisis_lci_escalation_4to6= `DM1Escalation_Month4-6_lb`, 
                     d_crisis_uci_escalation_4to6= `DM1Escalation_Month4-6_ub`)


# Reshape to long format
crisis_DM1_long <- pivot_longer(crisis_DM1,
                                cols = c(
                                  starts_with("d_crisis")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")


crisis_DM1_long <- rename(crisis_DM1_long,
                          d_crisis_mean= mean,
                          d_crisis_lci= lci,
                          d_crisis_uci= uci)


#....baseline 
baseline_DM1 <- read_excel("DM1/baseline_total_13-46M_by_age_.xlsx")
baseline_DM1 <- subset(baseline_DM1, Age != "Total")

names(baseline_DM1)[names(baseline_DM1) == "Age"] <- "age"


baseline_DM1 <- baseline_DM1[, !names(baseline_DM1) %in% c("DM1_All_mean", "DM1_All_lb", "DM1_All_ub")]

# Rename columns
baseline_DM1 <- rename(baseline_DM1,  
                       d_base_mean_1to3= `DM1_Month1-3_mean`, 
                       d_base_mean_4to6= `DM1_Month4-6_mean`, 
                       d_base_lci_1to3= `DM1_Month1-3_lb`, 
                       d_base_lci_4to6= `DM1_Month4-6_lb`, 
                       d_base_uci_1to3= `DM1_Month1-3_ub`, 
                       d_base_uci_4to6= `DM1_Month4-6_ub`) 

baseline_DM1_long <- pivot_longer(baseline_DM1,
                                  cols = c(
                                    starts_with("d_base")
                                  ),
                                  names_to = c(".value", "subperiod"),
                                  names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                  values_to = "Value")                   

baseline_DM1_long <- rename(baseline_DM1_long,
                            d_base_mean= mean,
                            d_base_lci= lci,
                            d_base_uci= uci)


DM1_merged_data <- merge(excess_DM1_long, crisis_DM1_long, by = c("age", "scenario", "subperiod"))
DM1 <- merge(DM1_merged_data, baseline_DM1_long, by = c("age", "subperiod"))
DM1 <- DM1[, !names(DM1) %in% c("...1.x", "...1.y", "...1")]


####  IS #### 
#... excess 
excess_IS <- read_excel("IS/excess_death_13-46M_by_age_.xlsx")
excess_IS <- subset(excess_IS, Age != "Total")
names(excess_IS)[names(excess_IS) == "Age"] <- "age"
excess_IS <- excess_IS[, !names(excess_IS) %in% c("ISCeasefire_All_mean", "ISCeasefire_All_lb", "ISCeasefire_All_ub",
                                                  "ISStatus Quo_All_mean", "ISStatus Quo_All_lb", "ISStatus Quo_All_ub",
                                                  "ISEscalation_All_mean", "ISEscalation_All_lb", "ISEscalation_All_ub")]
# Rename columns
excess_IS <- rename(excess_IS,  
                    d_excess_mean_ceasefire_1to3= `ISCeasefire_Month1-3_mean`, 
                    d_excess_lci_ceasefire_1to3= `ISCeasefire_Month1-3_lb`, 
                    d_excess_uci_ceasefire_1to3= `ISCeasefire_Month1-3_ub`, 
                    d_excess_mean_ceasefire_4to6= `ISCeasefire_Month4-6_mean`, 
                    d_excess_lci_ceasefire_4to6= `ISCeasefire_Month4-6_lb`, 
                    d_excess_uci_ceasefire_4to6= `ISCeasefire_Month4-6_ub`,
                    
                    d_excess_mean_statusquo_1to3= `ISStatus Quo_Month1-3_mean`, 
                    d_excess_lci_statusquo_1to3= `ISStatus Quo_Month1-3_lb`, 
                    d_excess_uci_statusquo_1to3= `ISStatus Quo_Month1-3_ub`, 
                    d_excess_mean_statusquo_4to6= `ISStatus Quo_Month4-6_mean`, 
                    d_excess_lci_statusquo_4to6= `ISStatus Quo_Month4-6_lb`, 
                    d_excess_uci_statusquo_4to6= `ISStatus Quo_Month4-6_ub`,
                    
                    
                    d_excess_mean_escalation_1to3= `ISEscalation_Month1-3_mean`, 
                    d_excess_lci_escalation_1to3= `ISEscalation_Month1-3_lb`, 
                    d_excess_uci_escalation_1to3= `ISEscalation_Month1-3_ub`, 
                    d_excess_mean_escalation_4to6= `ISEscalation_Month4-6_mean`, 
                    d_excess_lci_escalation_4to6= `ISEscalation_Month4-6_lb`, 
                    d_excess_uci_escalation_4to6= `ISEscalation_Month4-6_ub`)


# Reshape to long format
excess_IS_long <- pivot_longer(excess_IS,
                               cols = c(
                                 starts_with("d_excess")
                               ),
                               names_to = c(".value", "scenario", "subperiod"),
                               names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                               values_to = "Value")

excess_IS_long <- transform(excess_IS_long,
                            theme = "NCD",
                            disease = "IS")

excess_IS_long <- rename(excess_IS_long,
                         d_excess_mean= mean,
                         d_excess_lci= lci,
                         d_excess_uci= uci)

#....crisis 
crisis_IS <- read_excel("IS/scenario_total_13-46M_by_age_.xlsx")
crisis_IS <- subset(crisis_IS, Age != "Total")

names(crisis_IS)[names(crisis_IS) == "Age"] <- "age"

variable_names <- names(crisis_IS)
print(variable_names)

crisis_IS <- crisis_IS[, !names(crisis_IS) %in% c("ISCeasefire_All_mean", "ISCeasefire_All_lb", "ISCeasefire_All_ub",
                                                  "ISStatus Quo_All_mean", "ISStatus Quo_All_lb", "ISStatus Quo_All_ub",
                                                  "ISEscalation_All_mean", "ISEscalation_All_lb", "ISEscalation_All_ub")]

# Rename columns
crisis_IS <- rename(crisis_IS,  
                    d_crisis_mean_ceasefire_1to3= `ISCeasefire_Month1-3_mean`, 
                    d_crisis_lci_ceasefire_1to3= `ISCeasefire_Month1-3_lb`, 
                    d_crisis_uci_ceasefire_1to3= `ISCeasefire_Month1-3_ub`, 
                    d_crisis_mean_ceasefire_4to6= `ISCeasefire_Month4-6_mean`, 
                    d_crisis_lci_ceasefire_4to6= `ISCeasefire_Month4-6_lb`, 
                    d_crisis_uci_ceasefire_4to6= `ISCeasefire_Month4-6_ub`,
                    
                    d_crisis_mean_statusquo_1to3= `ISStatus Quo_Month1-3_mean`, 
                    d_crisis_lci_statusquo_1to3= `ISStatus Quo_Month1-3_lb`, 
                    d_crisis_uci_statusquo_1to3= `ISStatus Quo_Month1-3_ub`, 
                    d_crisis_mean_statusquo_4to6= `ISStatus Quo_Month4-6_mean`, 
                    d_crisis_lci_statusquo_4to6= `ISStatus Quo_Month4-6_lb`, 
                    d_crisis_uci_statusquo_4to6= `ISStatus Quo_Month4-6_ub`,
                    
                    
                    d_crisis_mean_escalation_1to3= `ISEscalation_Month1-3_mean`, 
                    d_crisis_lci_escalation_1to3= `ISEscalation_Month1-3_lb`, 
                    d_crisis_uci_escalation_1to3= `ISEscalation_Month1-3_ub`, 
                    d_crisis_mean_escalation_4to6= `ISEscalation_Month4-6_mean`, 
                    d_crisis_lci_escalation_4to6= `ISEscalation_Month4-6_lb`, 
                    d_crisis_uci_escalation_4to6= `ISEscalation_Month4-6_ub`)


# Reshape to long format
crisis_IS_long <- pivot_longer(crisis_IS,
                               cols = c(
                                 starts_with("d_crisis")
                               ),
                               names_to = c(".value", "scenario", "subperiod"),
                               names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                               values_to = "Value")


crisis_IS_long <- rename(crisis_IS_long,
                         d_crisis_mean= mean,
                         d_crisis_lci= lci,
                         d_crisis_uci= uci)


#....baseline 
baseline_IS <- read_excel("IS/baseline_total_13-46M_by_age_.xlsx")
baseline_IS <- subset(baseline_IS, Age != "Total")

names(baseline_IS)[names(baseline_IS) == "Age"] <- "age"


baseline_IS <- baseline_IS[, !names(baseline_IS) %in% c("IS_All_mean", "IS_All_lb", "IS_All_ub")]

# Rename columns
baseline_IS <- rename(baseline_IS,  
                      d_base_mean_1to3= `IS_Month1-3_mean`, 
                      d_base_mean_4to6= `IS_Month4-6_mean`, 
                      d_base_lci_1to3= `IS_Month1-3_lb`, 
                      d_base_lci_4to6= `IS_Month4-6_lb`, 
                      d_base_uci_1to3= `IS_Month1-3_ub`, 
                      d_base_uci_4to6= `IS_Month4-6_ub`) 

baseline_IS_long <- pivot_longer(baseline_IS,
                                 cols = c(
                                   starts_with("d_base")
                                 ),
                                 names_to = c(".value", "subperiod"),
                                 names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                 values_to = "Value")                   

baseline_IS_long <- rename(baseline_IS_long,
                           d_base_mean= mean,
                           d_base_lci= lci,
                           d_base_uci= uci)


IS_merged_data <- merge(excess_IS_long, crisis_IS_long, by = c("age", "scenario", "subperiod"))
IS <- merge(IS_merged_data, baseline_IS_long, by = c("age", "subperiod"))
IS <- IS[, !names(IS) %in% c("...1.x", "...1.y", "...1")]


####  HS #### 
#... excess 
excess_HS <- read_excel("HS/excess_death_13-46M_by_age_.xlsx")
excess_HS <- subset(excess_HS, Age != "Total")
names(excess_HS)[names(excess_HS) == "Age"] <- "age"
excess_HS <- excess_HS[, !names(excess_HS) %in% c("HSCeasefire_All_mean", "HSCeasefire_All_lb", "HSCeasefire_All_ub",
                                                  "HSStatus Quo_All_mean", "HSStatus Quo_All_lb", "HSStatus Quo_All_ub",
                                                  "HSEscalation_All_mean", "HSEscalation_All_lb", "HSEscalation_All_ub")]
# Rename columns
excess_HS <- rename(excess_HS,  
                    d_excess_mean_ceasefire_1to3= `HSCeasefire_Month1-3_mean`, 
                    d_excess_lci_ceasefire_1to3= `HSCeasefire_Month1-3_lb`, 
                    d_excess_uci_ceasefire_1to3= `HSCeasefire_Month1-3_ub`, 
                    d_excess_mean_ceasefire_4to6= `HSCeasefire_Month4-6_mean`, 
                    d_excess_lci_ceasefire_4to6= `HSCeasefire_Month4-6_lb`, 
                    d_excess_uci_ceasefire_4to6= `HSCeasefire_Month4-6_ub`,
                    
                    d_excess_mean_statusquo_1to3= `HSStatus Quo_Month1-3_mean`, 
                    d_excess_lci_statusquo_1to3= `HSStatus Quo_Month1-3_lb`, 
                    d_excess_uci_statusquo_1to3= `HSStatus Quo_Month1-3_ub`, 
                    d_excess_mean_statusquo_4to6= `HSStatus Quo_Month4-6_mean`, 
                    d_excess_lci_statusquo_4to6= `HSStatus Quo_Month4-6_lb`, 
                    d_excess_uci_statusquo_4to6= `HSStatus Quo_Month4-6_ub`,
                    
                    
                    d_excess_mean_escalation_1to3= `HSEscalation_Month1-3_mean`, 
                    d_excess_lci_escalation_1to3= `HSEscalation_Month1-3_lb`, 
                    d_excess_uci_escalation_1to3= `HSEscalation_Month1-3_ub`, 
                    d_excess_mean_escalation_4to6= `HSEscalation_Month4-6_mean`, 
                    d_excess_lci_escalation_4to6= `HSEscalation_Month4-6_lb`, 
                    d_excess_uci_escalation_4to6= `HSEscalation_Month4-6_ub`)


# Reshape to long format
excess_HS_long <- pivot_longer(excess_HS,
                               cols = c(
                                 starts_with("d_excess")
                               ),
                               names_to = c(".value", "scenario", "subperiod"),
                               names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                               values_to = "Value")

excess_HS_long <- transform(excess_HS_long,
                            theme = "NCD",
                            disease = "HS")

excess_HS_long <- rename(excess_HS_long,
                         d_excess_mean= mean,
                         d_excess_lci= lci,
                         d_excess_uci= uci)

#....crisis 
crisis_HS <- read_excel("HS/scenario_total_13-46M_by_age_.xlsx")
crisis_HS <- subset(crisis_HS, Age != "Total")

names(crisis_HS)[names(crisis_HS) == "Age"] <- "age"

variable_names <- names(crisis_HS)
print(variable_names)

crisis_HS <- crisis_HS[, !names(crisis_HS) %in% c("HSCeasefire_All_mean", "HSCeasefire_All_lb", "HSCeasefire_All_ub",
                                                  "HSStatus Quo_All_mean", "HSStatus Quo_All_lb", "HSStatus Quo_All_ub",
                                                  "HSEscalation_All_mean", "HSEscalation_All_lb", "HSEscalation_All_ub")]

# Rename columns
crisis_HS <- rename(crisis_HS,  
                    d_crisis_mean_ceasefire_1to3= `HSCeasefire_Month1-3_mean`, 
                    d_crisis_lci_ceasefire_1to3= `HSCeasefire_Month1-3_lb`, 
                    d_crisis_uci_ceasefire_1to3= `HSCeasefire_Month1-3_ub`, 
                    d_crisis_mean_ceasefire_4to6= `HSCeasefire_Month4-6_mean`, 
                    d_crisis_lci_ceasefire_4to6= `HSCeasefire_Month4-6_lb`, 
                    d_crisis_uci_ceasefire_4to6= `HSCeasefire_Month4-6_ub`,
                    
                    d_crisis_mean_statusquo_1to3= `HSStatus Quo_Month1-3_mean`, 
                    d_crisis_lci_statusquo_1to3= `HSStatus Quo_Month1-3_lb`, 
                    d_crisis_uci_statusquo_1to3= `HSStatus Quo_Month1-3_ub`, 
                    d_crisis_mean_statusquo_4to6= `HSStatus Quo_Month4-6_mean`, 
                    d_crisis_lci_statusquo_4to6= `HSStatus Quo_Month4-6_lb`, 
                    d_crisis_uci_statusquo_4to6= `HSStatus Quo_Month4-6_ub`,
                    
                    
                    d_crisis_mean_escalation_1to3= `HSEscalation_Month1-3_mean`, 
                    d_crisis_lci_escalation_1to3= `HSEscalation_Month1-3_lb`, 
                    d_crisis_uci_escalation_1to3= `HSEscalation_Month1-3_ub`, 
                    d_crisis_mean_escalation_4to6= `HSEscalation_Month4-6_mean`, 
                    d_crisis_lci_escalation_4to6= `HSEscalation_Month4-6_lb`, 
                    d_crisis_uci_escalation_4to6= `HSEscalation_Month4-6_ub`)


# Reshape to long format
crisis_HS_long <- pivot_longer(crisis_HS,
                               cols = c(
                                 starts_with("d_crisis")
                               ),
                               names_to = c(".value", "scenario", "subperiod"),
                               names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                               values_to = "Value")


crisis_HS_long <- rename(crisis_HS_long,
                         d_crisis_mean= mean,
                         d_crisis_lci= lci,
                         d_crisis_uci= uci)


#....baseline 
baseline_HS <- read_excel("HS/baseline_total_13-46M_by_age_.xlsx")
baseline_HS <- subset(baseline_HS, Age != "Total")

names(baseline_HS)[names(baseline_HS) == "Age"] <- "age"


baseline_HS <- baseline_HS[, !names(baseline_HS) %in% c("HS_All_mean", "HS_All_lb", "HS_All_ub")]

# Rename columns
baseline_HS <- rename(baseline_HS,  
                      d_base_mean_1to3= `HS_Month1-3_mean`, 
                      d_base_mean_4to6= `HS_Month4-6_mean`, 
                      d_base_lci_1to3= `HS_Month1-3_lb`, 
                      d_base_lci_4to6= `HS_Month4-6_lb`, 
                      d_base_uci_1to3= `HS_Month1-3_ub`, 
                      d_base_uci_4to6= `HS_Month4-6_ub`) 

baseline_HS_long <- pivot_longer(baseline_HS,
                                 cols = c(
                                   starts_with("d_base")
                                 ),
                                 names_to = c(".value", "subperiod"),
                                 names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                 values_to = "Value")                   

baseline_HS_long <- rename(baseline_HS_long,
                           d_base_mean= mean,
                           d_base_lci= lci,
                           d_base_uci= uci)


HS_merged_data <- merge(excess_HS_long, crisis_HS_long, by = c("age", "scenario", "subperiod"))
HS <- merge(HS_merged_data, baseline_HS_long, by = c("age", "subperiod"))
HS <- HS[, !names(HS) %in% c("...1.x", "...1.y", "...1")]


####  IHD #### 
#... excess 
excess_IHD <- read_excel("IHD/excess_death_13-46M_by_age_.xlsx")
excess_IHD <- subset(excess_IHD, Age != "Total")
names(excess_IHD)[names(excess_IHD) == "Age"] <- "age"
excess_IHD <- excess_IHD[, !names(excess_IHD) %in% c("IHDCeasefire_All_mean", "IHDCeasefire_All_lb", "IHDCeasefire_All_ub",
                                                     "IHDStatus Quo_All_mean", "IHDStatus Quo_All_lb", "IHDStatus Quo_All_ub",
                                                     "IHDEscalation_All_mean", "IHDEscalation_All_lb", "IHDEscalation_All_ub")]
# Rename columns
excess_IHD <- rename(excess_IHD,  
                     d_excess_mean_ceasefire_1to3= `IHDCeasefire_Month1-3_mean`, 
                     d_excess_lci_ceasefire_1to3= `IHDCeasefire_Month1-3_lb`, 
                     d_excess_uci_ceasefire_1to3= `IHDCeasefire_Month1-3_ub`, 
                     d_excess_mean_ceasefire_4to6= `IHDCeasefire_Month4-6_mean`, 
                     d_excess_lci_ceasefire_4to6= `IHDCeasefire_Month4-6_lb`, 
                     d_excess_uci_ceasefire_4to6= `IHDCeasefire_Month4-6_ub`,
                     
                     d_excess_mean_statusquo_1to3= `IHDStatus Quo_Month1-3_mean`, 
                     d_excess_lci_statusquo_1to3= `IHDStatus Quo_Month1-3_lb`, 
                     d_excess_uci_statusquo_1to3= `IHDStatus Quo_Month1-3_ub`, 
                     d_excess_mean_statusquo_4to6= `IHDStatus Quo_Month4-6_mean`, 
                     d_excess_lci_statusquo_4to6= `IHDStatus Quo_Month4-6_lb`, 
                     d_excess_uci_statusquo_4to6= `IHDStatus Quo_Month4-6_ub`,
                     
                     
                     d_excess_mean_escalation_1to3= `IHDEscalation_Month1-3_mean`, 
                     d_excess_lci_escalation_1to3= `IHDEscalation_Month1-3_lb`, 
                     d_excess_uci_escalation_1to3= `IHDEscalation_Month1-3_ub`, 
                     d_excess_mean_escalation_4to6= `IHDEscalation_Month4-6_mean`, 
                     d_excess_lci_escalation_4to6= `IHDEscalation_Month4-6_lb`, 
                     d_excess_uci_escalation_4to6= `IHDEscalation_Month4-6_ub`)


# Reshape to long format
excess_IHD_long <- pivot_longer(excess_IHD,
                                cols = c(
                                  starts_with("d_excess")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_excess_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")

excess_IHD_long <- transform(excess_IHD_long,
                             theme = "NCD",
                             disease = "IHD")

excess_IHD_long <- rename(excess_IHD_long,
                          d_excess_mean= mean,
                          d_excess_lci= lci,
                          d_excess_uci= uci)

#....crisis 
crisis_IHD <- read_excel("IHD/scenario_total_13-46M_by_age_.xlsx")
crisis_IHD <- subset(crisis_IHD, Age != "Total")

names(crisis_IHD)[names(crisis_IHD) == "Age"] <- "age"

variable_names <- names(crisis_IHD)
print(variable_names)

crisis_IHD <- crisis_IHD[, !names(crisis_IHD) %in% c("IHDCeasefire_All_mean", "IHDCeasefire_All_lb", "IHDCeasefire_All_ub",
                                                     "IHDStatus Quo_All_mean", "IHDStatus Quo_All_lb", "IHDStatus Quo_All_ub",
                                                     "IHDEscalation_All_mean", "IHDEscalation_All_lb", "IHDEscalation_All_ub")]

# Rename columns
crisis_IHD <- rename(crisis_IHD,  
                     d_crisis_mean_ceasefire_1to3= `IHDCeasefire_Month1-3_mean`, 
                     d_crisis_lci_ceasefire_1to3= `IHDCeasefire_Month1-3_lb`, 
                     d_crisis_uci_ceasefire_1to3= `IHDCeasefire_Month1-3_ub`, 
                     d_crisis_mean_ceasefire_4to6= `IHDCeasefire_Month4-6_mean`, 
                     d_crisis_lci_ceasefire_4to6= `IHDCeasefire_Month4-6_lb`, 
                     d_crisis_uci_ceasefire_4to6= `IHDCeasefire_Month4-6_ub`,
                     
                     d_crisis_mean_statusquo_1to3= `IHDStatus Quo_Month1-3_mean`, 
                     d_crisis_lci_statusquo_1to3= `IHDStatus Quo_Month1-3_lb`, 
                     d_crisis_uci_statusquo_1to3= `IHDStatus Quo_Month1-3_ub`, 
                     d_crisis_mean_statusquo_4to6= `IHDStatus Quo_Month4-6_mean`, 
                     d_crisis_lci_statusquo_4to6= `IHDStatus Quo_Month4-6_lb`, 
                     d_crisis_uci_statusquo_4to6= `IHDStatus Quo_Month4-6_ub`,
                     
                     
                     d_crisis_mean_escalation_1to3= `IHDEscalation_Month1-3_mean`, 
                     d_crisis_lci_escalation_1to3= `IHDEscalation_Month1-3_lb`, 
                     d_crisis_uci_escalation_1to3= `IHDEscalation_Month1-3_ub`, 
                     d_crisis_mean_escalation_4to6= `IHDEscalation_Month4-6_mean`, 
                     d_crisis_lci_escalation_4to6= `IHDEscalation_Month4-6_lb`, 
                     d_crisis_uci_escalation_4to6= `IHDEscalation_Month4-6_ub`)


# Reshape to long format
crisis_IHD_long <- pivot_longer(crisis_IHD,
                                cols = c(
                                  starts_with("d_crisis")
                                ),
                                names_to = c(".value", "scenario", "subperiod"),
                                names_pattern = "d_crisis_(mean|lci|uci)_(.+)_(.+)",
                                values_to = "Value")


crisis_IHD_long <- rename(crisis_IHD_long,
                          d_crisis_mean= mean,
                          d_crisis_lci= lci,
                          d_crisis_uci= uci)


#....baseline 
baseline_IHD <- read_excel("IHD/baseline_total_13-46M_by_age_.xlsx")
baseline_IHD <- subset(baseline_IHD, Age != "Total")

names(baseline_IHD)[names(baseline_IHD) == "Age"] <- "age"


baseline_IHD <- baseline_IHD[, !names(baseline_IHD) %in% c("IHD_All_mean", "IHD_All_lb", "IHD_All_ub")]

# Rename columns
baseline_IHD <- rename(baseline_IHD,  
                       d_base_mean_1to3= `IHD_Month1-3_mean`, 
                       d_base_mean_4to6= `IHD_Month4-6_mean`, 
                       d_base_lci_1to3= `IHD_Month1-3_lb`, 
                       d_base_lci_4to6= `IHD_Month4-6_lb`, 
                       d_base_uci_1to3= `IHD_Month1-3_ub`, 
                       d_base_uci_4to6= `IHD_Month4-6_ub`) 

baseline_IHD_long <- pivot_longer(baseline_IHD,
                                  cols = c(
                                    starts_with("d_base")
                                  ),
                                  names_to = c(".value", "subperiod"),
                                  names_pattern = "d_base_(mean|lci|uci)_(.+)",
                                  values_to = "Value")                   

baseline_IHD_long <- rename(baseline_IHD_long,
                            d_base_mean= mean,
                            d_base_lci= lci,
                            d_base_uci= uci)


IHD_merged_data <- merge(excess_IHD_long, crisis_IHD_long, by = c("age", "scenario", "subperiod"))
IHD <- merge(IHD_merged_data, baseline_IHD_long, by = c("age", "subperiod"))
IHD <- IHD[, !names(IHD) %in% c("...1.x", "...1.y", "...1")]




combined_NCD <- bind_rows(IHD, DM1, IS, HS, Bcancer, Ccancer, Lcancer, CKD)




######################### Combined modules #########################

combined_modules <- bind_rows(combined_infection, combined_injuries, combined_MNH, combined_NCD)

# period
combined_modules$subperiod[combined_modules$subperiod == "1to3"] <- "months 1 to 3"
combined_modules$subperiod[combined_modules$subperiod == "4to6"] <- "months 4 to 6"


# disease
combined_modules$disease[combined_modules$disease == "DM1"] <- "diabetes type 1"
combined_modules$disease[combined_modules$disease == "Bcancer"] <- "breast cancer"
combined_modules$disease[combined_modules$disease == "Ccancer"] <- "colorectal cancer"
combined_modules$disease[combined_modules$disease == "Lcancer"] <- "lung cancer"
combined_modules$disease[combined_modules$disease == "CKD"] <- "chronic kidney disease"
combined_modules$disease[combined_modules$disease == "IHD"] <- "ischaemic heart disease"
combined_modules$disease[combined_modules$disease == "IS"] <- "ischaemic stroke"
combined_modules$disease[combined_modules$disease == "HS"] <- "hemorrhagic stroke"



# Cleaning combination

combined_modules <- mutate(combined_modules, scenario = gsub("statusquo", "status quo", scenario))


# for NCD data <1 for 0mo and 1-11mo it is the same 

# Identify rows where age is "<1" for NCDs
rows_to_duplicate <- combined_modules$age == "<1"

# Duplicate rows and set new age values
combined_modules <- rbind(
  combined_modules,
  transform(combined_modules[rows_to_duplicate, ], 
            age = "0mo", 
            d_base_median = NA,
            d_crisis_median = NA,
            d_excess_median = NA,
            d_base_mean = 0,
            d_base_lci = 0,
            d_base_uci = 0,
            d_crisis_mean = 0,
            d_crisis_lci = 0,
            d_crisis_uci = 0, 
            d_excess_mean = 0,
            d_excess_lci = 0,
            d_excess_uci = 0, 
            scenario = scenario,
            disease= disease,
            theme=theme),
  
  transform(combined_modules[rows_to_duplicate, ], 
            age = "1 to 11mo",
            d_base_median = d_base_median,
            d_crisis_median = d_crisis_median,
            d_excess_median = d_excess_median,
            d_base_mean = d_base_mean,
            d_base_lci = d_base_lci,
            d_base_uci = d_base_uci,
            d_crisis_mean = d_crisis_mean,
            d_crisis_lci = d_crisis_lci,
            d_crisis_uci = d_crisis_uci, 
            d_excess_mean = d_excess_mean,
            d_excess_lci = d_excess_lci,
            d_excess_uci = d_excess_uci, 
            scenario = scenario,
            disease= disease,
            theme=theme)
)


# Drop the original "<1" rows
combined_modules <- combined_modules[!rows_to_duplicate, ]




# Write the data frame to an Excel file
write_xlsx(combined_modules, "20240212_long_modules.xlsx")


