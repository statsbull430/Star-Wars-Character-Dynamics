## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
mean(data$Charcaters_Proximity_to_Economic_Power)
sd(data$Charcaters_Proximity_to_Economic_Power)
table(data$Charcaters_Proximity_to_Economic_Power)
describe(data$Charcaters_Proximity_to_Economic_Power)
summary(data$Charcaters_Proximity_to_Economic_Power)

# EXAMINE QUANT_VAR2
mean(data$Characters_Proximity_to_Political_Power)
sd(data$Characters_Proximity_to_Political_Power)
table(data$Characters_Proximity_to_Political_Power)
describe(data$Characters_Proximity_to_Political_Power)
summary(data$Characters_Proximity_to_Political_Power)

# EXAMINE QUANT_VAR3
mean(data$Character_Development)
sd(data$Character_Development)
table(data$Character_Development)
describe(data$Character_Development)
summary(data$Character_Development)

# EXAMINE QUAL_VAR1
table(data$Characters_Attitudes_Towards_Their_Environment)
describe(data$Characters_Attitudes_Towards_Their_Environment)

# EXAMINE QUAL_VAR2
table(data$Interpersonal_Conflict)
describe(data$Interpersonal_Conflict)

# EXAMINE QUAL_VAR3
table(data$Character_Motivations)
describe(data$Character_Motivations)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Characters_Attitudes_Towards_Their_Environment,data$Character_Motivations)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Characters_Attitudes_Towards_Their_Environment, data$Character_Motivations))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(data$Characters_Proximity_to_Political_Power ~ data$Character_Motivations, data = data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
plot(data$Charcaters_Proximity_to_Economic_Power, data$Characters_Proximity_to_Political_Power)
cor(data$Charcaters_Proximity_to_Economic_Power,data$Characters_Proximity_to_Political_Power)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(data$Characters_Proximity_to_Political_Power ~ data$Charcaters_Proximity_to_Economic_Power, data = data)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Character_Development, data$Charcaters_Proximity_to_Economic_Power)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Characters_Proximity_to_Political_Power, residuals(linear_relationship))
abline(h= 0, col = "red")
