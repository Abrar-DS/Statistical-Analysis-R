# Import libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
utils::install.packages(pkgs = "ggstatsplot")

# Read data and save it in df
df <- read.csv("cancer.csv")

######################################################################
# 1.Derive descriptive statistics regarding this dataset, including
#measures of central tendency for the following fields:
#Age,Air Pollution,Alcohol use,Smoking

# Create a function to calculate standard deviation and variance 
descriptive.func <- function(x) {
  c(sd = sd(x), var = var(x))
}

# Use descriptive.func and summary to display the descriptive Statistics
# for Age column	
lapply(df['Age'], descriptive.func)
summary(df$Age)

# Use descriptive.func and summary to display the descriptive Statistics
# for Air Pollution column	
lapply(df['Air.Pollution'], descriptive.func)
summary(df$Air.Pollution)

# Use descriptive.func and summary to display the descriptive Statistics
# for Alcohol use column
lapply(df['Alcohol.use'], descriptive.func)
summary(df$Alcohol.use)

# Use descriptive.func and summary to display the descriptive Statistics
# for Smoking column
lapply(df['Smoking'], descriptive.func)
summary(df$Smoking)

######################################################################
# 2.The “cancer .csv” file contains few missing entries in the file.
#Find out how many rows have the missing values. How would you deal 
#with these missing values for your analysis?


# Find out how many rows have the missing values:
# Can be done in two ways:

# 1
sum(is.na(df))

# 2
# Creat a column called count_na and calculate the missing values in each row
df$count_na <- rowSums(is.na(df))
# Number of rows with missing values
count_rows_na <- nrow(df[df$count_na >= 1, ])
count_rows_na

# How would you deal with these missing values for your analysis?
# Written in the report


######################################################################
# 3.Find the effect of following factors on the “Level” of the cancer
# and justify your answer. Represent the correlation graphically: 
# Alcohol use, Genetic Risk, Balanced Diet, Smoking, Shortness of Breath

# Plot the effect of Alcohol use on the “Level” of the cancer
ggplot(df) +
  aes(x = Level, y = Alcohol.use) +
  stat_summary(geom = "bar", fun = "mean")


# Plot the effect of Genetic Risk on the “Level” of the cancer
ggplot(df) +
  aes(x = Level, y = Genetic.Risk) +
  stat_summary(geom = "bar", fun = "mean")

# Plot the effect of Balanced Diet on the “Level” of the cancer
ggplot(df) +
  aes(x = Level, y = Balanced.Diet) +
  stat_summary(geom = "bar", fun = "mean")

# Plot the effect of Smoking on the “Level” of the cancer
ggplot(df) +
  aes(x = Level, y = Smoking) +
  stat_summary(geom = "bar", fun = "mean")

# Plot the effect of Shortness of Breath on the “Level” of the cancer
ggplot(df) +
  aes(x = Level, y = Shortness.of.Breath) +
  stat_summary(geom = "bar", fun = "mean")

######################################################################

# 4.What is the probability of having cancer level = “High” when
# The level of passive smoking is more than equal to 5.

#P(cancer level = “High” Ո passive smoking >= 5) 
prob_high_level_and_passive_smoker <- (sum(df$Level == "High" & df$Passive.Smoker >= 5))/1000
#P(passive smoking >= 5) 
prob_passive_smoker <- (sum(df$Passive.Smoker >= 5))/1000
#P(cancer level = “High” | passive smoking >= 5)
prob_high_level_given_passive_smoker <- prob_high_level_and_passive_smoker/prob_passive_smoker
prob_high_level_given_passive_smoker


# What is the probability of having cancer level = “High” when
# The level of dust allergy is less than 5.

#P(cancer level = “High” Ո dust allergy < 5) 
prob_high_level_and_dust_allergy <- (sum(df$Level == "High" & df$Dust.Allergy < 5, na.rm = TRUE))/1000
#P(dust allergy < 5) 
prob_dust_allergy <- (sum(df$Dust.Allergy < 5,na.rm = TRUE ))/1000
#P(cancer level = “High” | dust allergy < 5)
prob_high_level_given_dust_allergy <- prob_high_level_and_dust_allergy/prob_dust_allergy
prob_high_level_given_dust_allergy

######################################################################
# 5.Is there any association between “Alcohol use” and “Smoking”? Justify your answer.         

# Check assumptions of normality for both Alcohol.use and Smoking 
# variables using Shapiro-Wilk test
shapiro.test(df$Alcohol.use)$p
shapiro.test(df$Smoking)$p

# As the p > 0.05 for both Alcohol.use and Smoking variables,
#we fail to reject null hypothesis and conclude that both variables
#are approximately normally distributed. We can use Pearson’s method 
#for finding the correlation coefficient.

# calculate Pearson's correlation coefficient
cor.test(df$Alcohol.use, df$Smoking, method = "pearson")

# plot the correlation using ggscatterstats
ggstatsplot::ggscatterstats(data = df, x = Alcohol.use, y = Smoking)


######################################################################

# 6.	Is there any outlier for the following?Justify your answer systematically. 
# 1.	Weight Loss
# 2.	Obesity

# Find the outliers for Weight Loss 
# get mean and Standard deviation
mean = mean(df$Weight.Loss)
std = sd(df$Weight.Loss)
# get threshold values for outliers
Tmin = mean-(3*std)
Tmax = mean+(3*std)
# find outlier
df$Weight.Loss[which(df$Weight.Loss < Tmin | df$Weight.Loss > Tmax)]
# Plot to find outlier
ggplot(df) + aes(x= Weight.Loss) + geom_histogram(bins = 30L) + theme_minimal()

# Find the outliers for Obesity
# get mean and Standard deviation
mean = mean(df$Obesity)
std = sd(df$Obesity)
# get threshold values for outliers
Tmin = mean-(3*std)
Tmax = mean+(3*std)
# find outlier
df$Obesity[which(df$Obesity < Tmin | df$Obesity > Tmax)]
# Plot to find outlier
ggplot(df) + aes(x= Obesity) + geom_histogram(bins = 30L) + theme_minimal()


######################################################################

# 7.Analyze the “Frequent Cold" and “Dry Cough” for male and female. Depict 
#graphically which category (Male or Female) is more prone to for these symptoms?
# Replace each 1 in the column gender with Male
df$Gender[df$Gender == '1'] <- 'Male'
# Replace each 2 in the column gender with Female
df$Gender[df$Gender == '2'] <- 'Female'
# Plot the "Frequent Cold" and “Dry Cough” for male and female.
scatter <- ggplot(df, aes(Frequent.Cold, Dry.Cough, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender)) 
+ labs(x = "Frequent Cold", y = "Dry Cough", colour = "Gender") 

######################################################################
# 8.Explain graphically which age group is snoring more? You can create your 
#own age group.

# Create categories for the Age
    df$age_group = dplyr::case_when(
      df$Age <= 20            ~ "0-20",
      df$Age > 20 & df$Age <= 40 ~ "21-40",
      df$Age > 40 & df$Age <= 50 ~ "41-50",
      df$Age > 50             ~ "> 51"
    )
   
 # Plot the relation between age groups and Snoring   
    ggplot(df, aes(x=age_group, y=Snoring)) +
      geom_boxplot(fill='steelblue')

    
######################################################################