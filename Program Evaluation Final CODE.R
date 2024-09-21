##PART 3 of Final Exam: Tax Rates##
#Understanding consequences of a higher marginal property tax rate 

setwd("/Users/misbaharshad/Downloads/") 
data <- read.csv("final_exam_2023.csv") 
library(ggplot2) 
library(dplyr) 
library(broom)
library(lmtest)
library(sandwich)

county_A <- data %>% filter(county == "ALAMEDA") 
county_B <- data %>% filter(county == "SANTA CLARA") 
county_C <- data %>% filter(county == "YOLO")


#Regression Discontinuity Tests: Smoothness 
ggplot(county_A, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for ALAMEDA: Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")

ggplot(county_B, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for SANTA CLARA: Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")

ggplot(county_C, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for YOLO: Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")

#Regression Discontinuity Tests: Bunching 
ggplot(county_A, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County A: Histogram of Household Income", x = "Household Income", y = "Count")
ggplot(county_B, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County B: Histogram of Household Income", x = "Household Income", y = "Count")
ggplot(county_C, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County C: Histogram of Household Income", x = "Household Income", y = "Count")




#Relationship between year of home purchase and the probability of having a 
#high marginal tax rate for each county
county_A <- county_A %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 
county_B <- county_B %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 
county_C <- county_C %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 

# Aggregate data 
tax_rate_by_year_A <- county_A %>%
  group_by(year_of_home_purchase) %>%
  summarise(prob_high_tax = mean(high_tax_dummy, na.rm = TRUE))

tax_rate_by_year_B <- county_B %>%
  group_by(year_of_home_purchase) %>%
  summarise(prob_high_tax = mean(high_tax_dummy, na.rm = TRUE))

tax_rate_by_year_C <- county_C %>%
  group_by(year_of_home_purchase) %>%
  summarise(prob_high_tax = mean(high_tax_dummy, na.rm = TRUE))

# Plot the relationship 
ggplot(tax_rate_by_year_A, aes(x = year_of_home_purchase, y = prob_high_tax)) +
  geom_point() +
  geom_line() +
  labs(title = "County A: Probability of High Tax Rate vs. Year of Home Purchase",
       x = "Year of Home Purchase", y = "Probability of High Tax Rate") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")

ggplot(tax_rate_by_year_B, aes(x = year_of_home_purchase, y = prob_high_tax)) +
  geom_point() +
  geom_line() +
  labs(title = "County B: Probability of High Tax Rate vs. Year of Home Purchase",
       x = "Year of Home Purchase", y = "Probability of High Tax Rate") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")

ggplot(tax_rate_by_year_C, aes(x = year_of_home_purchase, y = prob_high_tax)) +
  geom_point() +
  geom_line() +
  labs(title = "County C: Probability of High Tax Rate vs. Year of Home Purchase",
       x = "Year of Home Purchase", y = "Probability of High Tax Rate") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red")




#Estimate causal effect of having a high marginal property tax rate on tax evasion
county_A_rdd <- lm(evades_taxes_yn ~ high_tax_dummy, data = county_A) 
summary(county_A_rdd) 
coeftest(county_A_rdd, vcov. = sandwich)

county_B_lm <- lm(evades_taxes_yn ~ marginal_property_tax_rate, data = county_B) 
summary(county_B_lm) 
coeftest(county_B_lm, vcov. = sandwich)

county_C_lm <- lm(evades_taxes_yn ~ marginal_property_tax_rate, data = county_C) 
summary(county_C_lm) 
coeftest(county_C_lm, vcov. = sandwich)
