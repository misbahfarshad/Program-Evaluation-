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

ggplot(county_A, aes(x = household_income, y = marginal_property_tax_rate)) + 
  geom_point() + labs(title = "County A: Scatter Plot of Tax Rate vs. Income", 
                      x = "Household Income", y = "Marginal Property Tax Rate")
ggplot(county_B, aes(x = household_income, y = marginal_property_tax_rate)) + 
  geom_point() + labs(title = "County B: Scatter Plot of Tax Rate vs. Income", 
                      x = "Household Income", y = "Marginal Property Tax Rate")
ggplot(county_C, aes(x = household_income, y = marginal_property_tax_rate)) + 
  geom_point() + labs(title = "County C: Scatter Plot of Tax Rate vs. Income", 
                      x = "Household Income", y = "Marginal Property Tax Rate")

ggplot(county_A, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County A: Histogram of Household Income", x = "Household Income", y = "Count")
ggplot(county_B, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County B: Histogram of Household Income", x = "Household Income", y = "Count")
ggplot(county_C, aes(x = household_income)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white") + 
  labs(title = "County C: Histogram of Household Income", x = "Household Income", y = "Count")

county_A <- county_A %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 
county_B <- county_B %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 
county_C <- county_C %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 

ggplot(county_A, aes(x = year_of_home_purchase, y = high_tax_dummy)) + geom_point() + 
  labs(title = "County A: Probability of High Tax Rate vs. Year of Home Purchase", 
       x = "Year of Home Purchase", y = "Probability of High Tax Rate")
ggplot(county_B, aes(x = year_of_home_purchase, y = high_tax_dummy)) + 
  geom_point() + labs(title = "County B: Probability of High Tax Rate vs. Year of Home Purchase", 
                      x = "Year of Home Purchase", y = "Probability of High Tax Rate")
ggplot(county_C, aes(x = year_of_home_purchase, y = high_tax_dummy)) + 
  geom_point() + labs(title = "County C: Probability of High Tax Rate vs. Year of Home Purchase", 
                      x = "Year of Home Purchase", y = "Probability of High Tax Rate")

county_A_rdd <- lm(evades_taxes_yn ~ high_tax_dummy, data = county_A) 
summary(county_A_rdd) 
coeftest(county_A_rdd, vcov. = sandwich)

county_B_lm <- lm(evades_taxes_yn ~ marginal_property_tax_rate, data = county_B) 
summary(county_B_lm) 
coeftest(county_B_lm, vcov. = sandwich)

county_C_lm <- lm(evades_taxes_yn ~ marginal_property_tax_rate, data = county_C) 
summary(county_C_lm) 
coeftest(county_C_lm, vcov. = sandwich)
