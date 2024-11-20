##PART 3 of Final Exam: Tax Rates##
#Understanding consequences of a higher marginal property tax rate 


setwd("/Users/misbaharshad/Downloads/") 
data <- read.csv("final_exam_2023.csv") 
library(ggplot2) 
library(dplyr) 
library(broom)
library(lmtest)
library(sandwich)
library(ggthemes)
library(AER)
library(stargazer)

county_A <- data %>% filter(county == "ALAMEDA") 
county_B <- data %>% filter(county == "SANTA CLARA") 
county_C <- data %>% filter(county == "YOLO")

#Bunching Test
cutoff_year = 1978 

ggplot(county_A, aes(x = year_of_home_purchase)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red") +
  labs(title = "Bunching Test for Alameda County",
      subtitle = "Density of Year of Home Purchase in Alameda",
       x = "Year of Home Purchase", y = "Count") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(county_B, aes(x = year_of_home_purchase)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red") +
  labs(title = "Bunching Test for Santa Clara County",
      subtitle = "Density of Year of Home Purchase", 
       x = "Year of Home Purchase", y = "Count") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(county_C, aes(x = year_of_home_purchase)) +
  geom_histogram(binwidth = 1, color = "black", fill = "gray") +
  geom_vline(xintercept = cutoff_year, linetype = "dashed", color = "red") +
  labs(title = "Bunching Test for YOLO County",
      subtitle = "Density of Year of Home Purchase", 
       x = "Year of Home Purchase", y = "Count") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

#Smoothness Tests

ggplot(county_A, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for Alameda County", 
  subtitle = "Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(county_B, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for Santa Clara County", 
  subtitle = "Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(county_C, aes(x = year_of_home_purchase, y = household_income)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Smoothness Test for YOLO County", 
       subtitle = "Household Income vs Year of Home Purchase",
       x = "Year of Home Purchase", y = "Household Income") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

#Regression - Treatment Status Test 

county_A <- county_A %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 

county_C <- county_C %>% 
  mutate(high_tax_dummy = ifelse(marginal_property_tax_rate >= 50, 1, 0)) 

tax_rate_by_year_A <- county_A %>%
  group_by(year_of_home_purchase) %>%
  summarise(prob_high_tax = mean(high_tax_dummy, na.rm = TRUE))

tax_rate_by_year_C <- county_C %>%
  group_by(year_of_home_purchase) %>%
  summarise(prob_high_tax = mean(high_tax_dummy, na.rm = TRUE))

ggplot(tax_rate_by_year_A, aes(x = year_of_home_purchase, y = prob_high_tax)) +
  geom_point() +
  geom_line() +
  labs(title = "Treatment Status Test for Alameda County", 
      subtitle = "Probability of High Tax Rate by Year of Home Purchase",
       x = "Year of Home Purchase", y = "Probability of High Tax Rate") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

ggplot(tax_rate_by_year_C, aes(x = year_of_home_purchase, y = prob_high_tax)) +
  geom_point() +
  geom_line() +
  labs(title = "Treatment Status Test for YOLO County", 
      subtitle = "Probability of High Tax Rate by Year of Home Purchase",
       x = "Year of Home Purchase", y = "Probability of High Tax Rate") +
  geom_vline(xintercept = 1978, linetype = "dashed", color = "red") + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

#Alameda County: Fuzzy RD

first_stage <- lm(high_tax_dummy ~ year_of_home_purchase, data = county_A)
stargazer(first_stage, type = "text", title = "First Stage Regression Results", 
          dep.var.labels = "High Tax Rate (Dummy)", 
          covariate.labels = c("Instrument: Year of Home Purchase"),
          out = "first_stage_output.txt")

reduced_form <- lm(evades_taxes_yn ~ year_of_home_purchase, data = county_A)
stargazer(reduced_form, 
          type = "text", 
          title = "Reduced Form Regression Results", 
          dep.var.labels = "Tax Evasion (Yes/No)", 
          covariate.labels = c("Year of Home Purchase"),
          out = "reduced_form_output.txt")

Alameda_Fuzzy_RD <- ivreg(evades_taxes_yn ~ high_tax_dummy | year_of_home_purchase, data = county_A)
stargazer(Alameda_Fuzzy_RD, type = "text", title = "Fuzzy RD Results", 
          dep.var.labels = "Tax Evasion (Yes/No)", 
          covariate.labels = c("High Tax Rate (Dummy)", "Instrument: Year of Home Purchase"),
          out = "fuzzy_rd_output.txt")
summary(Alameda_Fuzzy_RD)

#YOLO County: Sharp RD
YOLO_Sharp_RD <- lm(evades_taxes_yn ~ high_tax_dummy, data = county_C) 
summary(YOLO_Sharp_RD) 
coeftest(YOLO_Sharp_RD, vcov. = sandwich)

