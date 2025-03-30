
library(fredr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# Creating some data to make predictions and manipulate
loans <- tibble(
  borrower_id = 1:1000,
  credit_score = round(rnorm(1000, 680, 50)), 
  loan_amount = round(runif(1000, 50000, 300000), -3), 
  mortgage_rate = round(rnorm(1000, 5, 0.5), 2), 
  default = rbinom(1000, 1, 0.07)
)

Filter to borrowers with credit_score < 600 who did default.

Calculate average loan amount and default rate by credit score band (<600, 600-700, >700).

Add a column for loan_to_rate_ratio = loan_amount / mortgage_rate.

Group by default and calculate average loan_to_rate_ratio.

# Setting up the api key
fredr_set_key("6c33d7d0d7fc3f739a751f2c742b9ae4")


# Pulling freddie Mac 30-Year fixrd mortgage rates (weekly data)
mortgage_data <-fredr(
  series_id ="MORTGAGE30US",
  observation_start = as.Date("2018-01-01"), 
  frequency = "w"
)

mortgage_data_clean <- mortgage_data %>%
  select(date, value) %>%
  rename(mortgage_rate = value)


head(mortgage_data_clean)


ggplot(mortgage_data_clean, aes(date, mortgage_rate))+
  geom_line()



