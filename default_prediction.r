
install.packages("usethis")
library(fredr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(usethis)
edit_r_environ()
readRenviron(".Renviron")

# Setting up the api key
fredr_set_key(Sys.getenv("FRED_API_KEY"))


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

set.seed(42)
# Creating some simulated data to run some EDA and modeling on.
loans <- mortgage_data_clean %>%
  slice_sample(n = 1000, replace = TRUE) %>%
  mutate(
    loan_amount = round(runif(n(), 100000, 500000), -3), # Uniform distribution from 1-5 hundred thousand.
    credit_score = pmin(pmax(round(rnorm(n(), 680, 50)), 300), 850), # Normally distributed with a mean of 680 and sd of 50.
    default = rbinom(n(), 1, prob = plogis(-5 + 0.5 * mortgage_rate - 0.002 * credit_score + 0.00001 * loan_amount)) # Target following a binomial distribution and a designated formula from the other features.                 
  )

head(loans)
  
# Terminal Output for Loans
# A tibble: 6 × 5
# date       mortgage_rate loan_amount credit_score default
# <date>             <dbl>       <dbl>        <dbl>   <int>
# 1 2018-12-06          4.75      306000          670       0
# 2 2024-02-22          6.9       405000          762       1
# 3 2020-12-03          2.71      207000          636       0
# 4 2019-05-30          3.99      375000          702       0
# 5 2022-05-12          5.3       323000          772       0
# 6 2020-10-15          2.81      272000          591       0

# Filter to borrowers with credit_score < 600 who did default.
loans %>%
  filter(credit_score < 600, default == 1)

# # A tibble: 49 × 5
# date       mortgage_rate loan_amount credit_score default
# <date>             <dbl>       <dbl>        <dbl>   <int>
# 1 2023-06-29          6.71      241000          591       1
# 2 2018-04-05          4.4       257000          594       1
# 3 2022-10-13          6.92      157000          596       1
# 4 2019-07-25          3.75      398000          579       1
# 5 2018-07-05          4.52      498000          598       1
# 6 2018-04-12          4.42      165000          597       1
# 7 2023-02-09          6.12      377000          589       1
# 8 2020-11-05          2.78      494000          584       1
# 9 2024-09-26          6.08      422000          580       1
# 10 2020-03-05          3.29      436000          597       1
# 11 2024-08-15          6.49      326000          596       1
# 12 2025-03-27          6.65      489000          572       1
# 13 2020-01-02          3.72      327000          590       1
# 14 2024-01-04          6.62      349000          587       1
# 15 2025-02-27          6.76      312000          585       1
# 16 2025-03-13          6.65      390000          512       1
# 17 2021-12-30          3.11      330000          590       1

# Calculate average loan amount and default rate by credit score band (<600, 600-700, >700).
loans <- loans %>%
  mutate(score_band = case_when(
    credit_score < 600 ~ "<600", 
    credit_score >=600 & credit_score <= 700 ~ "600-700", 
    credit_score > 700 ~ ">700"
  ))

head(loans)

# A tibble: 6 × 6
# date       mortgage_rate loan_amount credit_score default score_band
# <date>             <dbl>       <dbl>        <dbl>   <int> <chr>     
# 1 2018-12-06          4.75      306000          670       0 600-700   
# 2 2024-02-22          6.9       405000          762       1 >700      
# 3 2020-12-03          2.71      207000          636       0 600-700   
# 4 2019-05-30          3.99      375000          702       0 >700      
# 5 2022-05-12          5.3       323000          772       0 >700      
# 6 2020-10-15          2.81      272000          591       0 <600  

loans %>% 
  group_by(score_band) %>%
  summarise(
    default_rate = mean(default), 
    avg_loan_amount = mean(loan_amount), 
    count = n()
  )

# Statistice
# A tibble: 3 × 4
# score_band default_rate avg_loan_amount count
# <chr>             <dbl>           <dbl> <int>
# 1 600-700           0.354         304347.   570
# 2 <600              0.347         269653.    49
# 3 >700              0.304         297724.   381

# As we can see above, the group with the highest defualt rate is the group with credit scores
# ranging from 600-700. Now, this doesn't neccessarily make sense because we would expect the
# borrowers with lower credit scores to default more often. However, it is notable that the 600-700 band
# also has the highest average loan amount. Now I am going to dig a bit deeper to understand how loans and rates relate among these groups.


# Adding a column for loan_to_rate_ratio = loan_amount / mortgage_rate.

loans <- loans %>%
  mutate(
    loan_to_rate_ratio = loan_amount / mortgage_rate
  )

head(loans)


# Grouping by score_band and calculating average loan_to_rate_ratio.

loans %>%
  group_by(score_band) %>%
  summarise(
    default_rate = mean(default), 
    avg_loan_to_rate_ratio = mean(loan_to_rate_ratio), 
    avg_loan_amount = mean(loan_amount), 
    count = n()
  )

# A tibble: 3 × 5
# score_band default_rate avg_loan_to_rate_ratio avg_loan_amount count
# <chr>             <dbl>                  <dbl>           <dbl> <int>
# 1 600-700           0.354                 70974.         304347.   570
# 2 <600              0.347                 65030.         269653.    49
# 3 >700              0.304                 70531.         297724.   381

# Above we can see also that the same group has the higher avg_loan_to_rate_ratio as well.
# This is a bit unexpected. Based on this, when we run the model, it is likely that credit score will not be
# a very significant predictor for default. Now to split the data for modeling.

# Setting up the split
n <- nrow(loans)
train_index <- sample(1:n, size = 0.8*n)

# Splitting the data into train and test only since I will not be doing any cross validation for this first model.
train_data <- loans[train_index,]
test_data <- loans[-train_index,]

# Getting the class size for the target in the training data.
train_data %>%
  group_by(default) %>%
  

# Setting up a logistic regression model to predict default status on the training data using all features except the ratio to prevent any multicollinearity.
model <- glm(default ~ mortgage_rate + credit_score + loan_amount,
             data = train_data, family = binomial)








