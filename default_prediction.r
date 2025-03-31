
install.packages("usethis")
library(fredr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(usethis)
library(caret)
library(pROC)
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
  summarise(
    count = n()
  ) %>%
  mutate(
    proportion = count / sum(count)
  )

# Train data class sizing and ratio
# A tibble: 2 × 3
# default count proportion
# <int> <int>      <dbl>
# 1       0   527      0.659
# 2       1   273      0.341

# Getting the class sizing for the test data.
test_data %>%
  group_by(default) %>% 
  summarise(
    count = n()
  ) %>%
  mutate(
    proportion = count / sum(count)
  )

# Test data class sizing and proportion
# A tibble: 2 × 3
# default count proportion
# <int> <int>      <dbl>
# 1       0   138       0.69
# 2       1    62       0.31

# Given that there is only a 3% difference in the class sizing for the train/test split, I will not redo this split.



# Setting up a logistic regression model to predict default status on the training data using all features except the ratio to prevent any multicollinearity.
model <- glm(default ~ mortgage_rate + credit_score + loan_amount,
             data = train_data, family = binomial)

summary(model)


# Logistic model summary for the prediction of default status on the train_data
# Call:
#   glm(formula = default ~ mortgage_rate + credit_score + loan_amount, 
#       family = binomial, data = train_data)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -4.380e+00  1.285e+00  -3.408 0.000654 ***
#   mortgage_rate  4.706e-01  5.885e-02   7.997 1.27e-15 ***
#   credit_score  -2.712e-03  1.824e-03  -1.487 0.137048    
# loan_amount    1.034e-05  8.777e-07  11.780  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1026.98  on 799  degrees of freedom
# Residual deviance:  780.52  on 796  degrees of freedom
# AIC: 788.52
# 
# Number of Fisher Scoring iterations: 5

##########################
# As expected, the credit score feature is not significantly different from zero and does not have as much bearing on default as the other features.
# The mortgage_rate, and loan amount are significant for an alpha up to 0.01. 
# From here I will proceed to run the same model on the testing data and fine tune the threshold to enhance accuracy.

# Getting the prediction probabilities on the test_data.
test_data$pred_prob <- predict(model, newdata = test_data, type = "response")

# Plotting the probability values vs default

ggplot(test_data, aes(x = default, y = pred_prob)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(
    title = "Predicted Probability of Default vs Default Status - No Class Weights",
    x = "Default", 
    y = "Predicted Probability of Default"
  ) + 
  theme_minimal()

# Based on the plot, to get the maximal value in accuracy from the model
# I am going to use a baseline threshold value of 0.46 and finalize a value based on accuracy.
test_data$pred_class <- ifelse(test_data$pred_prob > 0.46, 1, 0)
test_data$pred_class <- factor(test_data$pred_class, levels = c(0, 1))
test_data$default <- factor(test_data$default, levels = c(0, 1))

# Plotting a confusion matrix to see the results.
cm <- confusionMatrix(
  test_data$pred_class, 
  test_data$default, 
  positive = "1"
)

cm

# Confusion Matrix and Statistics - No class weights
# 
# Reference
# Prediction   0   1
# 0 117  13
# 1  21  49
# 
# Accuracy : 0.83            
# 95% CI : (0.7706, 0.8793)
# No Information Rate : 0.69            
# P-Value [Acc > NIR] : 4.79e-06        
# 
# Kappa : 0.6163          
# 
# Mcnemar's Test P-Value : 0.2299          
#                                           
#             Sensitivity : 0.7903          
#             Specificity : 0.8478          
#          Pos Pred Value : 0.7000          
#          Neg Pred Value : 0.9000          
#              Prevalence : 0.3100          
#          Detection Rate : 0.2450          
#    Detection Prevalence : 0.3500          
#       Balanced Accuracy : 0.8191          
#                                           
#        'Positive' Class : 1 
# 0.46 Ended up having the best results, based on accuracy.

# Getting the f1-score because there is a slight class imbalance.
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]

f1 <-2*(precision*recall)/(precision+recall)

data.frame(
  Metric = c("Precision", "Recall", "F1-Score"),
  Value = c(precision, recall, f1))

# Metric     Value
# 1 Precision 0.7000000
# 2    Recall 0.7903226
# 3  F1-Score 0.7424242

conf_mat <- table(Predicted = test_data$pred_class, Actual = test_data$default)
conf_mat_df <- as.data.frame(conf_mat)

ggplot(conf_mat_df, aes(x = Actual, y = Predicted, fill=Freq))+
  geom_tile(color = "white") +
  geom_text(aes(label=Freq), size = 6, color="black")+
  scale_fill_gradient(low = "lightblue", high = "steelblue")+
  labs(
    title = "Confusion Matrix - No Class Weights", 
    x = "Actual Default", 
    y = "Predicted Default"
  ) + 
  theme_minimal(base_size = 14)

# Based on the results from this model, the performance is pretty solid,
# the overall accuracy is around .82 and the f1-score is .74. I did not account for the class
# imbalance through class weighting becuase I wanted to see what a baseline model would do.
# I am going to run another model with class weights included and plot them both on the roc curve
# to see how they measure up against each other.

# Adding the weight column to give more weight to the minority class.
train_data$weight <- ifelse(train_data$default == 1, 
                            1 / mean(train_data$default), # Weight for the default (minority)
                            1/ (1-mean(train_data$default)))# Weight for non-default (majority)
train_data$default <- as.integer(train_data$default)  # or use `as.factor()` if preferred

# Fit another model including class weights
model_weighted <- glm(default ~ mortgage_rate + credit_score + loan_amount,
                      data = train_data, 
                      family = binomial, 
                      weights = weight)

summary(model_weighted)

# Call:
#   glm(formula = default ~ mortgage_rate + credit_score + loan_amount, 
#       family = binomial, data = train_data, weights = weight)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -3.548e+00  8.557e-01  -4.146 3.38e-05 ***
#   mortgage_rate  4.624e-01  4.037e-02  11.453  < 2e-16 ***
#   credit_score  -2.878e-03  1.221e-03  -2.357   0.0184 *  
#   loan_amount    1.027e-05  5.886e-07  17.447  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2218.1  on 799  degrees of freedom
# Residual deviance: 1678.9  on 796  degrees of freedom
# AIC: 1972.1
# 
# Number of Fisher Scoring iterations: 4

##############################
# This model shows some slightly different results. After including class weights in the model,
# it seems that credit score has become a more significant predictor for default status.
# It is now significant for an alpha value of 0.05. Perhaps this will enhance the predictive power of the model
# on the test data.

# Getting the new probabilities for the test data.
test_data$pred_prob_weights <- predict(model_weighted, newdata = test_data, type="response")

# Plotting the probability values vs default for the weighted model.

ggplot(test_data, aes(x = default, y = pred_prob_weights)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  labs(
    title = "Predicted Probability of Default vs Default Status - Class Weights",
    x = "Default", 
    y = "Predicted Probability of Default"
  ) + 
  theme_minimal()

# This plot looks like it pushed the default bin up a bit higher so I will be using a threshold of 0.625 to start.

test_data$pred_class_weights <- ifelse(test_data$pred_prob_weights > 0.59, 1, 0)

test_data$pred_class_weights <- factor(test_data$pred_class_weights, levels = c(0, 1))
test_data$default <- factor(test_data$default, levels = c(0, 1))



cm_weights <- confusionMatrix(
  test_data$pred_class_weights, 
  test_data$default, 
  positive = "1"
)

cm_weights

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 115  12
# 1  23  50
# 
# Accuracy : 0.825          
# 95% CI : (0.7651, 0.875)
# No Information Rate : 0.69           
# P-Value [Acc > NIR] : 1.049e-05      
# 
# Kappa : 0.61           
# 
# Mcnemar's Test P-Value : 0.09097        
#                                          
#             Sensitivity : 0.8065         
#             Specificity : 0.8333         
#          Pos Pred Value : 0.6849         
#          Neg Pred Value : 0.9055         
#              Prevalence : 0.3100         
#          Detection Rate : 0.2500         
#    Detection Prevalence : 0.3650         
#       Balanced Accuracy : 0.8199         
#                                          
#        'Positive' Class : 1 

# The final threshold was 0.59 which showed the best results with only marginal improvement over the other model.
# I could run two new models using both over/under sampling to try to improve the results.
# I could also include more features to try and improve the model.
# For now I am going to finish gettign the precision, recall and f1, then plot on the roc curve to compare each model.

precision_weights <- cm_weights$byClass["Precision"]
recall_weights <- cm_weights$byClass["Recall"]
f1_weights <- 2*(precision_weights*recall_weights)/(precision_weights+recall_weights)

data.frame(
  Metric = c("Precision", "Recall", "F1-Score"), 
  Value = c(precision_weights, recall_weights, f1_weights)
)

# Metric     Value
# 1 Precision 0.6849315
# 2    Recall 0.8064516
# 3  F1-Score 0.7407407

# The tradeoff in these metrics from these models is from the slight change in precision vs recall.
# This second model has a slightly higher recall, so if the (fictitious) company
# were to value False Negatives (missing a default) over false positives (falsely predicting default)
# they should go with the second model over the first, even though it has a slightly lower precision and F1-Score.


# Plotting the confusion matrix for the weighted model
conf_mat_weights <- table(Predicted = test_data$pred_class_weights, Actual = test_data$default)
conf_mat_df <- as.data.frame(conf_mat_weights)

ggplot(conf_mat_df, aes(x = Actual, y = Predicted, fill=Freq))+
  geom_tile(color = "white") +
  geom_text(aes(label=Freq), size = 6, color="black")+
  scale_fill_gradient(low = "salmon", high = "red")+
  labs(
    title = "Confusion Matrix - Class Weights", 
    x = "Actual Default", 
    y = "Predicted Default"
  ) + 
  theme_minimal(base_size = 14)


# Plotting both models on the ROC curve.
roc_unweighted <- roc(test_data$default, test_data$pred_prob, levels = c(0,1))
roc_weighted <- roc(test_data$default, test_data$pred_prob_weights, levels = c(0, 1))


plot(roc_unweighted, col = "#1f77b4", lwd = 2, main = "ROC Curve: Unweighted vs Weighted Models")
plot(roc_weighted, col = "#d62728", lwd = 2, add = TRUE)

legend("bottomright", legend = c("Unweighted", "Weighted"),
       col = c("#1f77b4", "#d62728"), lwd = 2)





