# Classifying data with the bagging method

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

# --- The prepared churn dataset --- #
library(C50)
data(churn)
churnTrain <- churnTrain[, ! names(churnTrain) %in% c("state", "area_code", "account_length")]
set.seed(2)
ind <- sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))
trainset <- churnTrain[ind == 1,]
testset <- churnTrain[ind == 2, ]
# ------ #

library(adabag) # install.packages("adabag")

set.seed(2)

# Use bagging function to train a training dataset
churn.bagging <- bagging(churn ~ ., data = trainset, mfinal = 10)

# Access the importance variable
churn.bagging$importance

# Predict the results from testing dataset
churn.predbagging <- predict.bagging(churn.bagging, newdata = testset)

# Obtain the classification table
churn.predbagging$confusion

# Retrieve average error
churn.predbagging$error

# ipred package provides a bagging method for a classification tree
library(ipred) # install.packages("ipred")

# Bagging method to fit the classification method
churn.bagging <- bagging(churn ~ ., data = trainset, coob = TRUE)
churn.bagging

# Obtain an out of bag estimate of misclassification of the errors
mean(predict(churn.bagging) != trainset$churn)

# Find the predicted labels of the testing dataset
churn.prediction <- predict(churn.bagging, newdata = testset, type = "class")

# Obtain the classification tables
prediction.table <- table(churn.prediction, testset$churn)
prediction.table
