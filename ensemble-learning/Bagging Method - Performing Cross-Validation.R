# Performing cross-validation with the bagging method

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

library(adabag)

# Use the bagging.cv to make a 10-fold classification with 10 iterations
churn.baggingcv <- bagging.cv(churn ~ ., v = 10, data = trainset, mfinal = 10)

# Obtain the confusion matrix from the cross-validation results
churn.baggingcv$confusion

# Obtain the minimum estimation errors from the CV results
churn.baggingcv$error
