# Performing cross-validation with the boosting method

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
set.seed(2)

# Perform a cross-validation using boosting.cv function
churn.boostcv <- boosting.cv(churn ~ .,
                             v = 10, # 10-fold
                             data = trainset,
                             mfinal = 5, # 5 repetitions
                             control = rpart.control(cp = 0.01))

# Obtain the confusion matrix from the results
churn.boostcv$confusion

# Obtain the average errors of the boosting method
churn.boostcv$error
