# Estimating the prediction errors of different classifiers

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

library(ipred)

# Estimate the error of the bagging model
churn.bagging <- errorest(churn ~ ., data = trainset, model = bagging)
churn.bagging

# Estimate the error of the boosting model
library(ada)
churn.boosting <- errorest(churn ~ ., data = trainset, model = ada)
churn.boosting

# Estimate the error of the Random Forest model
churn.rf <- errorest(churn ~ ., data = trainset, model = randomForest)
churn.rf

# Estimate the error of the Single Decision Tree model
churn.predict <- function(object, newdata)
{
  predict(object, newdata = newdata, type = "class")
}
churn.tree <- errorest(churn ~ ., data = trainset, model = rpart, predict = churn.predict)
churn.tree
