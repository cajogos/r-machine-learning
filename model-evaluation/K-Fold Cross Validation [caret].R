# Performing cross-validation with the caret package

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

library(caret)

# Set-up control to train 10-fold cross validation, with 3 repetitions (Repeated K-Fold Validation)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Train classification model (rpart method)
model <- train(churn ~ .,
               data = trainset,
               method = "rpart",
               preProcess = "scale",
               trControl = control)

# Examine the output of the model
model
