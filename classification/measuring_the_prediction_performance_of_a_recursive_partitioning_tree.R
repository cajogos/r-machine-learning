# Measuring the prediction performance of a recursive partitioning tree

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

library(rpart)

# Build a classification tree model
churn.rp = rpart(churn ~ ., data = trainset)

# Generate a predicted label of testing the dataset
predictions <- predict(churn.rp, testset, type = "class")

# Generate a classification table for the dataset
table(testset$churn, predictions)

# Use the confusion matrix provided by the caret package
library(caret)

confusionMatrix(table(predictions, testset$churn))
