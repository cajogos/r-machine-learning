# Predicting labels based on a model trained by a support vector machine

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

library(e1071)
model <- svm(churn ~ ., data = trainset, kernel = "radial", cost = 1, gamma = 1 / ncol(trainset))

# Predict the label of the testing dataset
svm.pred <- predict(model, testset[, !names(testset) %in% c("churn")])

# Generate classification table
svm.table <- table(svm.pred, testset$churn)
svm.table

# Calculate the coefficients compared to the classification arguments
# diag - Represents the percentage of data points in the main diagonal of the classification table.
# kappa - Refers to diag, which is corrected for an agreement by a change (the probability of random agreements).
# rand - Represents the Rand index, which measures the similarity between two data clusters.
# crand - Indicates the Rand index, which is adjusted for the chance grouping of elements.
classAgreement(svm.table)

# Create confusion matrix to measure the prediction performance
library(caret)
confusionMatrix(svm.table)
