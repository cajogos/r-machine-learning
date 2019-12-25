# Measuring prediction performance with a confusion matrix

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

# Train an SVM model
svm.model <- train(churn ~ ., data = trainset, method = "svmRadial")

# Predict labels using the new model
svm.pred <- predict(svm.model, testset[, ! names(testset) %in% c("churn")])

# Generate the classification table
table(svm.pred, testset[, c("churn")])

# Generate the confusion matrix from caret package
library(caret)
confusionMatrix(svm.pred, testset[, c("churn")])
