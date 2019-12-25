# Comparing an ROC curve using the caret package

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
library(pROC) # install.packages("pROC")

# Set-up training control with 10-fold cross-validation, 3 repetitions
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

# Train using GLM (Generalized Linear Model)
glm.model <- train(churn ~ .,
                   data = trainset,
                   method = "glm",
                   metric = "ROC",
                   trControl = control)

# Train using SVM (Support Vector Machines)
svm.model <- train(churn ~ .,
                   data = trainset,
                   method = "svmRadial",
                   metric = "ROC",
                   trControl = control)

# Train using RPART (Recursive Partitioning and Regression Trees)
rpart.model <- train(churn ~ .,
                     data = trainset,
                     method = "rpart",
                     metric = "ROC",
                     trControl = control)

# Make the predictions using the different models
glm.probs <- predict(glm.model, testset[, !names(testset) %in% c("churn")], type = "prob")
svm.probs <- predict(svm.model, testset[, !names(testset) %in% c("churn")], type = "prob")
rpart.probs <- predict(rpart.model, testset[, !names(testset) %in% c("churn")], type = "prob")

# Generate the ROC curve of each model
glm.ROC <- roc(response = testset[, c("churn")],
               predictor = glm.probs$yes,
               levels = levels(testset[, c("churn")]))
svm.ROC <- roc(response = testset[, c("churn")],
               predictor = svm.probs$yes,
               levels = levels(testset[, c("churn")]))
rpart.ROC <- roc(response = testset[, c("churn")],
                 predictor = rpart.probs$yes,
                 levels = levels(testset[, c("churn")]))

plot(glm.ROC, type = "S", col = "red")
plot(svm.ROC, add = TRUE, col = "green")
plot(rpart.ROC, add = TRUE, col = "blue")

glm.ROC
svm.ROC
rpart.ROC



















