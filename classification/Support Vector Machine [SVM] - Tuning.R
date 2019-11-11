# Tuning a support vector machine

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

# Tune the support vector machine
tuned <- tune.svm(churn ~ ., data = trainset, gamma = 10^(-6:-1), cost = 10^(1:2))

summary(tuned)

# Retrain the model using the tuning result
model.tuned <- svm(churn ~ ., data = trainset,
                   gamma = tuned$best.parameters$gamma,
                   cost = tuned$best.parameters$cost)
summary(model.tuned)

# Predict labels
svm.tuned.pred <- predict(model.tuned, testset[, !names(testset) %in% c("churn")])

# Generate the classification table
svm.tuned.table <- table(svm.tuned.pred, testset$churn)
svm.tuned.table

# Generate the class agreement
classAgreement(svm.tuned.table)

# Confusion matrix to measure the performance of the retrained model
library(caret)
confusionMatrix(svm.tuned.table)
