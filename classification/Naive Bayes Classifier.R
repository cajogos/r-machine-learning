# Classifying data with the Naïve Bayes classifier

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

library(e1071) # install.packages("e1071")

classifier <- naiveBayes(trainset[, !names(trainset) %in% c("churn")],
                         trainset$churn)
classifier

# Generate the classification table
bayes.table <- table(predict(classifier, testset[, !names(testset) %in% c("churn")]),
                     testset$churn)
bayes.table

# Generate a confusion matrix
library(caret)
confusionMatrix(bayes.table)
