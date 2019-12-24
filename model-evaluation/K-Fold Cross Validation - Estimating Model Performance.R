# Estimating model performance with k-fold cross-validation

library(C50)
library(e1071)
data(churn)
churnTrain <- churnTrain[, ! names(churnTrain) %in% c("state", "area_code", "account_length")]

set.seed(2)

# Split the index into 10 fold - using cut function
ind <- cut(1:nrow(churnTrain), breaks = 10, labels = F)


# Loop to perform 10 fold cross-validation
accuracies <- c()
for (i in 1:10) {
  fit <- svm(churn ~ ., churnTrain[ind != i, ]) # Choose the model in this line (i.e. svm)
  predictions <- predict(fit, churnTrain[ind == i, ! names(churnTrain) %in% c("churn")])
  correct_count <- sum(predictions == churnTrain[ind == i, c("churn")])
  accuracies <- append(correct_count / nrow(churnTrain[ind == i, ]), accuracies)
}

# Print the accuracies
accuracies

# Generate the average of the accuracies
mean(accuracies)
