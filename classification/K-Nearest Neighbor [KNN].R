# Classifying data with the k-nearest neighbor classifier

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

library(class) # install.packages("class")

# Replace yes and no with 1 and 0
levels(trainset$international_plan) <- list("0" = "no", "1" = "yes")
levels(trainset$voice_mail_plan) <- list("0" = "no", "1" = "yes")
levels(testset$international_plan) <- list("0" = "no", "1" = "yes")
levels(testset$voice_mail_plan) <- list("0" = "no", "1" = "yes")

# Use the KNN classification method on the training and the testing dataset
churn.knn <- knn(trainset[, ! names(trainset) %in% c("churn")],
                 testset[, ! names(testset) %in% c("churn")],
                 trainset$churn, k = 3)

# Use summary function to retrieve the number of predicted tables
summary(churn.knn)

# Generate classification matrix
table(testset$churn, churn.knn)

# Generate a confusion matrix
library(caret)
confusionMatrix(table(testset$churn, churn.knn))
