# Performing cross-validation with the e1071 package

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

# Apply the tune.svm function with 10-fold cross validation
tuned <- tune.svm(churn ~ .,
                  data = trainset,
                  gamma = 10^-2,
                  cost = 10^2,
                  tunecontrol = tune.control(cross = 10))

# Obtain the information from the tuned model
summary(tuned)

# The performance details
tuned$performances

# Use optimum model to generate a classification table
svmfit <- tuned$best.model
table(trainset[, c("churn")], predict(svmfit))
