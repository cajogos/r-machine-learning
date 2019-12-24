# Ranking the variable importance with the caret package

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
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(churn ~ ., data = trainset, method = "rpart", preProcess = "scale", trControl = control)

# Estimate the variable importance with the varImp function
importance <- varImp(model, scale = FALSE)
importance

# Plot the variable importance
plot(importance)

# NOTE: Also can obtain information from rpart
library(rpart)
model.rp <- rpart(churn ~ ., data = trainset)
model.rp$variable.importance
