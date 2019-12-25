# Measuring performance differences between models with the caret package

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

# NOTE: Models below come from "Comparing ROC Curve" file
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
glm.model <- train(churn ~ ., data = trainset, method = "glm", metric = "ROC", trControl = control)
svm.model <- train(churn ~ ., data = trainset, method = "svmRadial", metric = "ROC", trControl = control)
rpart.model <- train(churn ~ ., data = trainset, method = "rpart", metric = "ROC", trControl = control)

# Resample the three models - To generate the statistics
cv.values <- resamples(list(glm = glm.model, svm = svm.model, rpart = rpart.model))

# Obtain the summary of the resampling
summary(cv.values)

# Plot the resamplic result using dotplot (with ROC metric)
dotplot(cv.values, metric = "ROC")

# Plot again using a box-whisker plot
bwplot(cv.values, layout = c(3, 1))
