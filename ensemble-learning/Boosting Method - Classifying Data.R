# Classifying data with the boosting method

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

library(adabag)
set.seed(2)

# Train the classification model using boosting function
churn.boost <- boosting(churn ~ .,
                        data = trainset,
                        mfinal = 10, # Number of iterations
                        coeflearn = "Freund",
                        boos = FALSE,
                        control = rpart.control(maxdepth = 3))

# Make predictions based on boosted model
churn.boost.pred <- predict.boosting(churn.boost, newdata = testset)

# Obtain the classification table
churn.boost.pred$confusion

# Obtain the average errors
churn.boost.pred$error

# use the caret package to perform a classification with the boosting method
library(caret)
library(mboost) # install.packages("mboost")
library(pROC)

# Set the training control and use the train method with adaboost
ctrl = trainControl(method = "repeatedcv",
                    repeats = 1,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

ada.train <-train(churn ~ .,
                  data = trainset,
                  method = "ada",
                  metric = "ROC",
                  trControl = ctrl)


# Use the summary to obtain the details of the classification model
ada.train$result

# Plot the ROC curve within different iterations
plot(ada.train)

# Make predictions on the model
ada.predict <- predict(ada.train, testset, "prob")
ada.predict.result <- ifelse(ada.predict[1] > 0.5, "yes", "no")

# Obtain the classification table
table(testset$churn, ada.predict.result)
