# Classifying data with gradient boosting

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
set.seed(2)

library(gbm) # install.packages("gbm")

# Transform yes/no into 1/0 (gbm only supports 1s and 0s)
trainset$churn <- ifelse(trainset$churn == "yes", 1, 0)

# Use the gbm function to train dataset
churn.gbm <- gbm(formula = churn ~ .,
                 distribution = "bernoulli",
                 data = trainset,
                 n.trees = 1000,
                 interaction.depth = 7,
                 shrinkage = 0.01,
                 cv.folds = 3)

# Obtain the summary information from the model
summary(churn.gbm)

# Obtain the best iteration using CV
churn.iter <- gbm.perf(churn.gbm, method = "cv")

# Retrieve the odd value of the log returned by the Bernoulli loss
churn.predict <- predict(churn.gbm, testset, n.trees = churn.iter)
str(churn.predict)

# Plot the ROC curve and get the best cut off with maximum accuracy
churn.roc <- roc(testset$churn, churn.predict)
plot(churn.roc)

churn.roc

# Retrieve the best cut off with the coords function and use it to obtain the predicted label
coords(churn.roc, "best", transpose = TRUE)
churn.predict.class <- ifelse(churn.predict > coords(churn.roc, "best", transpose = TRUE)["threshold"], "yes", "no")

# Obtain the classification table
table(testset$churn, churn.predict.class)

# use the mboost package to perform classifications with the gradient boosting method
library(mboost)

# Remove non-numerical attributes
trainset$voice_mail_plan <- NULL
trainset$international_plan <- NULL

# Train classification model with mboost
churn.mboost <- mboost(churn ~ .,
                       data = trainset,
                       control = boost_control(mstop = 10))

# Obtain the details of classification model
summary(churn.mboost)

# Draw a partial contribution plot of each attribute
par(mfrow = c(1, 2))
plot(churn.mboost)














