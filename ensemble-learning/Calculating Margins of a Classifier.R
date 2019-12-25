# Calculating the margins of a classifier

# NOTE: A margin is a measure of the certainty of classification.

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

# NOTE: From "Bagging Method - Classifying Data"
churn.bagging <- bagging(churn ~ ., data = trainset, mfinal = 10)
churn.predbagging <- predict.bagging(churn.bagging, newdata = testset)

# NOTE: From "Boosting Method - Classifying Data"
churn.boost <- boosting(churn ~ ., data = trainset, mfinal = 10, coeflearn = "Freund", boos = FALSE, control = rpart.control(maxdepth = 3))
churn.boost.pred <- predict.boosting(churn.boost, newdata = testset)

# Calculating the margins of the boosting classifiers
boost.margins <- margins(churn.boost, trainset)
boost.pred.margins <- margins(churn.boost.pred, testset)

# Plot a marginal cumulative distribution graph of the boosting classifiers
plot(sort(boost.margins[[1]]),
     (1:length(boost.margins[[1]])) / length(boost.margins[[1]]),
     type = "l",
     xlim = c(-1, 1),
     main = "Boosting: Margin cumulative distribution graph",
     xlab = "margin",
     ylab = "% observations",
     col = "blue")
lines(sort(boost.pred.margins[[1]]),
      (1:length(boost.pred.margins[[1]])) / length(boost.pred.margins[[1]]),
      type = "l",
      col = "green")
abline(v = 0, col = "red", lty = 2, lw = 2)

# Calculate percentage of negative margin matches training errors
boosting.training.margin <- table(boost.margins[[1]] > 0)
boosting.negative.training <- as.numeric(boosting.training.margin[1] / boosting.training.margin[2])
boosting.negative.training

# Calculate percentage of negative margin matches test errors
boosting.testing.margin <- table(boost.pred.margins[[1]] > 0)
boosting.negative.testing <- as.numeric(boosting.testing.margin[1] / boosting.testing.margin[2])
boosting.negative.testing

# Calculate the margins for bagging classifiers
bagging.margins <- margins(churn.bagging, trainset)
bagging.pred.margins <- margins(churn.predbagging, testset)

# Plot a margin cumulative distribution graph of the bagging classifiers
plot(sort(bagging.margins[[1]]),
     (1:length(bagging.margins[[1]])) / length(bagging.margins[[1]]),
     type = "l",
     xlim = c(-1, 1),
     main = "Bagging: Margin cumulative distribution graph",
     xlab = "margin",
     ylab = "% observations",
     col = "blue")
lines(sort(bagging.pred.margins[[1]]),
      (1:length(bagging.pred.margins[[1]])) / length(bagging.pred.margins[[1]]),
      type = "l",
      col = "green")
abline(v = 0, col = "red", lty = 2, lw = 2)

# Calculate percentage of negative margin matches training errors
bagging.training.margin <- table(bagging.margins[[1]] > 0)
bagging.negative.training <- as.numeric(bagging.training.margin[1] / bagging.training.margin[2])
bagging.negative.training

# Calculate percentage of negative margin matches test errors
bagging.testing.margin <- table(bagging.pred.margins[[1]] > 0)
bagging.negative.testing <- as.numeric(bagging.testing.margin[1] / bagging.testing.margin[2])
bagging.negative.testing
