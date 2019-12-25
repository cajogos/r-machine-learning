# Calculating the error evolution of the ensemble method

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

# NOTE: From "Boosting Method - Classifying Data"
churn.boost <- boosting(churn ~ ., data = trainset, mfinal = 10, coeflearn = "Freund", boos = FALSE, control = rpart.control(maxdepth = 3))

# Calculate the error evolution of the boosting classifiers
boosting.evol.train <- errorevol(churn.boost, trainset)
boosting.evol.test <- errorevol(churn.boost, testset)
plot(boosting.evol.test$error,
     type = "l",
     ylim = c(0, 1),
     main = "Boosting error versus number of trees",
     xlab = "Iterations",
     ylab = "Error",
     col = "red",
     lwd = 2)
lines(boosting.evol.train$error,
      cex = 0.5,
      col = "blue",
      lty = 2,
      lwd = 2)
legend("topright",
       c("test", "train"),
       col = c("red", "blue"),
       lty = 1:2,
       lwd = 2)

# Calculate the error evolution of the bagging classifiers
bagging.evol.train <- errorevol(churn.bagging, trainset)
bagging.evol.test <- errorevol(churn.bagging, testset)
plot(bagging.evol.test$error,
     type = "l",
     ylim = c(0, 1),
     main = "Bagging error versus number of trees",
     xlab = "Iterations",
     ylab = "Error",
     col = "red",
     lwd = 2)
lines(bagging.evol.train$error,
      cex = 0.5,
      col = "blue",
      lty = 2,
      lwd = 2)
legend("topright",
       c("test", "train"),
       col = c("red", "blue"),
       lty = 1:2,
       lwd = 2)
