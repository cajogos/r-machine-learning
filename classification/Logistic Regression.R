# Classifying data with logistic regression

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

fit <- glm(churn ~ ., data = trainset, family = binomial)
summary(fit)

# The built model contains insignificant variables - leading to misclassification
# Use only significant variables to train the classification model

fit <- glm(churn ~ international_plan + voice_mail_plan + total_intl_calls + number_customer_service_calls,
          data = trainset, family = binomial)
summary(fit)

# Use a fitted model to predict the outcome of testset
pred <- predict(fit, testset, type = "response")

# Determine the class - by judging whether the probability is above 0.5
Class <- pred > 0.5
summary(Class)

# Generate the counting statistics
tb <- table(testset$churn, Class)
tb

# Turn the statistics into a classification table
churn.mod <- ifelse(testset$churn == "yes", 1, 0)
pred.class <- churn.mod
pred.class[pred <= 0.5] <- 1 - pred.class[pred <= 0.5]
ctb <- table(churn.mod, pred.class)
ctb

# Generat the confusion matrix
library(caret)
confusionMatrix(ctb)
