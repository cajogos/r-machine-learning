# Measuring prediction performance using ROCR

# NOTE: A Receiver Operating Characteristic (ROC) curve is a plot that illustrates
# the performance of a binary classifier system and plot the true positive rate
# against the false positive rate for different cut points.

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

# Install and load the ROCR package
library(ROCR) # install.packages("ROCR")

# Train the SVM model (probability set to TRUE)
svmfit <- svm(churn ~ ., data = trainset, prob = TRUE)

# Make predictions (probability set to TRUE)
pred <- predict(svmfit, testset[, !names(testset) %in% c("churn")], probability = TRUE)

# Obtain the proability of labels with yes
pred.prob <- attr(pred, "probabilities")
pred.to.roc <- pred.prob[, 2]

# Generate a prediction result
pred.rocr <- prediction(pred.to.roc, testset$churn)

# Obtain the performance measurement - Including AUC (Area Under Curve)
perf.rocr <- performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr <- performance(pred.rocr, "tpr", "fpr")

# Visualize the ROC curve
plot(perf.tpr.rocr, colorize = TRUE, main = paste("AUC:", perf.rocr@y.values))

# NOTE: High AUC value means it performs well under the given model (i.e. SVM)