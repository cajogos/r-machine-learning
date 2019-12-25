# Classifying data with random forest

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

library(randomForest) # install.packages("randomForest")

# Fit the Random Forest classifier
churn.rf <- randomForest(churn ~ ., data = trainset, importance = TRUE)
churn.rf

# Make predictions based on the fitted model
churn.prediction <- predict(churn.rf, testset)

# Obtain the classification table
table(churn.prediction, testset$churn)

# Plot the mean square error (MSE)
plot(churn.rf)

# Examine importance of each attribute
importance(churn.rf)

# Plot the importance using varImpPlot
varImpPlot(churn.rf)

# Calculate the margins and plot the margin cumulative distribution
margins.rf <- margin(churn.rf, trainset)
plot(margins.rf)

# Use an histogram to visualise the margin distribution of the Random Forest
hist(margins.rf, main = "Margins of Random Forest for churn dataset")

# Using a box plot
boxplot(margins.rf ~ trainset$churn, main = "Margins of Random Forest for churn dataset by class")

# use the cforest function within the party package to perform classifications
library(party)

# Fit the classification model
churn.cforest <- cforest(churn ~ ., data = trainset, controls = cforest_unbiased(ntree = 1000, mtry = 5))
churn.cforest

# Make predictions based on the built model
churn.cforest.prediction <- predict(churn.cforest, testset, OOB = TRUE, type = "response")

# Obtain the classification table
table(churn.cforest.prediction, testset$churn)
