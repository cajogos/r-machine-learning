# Measuring the prediction performance of a conditional inference tree

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

library(party) # install.packages("party")
ctree.model <- ctree(churn ~ ., data = trainset)

ctree.predict <- predict(ctree.model, testset)
table(ctree.predict, testset$churn)

library(caret)
confusionMatrix(table(ctree.predict, testset$churn))

# treeresponse will tell you the list of class probabilities
tr <- treeresponse(ctree.model, newdata = testset[1:5,])
tr

# NOTE: If you specify the type as prob when using the predict function
# you will get exactly the same result as what treeresponse returns
