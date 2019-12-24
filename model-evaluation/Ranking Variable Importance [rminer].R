# Ranking the variable importance with the rminer package

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

library(rminer) # install.packages("rminer")

# Fit the svm model with the training set
model <- fit(churn ~ ., trainset, model = "svm")

# Importance function to obtain the variable importance
variableImportance <- Importance(model, trainset, method = "sensv")

# Plot variable importance - ranked by the variance
L <- list(runs = 1, sen = t(variableImportance$imp), sresponses = variableImportance$sresponses)
mgraph(L, graph = "IMP", leg = names(trainset), col = "gray", Grid = 10)
