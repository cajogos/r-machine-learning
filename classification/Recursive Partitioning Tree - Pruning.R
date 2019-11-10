# Pruning a recursive partitioning tree

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

library(rpart)

# Build a classification tree model
churn.rp <- rpart(churn ~ ., data = trainset)

# 1. Find the minimum cross-validation error of the classification tree model
min(churn.rp$cptable[, "xerror"])

# 2. Locate the record with the minimum cross-validation errors
which.min(churn.rp$cptable[, "xerror"])

# 3. Get the cost complexity parameter of the record with the minimum cross-validation errors
churn.cp <- churn.rp$cptable[8, "CP"]
churn.cp

# 4. Prune the tree by setting the cp parameter to the CP value of the record with minimum cross-validation errors
prune.tree <- prune(churn.rp, cp = churn.cp)

# 5. Visualise the classification tree
plot(prune.tree, margin = 0.1)
text(prune.tree, all = TRUE, use.n = TRUE)

# 6. Generate a classification table based on pruned classification tree model
predictions <- predict(prune.tree, testset, type = "class")
table(testset$churn, predictions)

# 7. Generate a confusion matrix
library(caret)
confusionMatrix(table(predictions, testset$churn))
