# Visualizing a recursive partitioning tree

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
churn.rp = rpart(churn ~ ., data = trainset)

plot(churn.rp, margin = 0.1)
text(churn.rp, all = TRUE, use.n = TRUE)

# Adjusting the parameters
plot(churn.rp, uniform = TRUE, branch = 0.6, margin = 0.1)
text(churn.rp, all = TRUE, use.n = TRUE)

