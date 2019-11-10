# Visualizing a conditional inference tree

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

plot(ctree.model)

# Reduce the built model with less input features
daycharge.model <- ctree(churn ~ total_day_charge, data = trainset)
plot(daycharge.model)
