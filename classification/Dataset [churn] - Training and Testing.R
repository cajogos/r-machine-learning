library(C50) # install.packages("C50")
data(churn)

# Remove columns not appropriate for classification features
churnTrain <- churnTrain[, ! names(churnTrain) %in% c("state", "area_code", "account_length")]

set.seed(2)

# Split the data : 70% for training, 30% for testing
ind <- sample(2, nrow(churnTrain), replace = TRUE, prob = c(0.7, 0.3))

churn.Train <- churnTrain[ind == 1,]
churn.Test <- churnTrain[ind == 2, ]

dim(churn.Train)
dim(churn.Test)
