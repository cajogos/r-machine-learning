# Finding highly correlated features with the caret package

# NOTE: In regression or classification models perform better if highly correlated attributes are removed

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

library(caret)

# Remove any features that are not coded in numeric characters
new_train <- trainset[, ! names(churnTrain) %in% c("churn", "international_plan", "voice_mail_plan")]

# Obtain correlation of each attribute
cor_mat <- cor(new_train)
cor_mat

# Use findCorrelation to search for highly correlated attributes
highlyCorrelated <- findCorrelation(cor_mat, cutoff = 0.75) # Using cut off of 0.75

# Obtain the column names of highly correlated features
names(new_train)[highlyCorrelated]

# You can consider removing some highly correlated attributes and keep one or two for better accuracy