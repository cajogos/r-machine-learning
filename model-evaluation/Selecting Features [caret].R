# Selecting features using the caret package

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

# Transform features and replace columns (training set)
intl_plan <- model.matrix(~ trainset.international_plan - 1, data = data.frame(trainset$international_plan))
colnames(intl_plan) <- c("trainset.international_planno" = "intl_no",
                         "trainset.international_planyes" = "intl_yes")

voice_plan <- model.matrix(~ trainset.voice_mail_plan - 1, data = data.frame(trainset$voice_mail_plan))
colnames(voice_plan) <- c("trainset.voice_mail_planno" = "voice_no",
                          "trainset.voice_mail_planyes" = "voice_yes")

trainset$international_plan <- NULL
trainset$voice_mail_plan <- NULL
trainset <- cbind(intl_plan, voice_plan, trainset)


# Transform features and replace columns (testing set)
intl_plan <- model.matrix(~ testset.international_plan - 1, data = data.frame(testset$international_plan))
colnames(intl_plan) <- c("testset.international_planno" = "intl_no",
                         "testset.international_planyes" = "intl_yes")

voice_plan <- model.matrix(~ testset.voice_mail_plan - 1, data = data.frame(testset$voice_mail_plan))
colnames(voice_plan) <- c("testset.voice_mail_planno" = "voice_no",
                          "testset.voice_mail_planyes" = "voice_yes")

testset$international_plan <- NULL
testset$voice_mail_plan <- NULL
testset <- cbind(intl_plan, voice_plan, testset)

# Create feature selection algorithm (Linear Discriminant Analysis)
ldaControl <- rfeControl(functions = ldaFuncs, method = "cv")

# Perform backward feature selection on the training dataset
ldaProfile <- rfe(trainset[, ! names(trainset) %in% c("churn")],
                  trainset[, c("churn")],
                  sizes = c(1:18),
                  rfeControl = ldaControl)
ldaProfile

# Plot the selection result
plot(ldaProfile, type = c("o", "g"))

# Best subset of variables
ldaProfile$optVariables

# Examine the fitted model
ldaProfile$fit

# Calculate performance accross resamples
postResample(predict(ldaProfile, testset[, !names(testset) %in% c("churn")]), testset[, c("churn")])

