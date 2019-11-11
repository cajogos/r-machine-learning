# Predicting labels based on a model trained by nnet

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

# Prepared dataset
data(iris)
set.seed(2)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

# Train the neural network using nnet
library(nnet)
iris.nn <- nnet(Species ~ ., data = trainset, size = 2, rang = 0.1,
                decay = 5e-4, maxit = 200)


# Generate the predictions of the testing dataset on the model
iris.predict <- predict(iris.nn, testset, type = "class")

# Generate a classification table based on the predicted labels and labels of testing
nn.table <- table(testset$Species, iris.predict)
nn.table

# Generate a confusion matrix based on the classification table
library(caret)
confusionMatrix(nn.table)
