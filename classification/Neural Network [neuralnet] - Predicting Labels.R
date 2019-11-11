# Predicting labels based on a model trained by neuralnet

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

# Split the dataset into a training and testing set
data(iris)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]
trainset$setosa <- trainset$Species == "setosa"
trainset$virginica <- trainset$Species == "virginica"
trainset$versicolor <- trainset$Species == "versicolor"

set.seed(12345)
# Train the neural network with three hidden neurons in each layer
library(neuralnet)
network <- neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     trainset, hidden = 3)

# Generate a probability matrix based on trained neural network and testing dataset
net.predict <- compute(network, testset[-5])$net.result

# Obtain other possible values by finding the column with the greatest probability
net.prediction <- c("versicolor", "virginica", "setosa")[apply(net.predict, 1, which.max)]

# Generate a classification table based on the predicted labels and the labels of the testing dataset
predict.table <- table(testset$Species, net.prediction)
predict.table

# Generate a class agreement
classAgreement(predict.table)

# Use a confusion matrix to measure prediction performance
confusionMatrix(predict.table)
