# Training a neural network with neuralnet

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

# Load the iris dataset
data(iris)

# Split the dataset into a training and testing set
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

# load the neuralnet library
library(neuralnet) # install.packages("neuralnet")

# Add the columns based on the name matched value in species column
trainset$setosa <- trainset$Species == "setosa"
trainset$virginica <- trainset$Species == "virginica"
trainset$versicolor <- trainset$Species == "versicolor"

set.seed(12345)

# Train the neural network with three hidden neurons in each layer
network <- neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     trainset, hidden = 3)
network

# View the summary by accessing the result.matrix
network$result.matrix

# View the generalized weight by accessing it in the network
head(network$generalized.weights[[1]])
