# Training a neural network with nnet

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

# Load the nnet package
library(nnet) # install.packages("nnet")

data(iris)
set.seed(2)

# Split the dataset into a training and testing dataset
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
trainset <- iris[ind == 1,]
testset <- iris[ind == 2,]

# Train the neural network using nnet
iris.nn <- nnet(Species ~ ., data = trainset, size = 2, rang = 0.1,
                decay = 5e-4, maxit = 200)

summary(iris.nn)
