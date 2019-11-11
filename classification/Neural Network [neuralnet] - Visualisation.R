# Visualizing a neural network trained by neuralnet

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


# Visualise the trained network
plot(network)

# Visualise generalised weights
par(mfrow = c(2, 2))
gwplot(network, selected.covariate = "Petal.Width")
gwplot(network, selected.covariate = "Petal.Length")
gwplot(network, selected.covariate = "Sepal.Width")
gwplot(network, selected.covariate = "Sepal.Length")

# If all the generalized weights are close to zero on the plot, it means the covariate has little effect.
# However, if the overall variance is greater than one, it means the covariate has a nonlinear effect.
