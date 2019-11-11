rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

library(e1071)

library(car)
data(Quartet)

# Train the support vector machine on the Quartet dataset
model.regression <- svm(Quartet$y1 ~ Quartet$x, type = "eps-regression")

# Obtain prediction results
predict.y <- predict(model.regression, Quartet$x)
predict.y

plot(Quartet$x, Quartet$y1, pch = 19)
points(Quartet$x, predict.y, pch = 15, col = "red")
