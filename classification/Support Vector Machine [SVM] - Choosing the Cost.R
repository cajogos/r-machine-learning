# Choosing the cost of a support vector machine

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

iris.subset <- subset(iris, select = c("Sepal.Length", "Sepal.Width", "Species"), Species %in% c("setosa", "virginica"))

plot(x = iris.subset$Sepal.Length, y = iris.subset$Sepal.Width,
     col = iris.subset$Species, pch = 19)

# Train the SVM model using the iris subset
svm.model <- svm(Species ~ ., data = iris.subset, kernel = "linear", cost = 1, scale = FALSE)

# Circle the support vectors
points(iris.subset[svm.model$index, c(1, 2)], col = "blue", cex = 2)

# Add separation line on the plot
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a = -b / w[1, 2], b = -w[1, 1] / w[1, 2], col = "red", lty = 5)

# Create SVM classifier with a cost of 10,
plot(x = iris.subset$Sepal.Length, y = iris.subset$Sepal.Width,
     col = iris.subset$Species, pch = 19)

svm.model <- svm(Species ~ ., data = iris.subset, type = "C-classification",
                 kernel = "linear", cost = 10000, scale = FALSE)

points(iris.subset[svm.model$index, c(1, 2)], col = "blue", cex = 2)

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho

abline(a = -b / w[1, 2], b = -w[1, 1] / w[1, 2], col = "red", lty = 5)
