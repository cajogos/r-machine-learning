# Measuring the performance of the regression model

rm(list = ls(all = TRUE)) # Clean-up environment
dev.off() # Clean-up any plots

library(car)
data(Quartet)

# Plot attribute y3 against attribute x (using lm [Linear Model])
plot(Quartet$x, Quartet$y3)
lmfit <- lm(Quartet$y3 ~ Quartet$x)
abline(lmfit, col = "red")

# Retrieve predicted values
predicted <- predict(lmfit, newdata = Quartet[c("x")])

# Calculate the Root Mean Square Error (RMSE)
actual <- Quartet$y3
rmse <- (mean((predicted - actual)^2))^0.5
rmse

# Calculate the Relative Square Error (RSE)
mu <- mean(actual)
rse <- mean((predicted - actual)^2) / mean((mu - actual)^2)
rse

# R-Squared measurement
rsquare <- 1 - rse
rsquare

# Plot attribute y3 against attribute x (using rlm [Regression Model] from MASS)
library(MASS)
plot(Quartet$x, Quartet$y3)
rlmfit <- rlm(Quartet$y3 ~ Quartet$x)
abline(rlmfit, col = "purple")

# Get the predicted results
predicted <- predict(rlmfit, newdata = Quartet[c("x")])

# Calculate RMSE
actual <- Quartet$y3
rmse <- (mean((predicted - actual)^2))^0.5
rmse

# Calculate RSE
mu <- mean(actual)
rse <- mean((predicted - actual)^2) / mean((mu - actual)^2)
rse

# R-Square
rsquare <- 1 - rse
rsquare
