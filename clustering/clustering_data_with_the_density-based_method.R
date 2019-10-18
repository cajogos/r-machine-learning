# Clustering data with the density-based method

# As an alternative to distance measurement, you can use a density-based measurement to cluster data

library(mlbench) # install.packages("mlbench")
library(fpc) # install.packages("fpc")

set.seed(22)

# Use the mlbench library to draw a Cassini problem graph
p = mlbench.cassini(500)
plot(p$x)

# Cluster the data in regards to its density measurement
ds = dbscan(dist(p$x), 0.2, 2, countmode = NULL, method = "dist")
ds

# Plot the data in a scatter plot
plot(ds, p$x)

# Use dbscan to predict which cluster the data belongs to
y = matrix(0, nrow = 3, ncol = 2)
y[1,] = c(0, 0)
y[2,] = c(0, -1.5)
y[3,] = c(1, 1)
y

predict(ds, p$x, y)
