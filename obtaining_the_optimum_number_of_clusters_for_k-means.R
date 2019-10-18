# Obtaining the optimum number of clusters for k-means

customer = read.csv('data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

# Calculate the sum of squares (withinss) of different numbers of clusters
nk = 2:10
set.seed(22)


WSS = sapply(nk, function(k) {
  kmeans(customer, centers = k)$tot.withinss
})
WSS # For the within sum of squares, lower values represent clusters with better quality

# Plot the within sum of squares (with different values for k)
plot(nk, WSS, type = "l", xlab = "Number of K", ylab = "Within Sum of Squares")


# Calculate the average silhouette width
SW = sapply(nk, function(k) {
  cluster.stats(dist(customer), kmeans(customer, centers = k)$cluster)$avg.silwidth
})
SW

# Plot the average silhouette width (with different values for k)
plot(nk, SW, type = "l", xlab = "Number of Clusters", ylab = "Average Silhouette Width")

# Determine the location of the maximum average silhouette width
nk[which.max(SW)]
