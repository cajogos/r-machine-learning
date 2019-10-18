# Comparing clustering methods

customer = read.csv('data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

# Using the fpc package

library(fpc) # install.packages("fpc")

# Hierarchical clustering with the single method
single_c = hclust(dist(customer), method = "single")
hc_single = cutree(single_c, k = 4)

# Hierarchical clustering with the complete method
complete_c = hclust(dist(customer), method = "complete")
hc_complete = cutree(complete_c, k = 4)

# K-means clustering
set.seed(22)
km = kmeans(customer, 4)

# Retrieve the cluster validation statistics
cs = cluster.stats(dist(customer), km$cluster)
cs

# Most often we focus on within.cluster.ss and avg.silwidth to validate a clustering method
cs[c("within.cluster.ss", "avg.silwidth")]

# Generate cluster statistics of each clustering method
sapply(list(kmeans = km$cluster, hc_single = hc_single, hc_complete = hc_complete), function(c) cluster.stats(dist(customer), c)[c("within.cluster.ss", "avg.silwidth")])

# K-Means also outputs statistics to validate a clustering method
km$withinss
km$betweenss
