# Extracting silhouette information from clustering

# Silhouette information is a measurement to validate a cluster of data.
# The silhouette coefficient combines the measurement of the intracluster and intercluster distance.
# The output value typically ranges from 0 to 1; the closer to 1, the better the cluster is.

customer = read.csv('data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

# Generate the k-means
set.seed(22)
km = kmeans(customer, 4)

# Compute the silhouette information
kms = silhouette(km$cluster, dist(customer))
summary(kms)

# Plot the silhouette information
plot(kms)
