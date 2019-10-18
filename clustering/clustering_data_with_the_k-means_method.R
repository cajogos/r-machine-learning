# Clustering data with the k-means method

customer = read.csv('data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

set.seed(22)

# Use k-means to cluster the customer data
fit = kmeans(customer, 4)
fit

# Inspect the center of each cluster using a barplot
barplot(t(fit$centers), beside = TRUE, xlab = "cluster", ylab = "value")

# Scatter plot of the data (with colours)
plot(customer, col = fit$cluster)
