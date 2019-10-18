# Clustering data with hierarchical clustering

customer = read.csv('clustering/data/customer.csv', header = TRUE)
head(customer)

# Examine the dataset structure
str(customer)

# Normalize the customer data into scale
customer = scale(customer[,-1])

# Agglomerative (bottom-up) hierarchical clustering
hc = hclust(dist(customer, method = "euclidean"), method = "ward.D2")
hc

# Plot the dendrogram
plot(hc, hang = -0.01, cex = 0.7)

# Using single method to perform hierarchical clustering
hc2 = hclust(dist(customer), method = "single")

plot(hc2, hang = -0.01, cex = 0.7)

# Divisive (top-down) hierarchical clustering: use diana function
library(cluster) # install.packages("cluster")

dv = diana(customer, metric = "euclidean")

summary(dv)

plot(dv) # NOTE: Will give two plots (banner and dendrogram)


# Drawing an horizontal dendrogram
library(dendextend) # install.packages("dendextend")

dend = customer %>% dist %>% hclust %>% as.dendrogram

dend %>% plot(horiz = TRUE, main = "Horizontal Dendrogram")
