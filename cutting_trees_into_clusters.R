# Cutting trees into clusters

customer = read.csv('data/customer.csv', header = TRUE)

# Normalize the customer data into scale
customer = scale(customer[,-1])

# Agglomerative (bottom-up) hierarchical clustering
hc = hclust(dist(customer, method = "euclidean"), method = "ward.D2")
hc

# Categorize the data into four groups
fit = cutree(hc, k = 4)

# Examine the cluster labels for the data
fit

# Count the number of data within each cluster
table(fit)

# Visualize how data is clustered with a red rectangle border
plot(hc)
rect.hclust(hc, k = 4, border = "red")

# Place a rectangle around a certain cluster
dev.off()
plot(hc)
rect.hclust(hc, k = 4, which = 2, border = "red")

# Horizontal dendrogram with multiple colours
library(dendextend)

dend = customer %>% dist %>% hclust %>% as.dendrogram

dend %>% color_branches(k = 4) %>% plot(horiz = TRUE, main = "Horizontal Dendrogram")

# Red rectangle around the clusters
dend %>% rect.dendrogram(k = 4, horiz = TRUE)

# Add a line showing the tree cutting location
abline(v = heights_per_k.dendrogram(dend)["4"] + 0.1, lwd = 2, lty = 2, col = "blue")
