# Visualizing a dissimilarity matrix

# A dissimilarity matrix can be used as a measurement for the quality of a cluster.
# To visualize the matrix, we can use a heat map on a distance matrix.

# Within the plot, entries with low dissimilarity (or high similarity) are plotted darker, which is helpful to identify hidden structures in the data.

customer = read.csv('clustering/data/customer.csv', header = TRUE)
customer = scale(customer[,-1])
set.seed(22)
km = kmeans(customer, 4)

library(seriation) # install.packages("seriation")

# Use dissplot to visualise the dissimilarity matrix in a heat map
dissplot(dist(customer), labels = km$cluster, options = list(main = "k-means Clustering with k = 4"))

# Use dissplot on hierarchical clustering in a heat map
complete_c = hclust(dist(customer), method = "complete")
hc_complete = cutree(complete_c, k = 4)

dissplot(dist(customer), labels = hc_complete, options = list(main = "Hierarchical Clustering"))

# Visualize a distance matrix
image(as.matrix(dist(customer)))

# Plotting both a dendrogram and heat map showing how data is clustered
cd = dist(customer)
hc = hclust(cd)
cdt = dist(t(customer))
hcc = hclust(cdt)
heatmap(customer, Rowv = as.dendrogram(hc), Colv = as.dendrogram(hcc))
