# Drawing a bivariate cluster plot

customer = read.csv('clustering/data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

set.seed(22)

# Use k-means to cluster the customer data
fit = kmeans(customer, 4)

library(cluster) # install.packages("cluster")

# Draw a bivariate cluster plot
clusplot(customer, fit$cluster, color = TRUE, shade = TRUE)

# Zoom into the bivariate cluster plot
dev.off()

par(mfrow = c(1, 2)) # Set-up two plots (1 row and 2 columns)

clusplot(customer, fit$cluster, color = TRUE, shade = TRUE)
rect(-2.2, 1.2, 0.8, 1.7, border = "orange", lwd = 2) # x-left, y-bottom, x-right, y-top

clusplot(customer, fit$cluster, color = TRUE, xlim = c(-2.2, 0.8), ylim = c(1.2, 1.7)) # Value for xlim, ylim will match above

# ======================================== #

# The clusplot function uses princomp and cmdscale to reduce the original feature dimension to the principal component.
# Therefore, one can see how data is clustered in a single plot with these two components as the x-axis and y-axis.

dev.off()
mds = cmdscale(dist(customer), k = 2)
plot(mds, col = fit$cluster)
