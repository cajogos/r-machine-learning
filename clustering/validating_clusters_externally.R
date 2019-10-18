# Validating clusters externally

library(png) # install.packages("png")

# Read image and transform it into a scatter plot
img2 = readPNG("clustering/data/handwriting.png", TRUE)
img3 = img2[, nrow(img2):1]

b = cbind(as.integer(which(img3 < -1) %% 28), which(img3 < -1) / 28)

plot(b, xlim = c(1, 28), ylim = c(1, 28))

# Perform a k-means clustering method on the handwriting digits
set.seed(18)

fit = kmeans(b, 2)

plot(b, col = fit$cluster)
plot(b, col = fit$cluster, xlim = c(1, 28), ylim = c(1, 28))

# Perform a dbscan clustering method on the handwriting digits
ds = dbscan(b, 2)
ds

plot(ds, b, xlim = c(1, 28), ylim = c(1, 28))
