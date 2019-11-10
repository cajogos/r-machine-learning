# Clustering data with the model-based method

# Model-based clustering techniques assume varieties of data models and apply an EM algorithm to obtain the most likely model, and further use the model to infer the most likely number of clusters

customer = read.csv('clustering/data/customer.csv', header = TRUE)
customer = scale(customer[,-1])

library(mclust) # install.packages("mclust")

# Perfom model-based clustering on the customer dataset
mb = Mclust(customer)

plot(mb)

# Summary function will return the most likely model and number of clusters
summary(mb)
