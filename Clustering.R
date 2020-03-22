library(dplyr)
library(randomForest)
library(gbm)
library(caret)
library(ROCR)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)


# Read in the datasets

users <- read.csv("data_full.csv")

# Factorize data for clustering
users_factor <- apply(users, 2, function(x) as.numeric(as.factor(x)))

# K-means clustering
set.seed(1680) # for reproducibility
km <- kmeans(users_factor, centers = 10, iter.max=50) # centers randomly selected from rows of airline.scaled

km.centroids <- km$centers
km.clusters <- km$cluster
table(km.clusters)
km$tot.withinss  # cluster dissimilarity


# Cross validation
wss <- function(k) {
  kmeans(users_factor, k, nstart = 10)$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# Pick the best k = 7
km_cv <- kmeans(users_factor, centers = 7, iter.max=50)
km_cv$centers


# Visualize clusters
fviz_cluster(km_cv, users_factor, labelsize = 0.02)

table(km_cv$cluster, users$country_destination)
table(km_cv$cluster, users$age_generation)

km_cv$cluster <- as.factor(km_cv$cluster)
ggplot(users, aes(gender, age, color = km_cv$cluster)) + geom_point()



