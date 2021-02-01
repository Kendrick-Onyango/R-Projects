#Load data set
data("USArrests")
head(USArrests)

str(USArrests)

any(is.na(USArrests))

install.packages("corrplot")

library(corrplot)

corrplot(cor(USArrests), method = "number", type = "lower")

USArrests <- scale(USArrests)

dim(USArrests)

head(USArrests, n = 5)

#cluster for computing clustering algorithms, and factoextra for ggplot2-based elegant visualization of clustering results
install.packages("cluster")
library(cluster)
library(factoextra)
library(ggplot2)

set.seed(123)

crime <- sample(1:50, 10)

crime_1 <- USArrests[crime,]
crime_1

dist_eucl <- dist(crime_1, method = "euclidean")

head(dist_eucl)

round(as.matrix(dist_eucl)[1:4, 1:4], 1)

#heatmap
fviz_dist(dist_eucl)


wss <- sapply(1:crime, 
              function(k){kmeans(USArrests, k, nstart=20,iter.max = 15)$tot.withinss})

plot(1:crime, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(USArrests, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")

getwd()
