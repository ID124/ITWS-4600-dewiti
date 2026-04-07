library(caret)
library(GGally)
library(psych)
library(class)
library(cluster)
library(ggplot2)
library(factoextra)

# Rule of thumb: Take square root of observations and use that for number of neighbors

# ---- First KNN model ----- 

## read data
abalone <- read.csv("C:/Users/IDWT/OneDrive - Rensselaer Polytechnic Institute/Desktop/Spring 2026 Classes/Data Analytics/Labs/Lab 3/abalone/abalone.data", header=FALSE)

colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

abalone$weight_class <- cut(abalone$rings, br=c(0,8,11,35), labels = c("light", 'medium', 'heavy'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$weight_class <- as.character(abalone.sub$weight_class)

## convert back to factor
abalone.sub$weight_class <- as.factor(abalone.sub$weight_class)

## split train/test
set.seed(123)
train.indices <- createDataPartition(y = abalone$weight_class, p = 0.7, list = FALSE)


train <- abalone.sub[train.indices,]
test <- abalone.sub[-train.indices,]

knn.predicted <- knn(train[,1:7], test[,1:7],
                     train[,7], k=sqrt(nrow(abalone.sub)))

table(knn.predicted, test[,8],
      dnn=list('predicted','actual'))

## ----- Second KNN model -----
abalone$length_class <- cut(abalone$rings, br=c(0,8,11,35), labels = c("short", 'medium', 'long'))
abalone.sub1 <- abalone[,c(2:8,11)]

abalone.sub1$length_class <- as.character(abalone.sub1$length_class)

## convert back to factor
abalone.sub1$length_class <- as.factor(abalone.sub1$length_class)

## split train/test
set.seed(123)
train.indices1 <- createDataPartition(y = abalone.sub1$length_class, p = 0.7, list = FALSE)

train1 <- abalone.sub[train.indices1,]
test1 <- abalone.sub[-train.indices1,]

knn.predicted <- knn(train1[,1:7], test1[,1:7],
                     train1[,7], k=sqrt(nrow(abalone.sub)))

table(knn.predicted, test[,8],
      dnn=list('predicted','actual'))

## ---- Exercise 2 ----

# ---- K-means on first subset ----

# Same seed and k as your reference
set.seed(6)
k <- 3

# First feature subset of kNN model
X <- scale(abalone.sub[, 1:7])

# Learn clusters
abalone.km <- kmeans(X, centers = k, nstart = 25, iter.max = 100)

# WCSS 
wcss <- abalone.km$tot.withinss
wcss

# Cluster assignments 
assigned.clusters.km <- as.factor(abalone.km$cluster)

# attach to data frame 
abalone.sub$km_cluster <- assigned.clusters.km

# Scatterplot of the data colored by cluster membership
ggplot(abalone.sub, aes(x = whole_weight, y = shell_weight, colour = assigned.clusters.km)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "K-means clustering (k = 3)",
       x = "Whole weight",
       y = "Shell weight",
       colour = "Cluster")

## ---- K-means on second subset ---- 
set.seed(10)
k1 <- 3

# Second feature subset of kNN model
X1 <- scale(abalone.sub1[, 1:7])

# Learn clusters
abalone.km1 <- kmeans(X1, centers = k1, nstart = 25, iter.max = 100)

# Total (for all clusters) within-cluster sum of squares 
wcss <- abalone.km1$tot.withinss
wcss

# Put cluster assignments into a variable
assigned.clusters.km <- as.factor(abalone.km1$cluster)

# Optional: attach to your data frame (non-destructive—adds a new column)
abalone.sub1$km_cluster <- assigned.clusters.km

# Scatterplot of the data colored by cluster membership
ggplot(abalone.sub1, aes(x = length, y = height, colour = assigned.clusters.km)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "K-means clustering (k = 3)",
       x = "Length",
       y = "Height",
       colour = "Cluster")

## ---- PAM on first subset ----

PAM_k1 = 3

iris.pam1 <- pam(abalone.sub[-8], PAM_k1)

iris.pam1$objective

# Clustering output 
assigned.clusters <- as.factor(iris.pam1$cluster)

ggplot(abalone.sub, aes(x = whole_weight, y = shell_weight, colour = assigned.clusters)) +
  geom_point()

# Silhouette plot
sil1 <- silhouette(iris.pam$cluster, dist(abalone.sub[-8]))
fviz_silhouette(sil1)

## ---- PAM on second subset ---- 
PAM_k2 = 3

iris.pam2 <- pam(abalone.sub1[-8], PAM_k2)

iris.pam2$objective

# Clustering output 
assigned.clusters2 <- as.factor(iris.pam$cluster)

ggplot(abalone.sub1, aes(x = length, y = height, colour = assigned.clusters2)) +
  geom_point()

# Silhouette plot
sil2 <- silhouette(iris.pam$cluster, dist(abalone.sub1[-8]))
fviz_silhouette(sil2)
