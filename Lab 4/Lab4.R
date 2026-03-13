library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
wine$Type <- as.factor(wine$Type)


X <- wine[,2:13]
Y <- wine[,14]

# Computing PCs
Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = F)

principal_components <- princomp(Xc)

summary(principal_components)

principal_components$loadings # weights of X components

plot(principal_components)

# Plotting PC1 vs PC2
ggplot(principal_components$scores, aes(x = Comp.1, y = Comp.2)) + geom_point(color="blue")

# Variables that contribute to PC1
principal_components$scores
principal_components$loadings[, 1]
    # Magnesium has the highest contribution

# Training dataset using kNN
wine.exclusive <- wine[,1:5]
w.train <-  sample(178,100) 

wine.train <- wine.exclusive[w.train,]
wine.test <- wine.exclusive[-w.train,]

knn.predicted <- knn(wine.train[,2:5], wine.test[,2:5], wine.train$Type, k=3) # Make sure 3rd var is a vector

# Train dataset using data projected on first 2 PCs
pc_features <- principal_components$scores[, 1:2]

pc_train <- pc_features[w.train, ]
pc_test  <- pc_features[-w.train, ]

train_labels <- wine$Type[w.train]

pca_predicted <- knn(pc_train, pc_test, train_labels, k = 3)

# Comparing two models using contingency tables and metrics
table(knn.predicted, pca_predicted, dnn=list('predicted','actual'))
