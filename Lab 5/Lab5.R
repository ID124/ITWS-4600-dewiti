library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(performance)

wine <- read_csv("wine.data", col_names = FALSE)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
wine$Type <- as.factor(wine$Type)

N <- nrow(wine)
train.indexes <- sample(N,0.7*N)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

# Linear SVM
svm.mod0 <- svm(Type~., data = train, kernel = 'linear')
svm.mod0
test.pred <- predict(svm.mod0, test)
cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))
cm

# Radial SVM
svm.mod1 <- svm(Type~., data = train, kernel = 'radial')
radial_test.pred <- predict(svm.mod1, test)
radial_cm = as.matrix(table(Actual = test$Type, Predicted = radial_test.pred))
radial_cm

# Finding best gamma values
gamma.range <- seq(.1,10,.1)
gamma.range

tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial', gamma = gamma.range)
tuned.svm

# kNN
knn.predicted <- knn(train[,2:13], test[,2:13], train$Type, k=3)
knn_matrix <- as.matrix(table(Actual = test$Type, Predicted = knn.predicted))

# Comparing SVM and kNN
# Linear SVM
n = sum(cm) 
nc = nrow(cm) 
diagv = diag(cm) 
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 

accuracy <- sum(diagv)/n
accuracy

recall = diagv / rowsums 
recall

precision = diagv / colsums
precision

f1 = 2 * precision * recall / (precision + recall)
f1

#kNN
n1 = sum(knn_matrix) 
knn_nc = nrow(knn_matrix) 
knn_diagv = diag(knn_matrix) 
knn_rowsums = apply(knn_matrix, 1, sum) 
knn_colsums = apply(knn_matrix, 2, sum) 
knn_p = knn_rowsums / n1 
knn_q = knn_colsums / n1

knn_accuracy <- sum(knn_diagv)/n
knn_accuracy

knn_recall = knn_diagv / knn_rowsums 
knn_recall

knn_precision = knn_diagv / knn_colsums
knn_precision

knn_f1 = 2 * precision * recall / (precision + recall)
knn_f1