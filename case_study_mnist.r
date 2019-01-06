library(dslabs)
library(matrixStats)
library(ggplot2)
library(caret)
mnist <- read_mnist()

names(mnist)

dim(mnist$train$images)

class(mnist$train$labels)

table(mnist$train$labels)

set.seed(123)

index <- sample(nrow(mnist$train$images), 10000)

x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)

x_test <- mnist$test$images[index, ]
y_test <- factor(mnist$test$labels[index])

sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

nzv <- nearZeroVar(x)
nzv

#removing the near zero variance cols

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

#Implementing the KNN, optimizing using the K-Fold Cross Validations
#-----------------------------#
# To Quickly check how long will it take to run
n <- 1000                     #
b <- 2                        #
index <- sample(nrow(x), n)   #
#-----------------------------#

control <- trainControl(method="cv", number=10, p=0.9)
train <- train(x[, col_index], y, 
               method="knn", 
               tuneGrid = data.frame(k=c(1, 3, 5, 7)), 
               trControl = control)
plot(train)
fit_knn <- knn3(x[, col_index], y, k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type = "class")

cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

#Implementing the RandomForest using 5-Fold Cross Validation (Rborist)

library(Rborist)
control <- trainControl(method = "cv", number=5, p=0.8)
grid <- expand.grid(minNode=c(1, 5), predFixed=c(10, 15, 25, 35, 50))

train_rf <- train(x[, col_index], 
                  y,
                  method = "rf",
                  nTree = 50, 
                  trControl = control, 
                  tuneGrid = grid, 
                  nSamp = 5000)

ggplot(train_rf)

train_rf$bestTune

fit_rf <- Rborist(x[,col_index], y, 
                  nTree = 1000, 
                  minNode = train_rf$bestTune$minNode, 
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[, col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]
