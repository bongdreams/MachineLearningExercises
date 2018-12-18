library(dslabs)
library(caret)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

ks <- c(1, 3, 5, 7, 9, 11)

F_1 <- function(k){
  set.seed(1)
  train_index <- createDataPartition(y, list = FALSE)
  
  train_set = x[train_index,]
  test_set = x[-train_index,]
  train_set_y = y[train_index]
  test_set_y = y[-train_index]
  
  knn_fit <- knn3(train_set,train_set_y, k = k)
  
  y_hat <- predict(knn_fit, test_set, type = "class")
  
  mean(y_hat == test_set_y) 
}

sapply(ks, F_1)