library(dslabs)
library(purrr)
library(caret)

data('heights')

y <-heights$sex
x <- heights$height

index <- createDataPartition(y, times=1, p=0.5, list=F)

train_set <- heights[index,]
test_set <- heights[-index,]

ks <- seq(1, 101, 3)

F_Accuracy <- map_df(ks, function(k){
  set.seed(1)
  
  knn_fit <- knn3(train_set$sex~train_set$height, data=train_set, k = k)
  y_hat <- predict(knn_fit, test_set, type="class")
  F_value <- F_meas(data=y_hat, reference=factor(test_set$sex))
  print(paste(k, F_value))
})

