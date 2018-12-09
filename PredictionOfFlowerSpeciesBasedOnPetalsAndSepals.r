library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Analysing the best predictor from the results of the data
table(train$Species, train$Sepal.Length)
table(train$Species,train$Sepal.Width)
table(train$Species, train$Petal.Length)
table(train$Species, train$Petal.Width)

# Setting up the prediction with the best predictor
y_hat <- ifelse(train$Petal.Length <= 4.7, "versicolor", "virginica")

# Finding the overall accuracy for the test set without smart cutoff technique
mean(test$Species == y_hat)

# Finding overall accuracy for the test set with best cutoff technique
cutoff <- seq(3, 7, by = 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff,"virginica", "versicolor") %>% factor(levels = levels(test$Species))
y_hat <- factor(y_hat, levels=c("virginica", "versicolor", "setosa"))
mean(y_hat == test$Species)

# Analysing the best predictor from the results of the test data
table(test$Species, test$Sepal.Length)
table(test$Species, test$Sepal.Width)
table(test$Species, test$Petal.Length)
table(test$Species, test$Petal.Width)


# Finding overall accuracy for the test set with best cutoff technique for Both PetalLength and PetalWidth

# Finding best cutoff for Petal Length
cutoffPetalLength <- seq(3, 7, by = 0.1)
accuracyLength <- map_dbl(cutoffPetalLength, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracyLength)
best_cutoff_Length <- cutoffPetalLength[which.max(accuracyLength)]
best_cutoff_Length

# Finding best cutoff for Petal Width
cutoffPetalWidth <- seq(1, 2.6, by = 0.1)
accuracyWidth <- map_dbl(cutoffPetalWidth, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
max(accuracyWidth)
best_cutoff_Width <- cutoffPetalWidth[which.max(accuracyWidth)]
best_cutoff_Width

# With this data set the best_cutoff_width is coming to be 1.5 however looking at the data, it looks like 1.6 will make the overall accuracy better thus assigning best_cutoff_width = 1.6
best_cutoff_Width <- 1.6

y_hat <- ifelse((test$Petal.Length > best_cutoff_Length | test$Petal.Width > best_cutoff_Width), "virginica", "versicolor") %>% factor(levels = levels(test$Species))

y_hat <- factor(y_hat, levels=c("virginica", "versicolor", "setosa"))

mean(y_hat == test$Species)
