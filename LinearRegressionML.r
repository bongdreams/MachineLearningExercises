library(caret)

#Use the caret package to partition the dataset into test and training sets of equal size. 
#Train a linear model and calculate the RMSE. 
#Repeat this exercise 100 times and report the mean and standard deviation of the RMSEs.

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1)
rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data=train)
  y_hat <- fit$coef[1] + fit$coef[2]*test$x
  sqrt(mean((y_hat - test$y)^2))
})

mean(rmse)
sd(rmse)

# Highly correlated

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1)
rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data=train)
  y_hat <- fit$coef[1] + fit$coef[2]*test$x
  sqrt(mean((y_hat - test$y)^2))
})

mean(rmse)
sd(rmse)



