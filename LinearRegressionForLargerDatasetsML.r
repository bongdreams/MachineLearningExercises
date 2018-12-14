library(caret)

#Repeat the previous exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). 
#Save the average and standard deviation of RMSE from the 100 repetitions using a seed of 1.

set.seed(1)
rmse_calc <- function(size){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(size, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit <- lm(y ~ x, data=train)
    y_hat <- fit$coef[1] + fit$coef[2]*test$x
    sqrt(mean((y_hat - test$y)^2))
  })
  
  list(mean(rmse), sd(rmse))
}

size <- c(100, 500, 1000, 5000, 10000)
sapply(size, rmse_calc)



