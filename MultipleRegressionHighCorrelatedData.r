set.seed(1)
n <- 1000

Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)

dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


rmse_x1 <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  
  fit <- lm(y ~ x_1, data=train)
  y_hat <- fit$coef[1] + fit$coef[2]*test$x_1
  sqrt(mean((y_hat - test$y)^2))
})

mean(rmse_x1)
sd(rmse_x1)

rmse_x2 <- replicate(n, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  
  fit <- lm(y ~ x_2, data=train)
  y_hat <- fit$coef[1] + fit$coef[2]*test$x_2
  sqrt(mean((y_hat - test$y)^2))
})

mean(rmse_x2)
sd(rmse_x2)


rmse_x1_x2 <- replicate(100, {
  set.seed(1)
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  
  fit <- lm(y ~ x_1 + x_2, data=train)
  y_hat <- fit$coef[1] + fit$coef[2]*test$x_1 + fit$coef[3]*test$x_2
  sqrt(mean((y_hat - test$y)^2))
})

mean(rmse_x1_x2)
sd(rmse_x1_x2)