B <- 10000
y <- rnorm(100, 0, 1)
set.seed(1)
Q_stars <- replicate(B, {
  y_star <- rnorm(100, 0, 1)
  print(y_star)
  Q_star <- quantile(y_star, 0.75)
  
})
mean(Q_stars)
sd(Q_stars)