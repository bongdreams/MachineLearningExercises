
y <- rnorm(100, 0, 1)

set.seed(1)
indexes <- createResample(y, 10000)

output <- matrix(ncol=1, nrow=10000)

count <- 1
for(i in indexes){
  output[count,] <- quantile(i, 0.75)
  count <- count +1 
  
}

q_star <- data.frame(output)
mean(q_star$output)

sd(q_star$output)

