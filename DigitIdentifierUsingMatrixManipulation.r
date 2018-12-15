library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()

mat <- mnist$train$images 

mat[mat < 50 | mat > 205] <- 0
mat[mat >= 50 & mat <= 205] <- 1

rm <- rowMeans(mat)

mean(rm)