library(dplyr)
library(devtools)
library(genefilter)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5)

tt <- colttests(x, as.factor(y))

tt %>% filter(p.value <= 0.01)

ind <- which(tt$p.value <= 0.01)

x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results