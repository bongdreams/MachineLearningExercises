library(dplyr)
library(devtools)
library(genefilter)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5)

x_subset <- x[ ,sample(p, 100)]

tt <- colttests(x, as.factor(y))

tt %>% filter(p.value < 0.01)
