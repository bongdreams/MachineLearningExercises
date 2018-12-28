library(Rborist)

fit <- train(y ~ ., method="Rborist", tuneGrid=data.frame(predFixed = 1, minNode=seq(25, 100, 25)), data=dat)

fit