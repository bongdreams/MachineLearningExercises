library(caret)
library(rpart)
library(rpart.plot)
data("tissue_gene_expression")


x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
set.seed(1991)
#fit <- train(x, y, method="rpart", control = rpart.control(minsplit = 0))

fittest <- rpart(y ~ x, data = tissue_gene_expression, control = rpart.control(minsplit = 0))

y_hat <- predict(fittest, type = "class")

confusionMatrix(data = as.factor(y_hat), reference = as.factor(tissue_gene_expression$y))

rpart.plot(fittest)