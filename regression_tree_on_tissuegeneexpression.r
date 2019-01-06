library(caret)
library(rpart)
library(rpart.plot)
data("tissue_gene_expression")


x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
set.seed(1991)

fit <- train(x, y, method="rpart", control = rpart.control(minsplit = 0))
fit

confusionMatrix(fit)