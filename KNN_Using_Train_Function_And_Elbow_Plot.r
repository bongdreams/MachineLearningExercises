library(dslabs)
library(caret)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1, 500, 10)))
ggplot(fit)