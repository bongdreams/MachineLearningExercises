library(caret)
library(dslabs)
library(ggplot2)
library(dplyr)

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda<-train(x, y, method="lda",data=x)

train_lda