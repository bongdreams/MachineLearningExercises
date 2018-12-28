library(caret)
library(dslabs)
library(ggplot2)
library(dplyr)
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_qda<-train(x, y, method="qda",data=x)

train_qda

train_qda$finalModel$means

qplot(train_qda$finalModel$means[1,], train_qda$finalModel$means[2,])