library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

d<- dist(tissue_gene_expression$x)

d_mat <- as.matrix(d)

d_mat[1,2]
d_mat[1, 39]
d_mat[1,73]

d_mat[39, 40]
d_mat[39,73]

d_mat[73, 74]

image(as.matrix(d))