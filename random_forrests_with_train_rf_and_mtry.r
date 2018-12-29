library(caret)
library(devtools)

data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

#dat <- data.frame(y=tissue_gene_expression['y'],tissue_gene_expression['x']) 
set.seed(1991) 
fit_rf <- train(x, y, method="rf", tuneGrid=data.frame(mtry=seq(50,200,25)), nodesize=1) 
fit_rf$bestTune

varImp(fit_rf)
