set.seed(1)
disease <- sample(c("healthy","disease"), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease=="healthy"] <- sample(c("negative","positive"), size=sum(disease=="healthy"), replace=TRUE, prob=c(0.90,0.10))
test[disease=="disease"] <- sample(c("negative","positive"), size=sum(disease=="disease"), replace=TRUE, prob=c(0.15,0.85))

healthy_positive <- table(test[disease=="healthy"])["positive"]
disease_positive <- table(test[disease=="disease"])["positive"]

healthy_negative <- table(test[disease=="healthy"])["negative"]
disease_negative <- table(test[disease=="disease"])["negative"]

total <- healthy_positive + healthy_negative + disease_positive + disease_negative

# Probability that a test is positive

p_positive <- ( healthy_positive + disease_positive ) / total
p_positive

# Probability that has Disease if the test is negative

p_disease_negative <- disease_negative / total
p_disease_negative

# Probability that has Disease if the test is positive

p_disease_positive <- disease_positive / (healthy_positive + disease_positive)
p_disease_positive


