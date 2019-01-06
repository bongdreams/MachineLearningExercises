library(dslabs)
library(dplyr)
library(caret)

data("movielens")

head(movielens)

movielens %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

set.seed(755)

test_index <- createDataPartition(y = movielens$rating, times = 1, p=0.2, list=F)

train_set <- movielens[-test_index, ]

test_set <- train_set %>% semi_join(train_set, by = "movieId") %>% semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)

# fit <- lm(rating ~ as.factor(movieId), data=movielens)
# plot(fit)



