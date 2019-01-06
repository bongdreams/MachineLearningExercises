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

mu <- mean(train_set$rating)
movie_avgs <- train_set %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

hist(movie_avgs$b_i)

predicted_ratings <- mu + test_set %>% left_join(movie_avgs, by = 'movieId') %>% .$b_1

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, data.frame(method='Movie effect model', RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

# Bringing users into the mix
# fit <- lm(rating ~ as.factor(userId) + as.factor(movieId), data=movielens)
# plot(fit)

user_avgs <- test_set %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by = 'movieId') %>% 
  left_join(user_avgs, by ='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results, data.frame(method = 'Movie + User Effects Model', RMSE = model_2_rmse))

rmse_results %>% knitr::kable()



