library(dslabs)
data("heights")

heights %>%
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%

qplot(height, p, data =.)

###############################################################################

ps <- seq(0, 1, 0.1)
heights %>% 
  
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  
group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


###############################################################################

