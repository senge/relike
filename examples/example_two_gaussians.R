# rm(list=ls())

# Setup
library(tidyverse)

source("R/relike.uq.R")
source("R/relike.model.funs.R")

# Parameters
bag_size = 200
train_num_instances = 500
test_grid_stepsize = 0.1
target_column = "y"
train.fun = relike.train.fun.nb
predict.fun = relike.predict.fun.nb


# Synthetic train dataset
set.seed(23)
df_train <- 
      tibble(x1 = rnorm(train_num_instances) + 1, 
             x2 = rnorm(train_num_instances) + 1, 
             y = "neg") %>%
  union_all(
      tibble(x1 = rnorm(train_num_instances) + 3, 
             x2 = rnorm(train_num_instances) + 3, 
             y = "pos"))

df_train_x = df_train %>% select(-any_of(c(target_column)))
df_train_y = df_train %>% pull(target_column)

#ggplot(df_train, aes(x=x1, y=x2, color=y)) + geom_point()

# Synthetic test dataset
x1_range = round(range(df_train$x1), digits = 2) * 1.2
x2_range = round(range(df_train$x2), digits = 2) * 1.2
df_test_x = expand.grid(
  x1 = seq(x1_range[1], x1_range[2], by = test_grid_stepsize), 
  x2 = seq(x2_range[1], x2_range[2], by = test_grid_stepsize))

# Do the experiment
ruq = relike.uq(df_train_x, df_train_y, df_test_x, train.fun = train.fun, predict.fun = predict.fun)

# Plot the result
plot_data = cbind(df_test_x, ruq)
  
g1 = ggplot(plot_data, aes(x = x1, y = x2, fill = ua)) + geom_tile() +
  ggtitle("Aleatoric Uncertainty") + 
  scale_fill_gradient(low = "white", high="red", limits = c(0, 1)) + 
  labs(color = "class") +
  geom_point(data = df_train, aes(x = x1, y = x2, fill = NULL, color = y)) + 
  theme_bw()

g2 = ggplot(plot_data, aes(x = x1, y = x2, fill = ue)) + geom_tile() +
  ggtitle("Epistemic Uncertainty") + 
  scale_fill_gradient(low = "white", high="red", limits = c(0, 1)) + 
  geom_point(data = df_train, aes(x = x1, y = x2, fill = NULL, color = y)) + 
  labs(color = "class") +
  theme_bw()

g3 = ggplot(plot_data, aes(x = x1, y = x2, fill = pos)) + geom_tile() +
  ggtitle("Positive Probability") + 
  scale_fill_gradient(low = "white", high="green", limits = c(0, 1)) + 
  geom_point(data = df_train, aes(x = x1, y = x2, fill = NULL, color = y)) + 
  labs(color = "class") +
  theme_bw()

g4 = ggplot(plot_data, aes(x = x1, y = x2, fill = neg)) + geom_tile() +
  ggtitle("Negative Probability") + 
  scale_fill_gradient(low = "white", high="green", limits = c(0, 1)) + 
  geom_point(data = df_train, aes(x = x1, y = x2, fill = NULL, color = y)) + 
  labs(color = "class") +
  theme_bw()

grid.arrange(g1, g2, g3, g4, nrow = 2, top = paste0("Two Gaussians: size of bag: ", bag_size))




