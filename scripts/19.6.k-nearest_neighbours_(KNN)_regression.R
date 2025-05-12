## --- K-nearest neighbours (KNN) regression --- ##

# define a KNN model with K=1
knn_model <- nearest_neighbor(neighbors = 1) |> 
  set_engine("kknn") |> 
  set_mode("regression")

# create a workflow with the KNN model
workflow_model_knn <- workflow() |> 
  add_model(knn_model) |> 
  add_recipe(age_recipe)

# fit the KNN model
fit_knn <- fit(workflow_model_knn, data = ch4)

# check model performance
mse_impl(fit_knn, ch4, age)

# exploring K values

# try different K values
k_values <- c(1, 5, 10, 25, 50, 100)

k_results <- map_dfr(k_values, function(k) {
  # define KNN model with current K value
  knn_model <- nearest_neighbor(neighbors = k) |>  
    set_engine("kknn") |>  
    set_mode("regression")
  
  # create workflow
  workflow_model_knn <- workflow() |>  
    add_model(knn_model) |>  
    add_recipe(age_recipe)
  
  # fit model
  fit_knn <- fit(workflow_model_knn, data = ch4)
  
  # calculate MSE
  mse_value <- mse_impl(fit_knn, ch4, age)
  
  # return results as a data frame row
  tibble(k = k, mse = mse_value, rmse = sqrt(mse_value))
})

# plot results
k_value_plot <- ggplot(k_results, aes(x = k, y = rmse)) +
  geom_line() +
  geom_point() +
  labs(title = "KNN Performance by Number of Neighbors (K)",
       x = "Number of Neighbors (K)",
       y = "Root Mean Squared Error (years)") +
  theme_minimal()

ggsave("figures/k_value_plot.pdf",
       plot = k_value_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- train and test comparison of all models --- ##

# make sure we've created train/test split first
set.seed(123)
split <- initial_split(ch4, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# train models on training data
fit_lm <- fit(workflow_model, data = train_data)
fit_ridge <- fit(workflow_model_ridge, data = train_data)
fit_lasso <- fit(workflow_model_lasso, data = train_data)
fit_knn <- fit(workflow_model_knn, data = train_data)

# generate predictions on test data
predictions <- list(
  "Linear" = augment(fit_lm, new_data = test_data)$.pred,
  "Ridge" = augment(fit_ridge, new_data = test_data)$.pred,
  "Lasso" = augment(fit_lasso, new_data = test_data)$.pred,
  "KNN" = augment(fit_knn, new_data = test_data)$.pred
)

# create visualization of predictions vs actual values
plots_list <- map2(predictions, names(predictions), function(preds, model_name) {
  ggplot(data = test_data, aes(x = age, y = preds)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = model_name,
         x = "Actual Age (years)",
         y = "Predicted Age (years)") +
    theme_minimal()+
    scale_y_continuous(limits = c(0,9))
})
print(plots_list)

# display plots in a grid
library(patchwork)
all_plots_list <- wrap_plots(plots_list)

ggsave("figures/all_plots_list.pdf",
       plot = all_plots_list, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- comparing models with r-squared --- ##

# calculate R-squared for each model
model_metrics <- map2_dfr(predictions, names(predictions), 
                          function(preds, model_name) {
                            results <- tibble(
                              truth = test_data$age,  # actual values
                              estimate = preds        # predicted values
                            )
                            
                            # calculate R-squared
                            rsq_val <- yardstick::rsq(results, truth = truth, estimate = estimate)
                            
                            # add model name and return
                            rsq_val |>  mutate(model = model_name)
                          })

# create a bar plot comparing R-squared values
rsquared_plots <- ggplot(model_metrics, aes(x = model, y = .estimate, fill = model)) +
  geom_col() +
  labs(title = "Model Comparison: R-squared on Test Data",
       x = "Model",
       y = "R-squared") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/rsquared_plot.pdf",
       plot = rsquared_plots, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- comparing models using rmse --- ##

# calculate RMSE for each model
rmse_metrics <- map2_dfr(predictions, names(predictions), 
                         function(preds, model_name) {
                           results <- tibble(
                             truth = test_data$age,
                             estimate = preds
                           )
                           
                           # calculate RMSE
                           rmse_val <- yardstick::rmse(results, truth = truth, estimate = estimate)
                           
                           # add model name and return
                           rmse_val |>  mutate(model = model_name)
                         })

# create a bar plot comparing RMSE values
rmse_plot <- ggplot(rmse_metrics, aes(x = model, y = .estimate, fill = model)) +
  geom_col() +
  labs(title = "Model Comparison: RMSE on Test Data",
       x = "Model",
       y = "Root Mean Squared Error (years)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/rmse_plot.pdf",
       plot = rmse_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

