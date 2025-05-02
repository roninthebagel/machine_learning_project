## --- training and testing sets ---

# split the data into 80% training and 20% testing
set.seed(123)  # For reproducibility
split <- initial_split(ch4, prop = 0.8)

# extract training and testing sets
train_data <- training(split)
test_data <- testing(split)

# check the dimensions of the splits
glimpse(train_data)
glimpse(test_data)

# fit the model on the training data
lm_fit <- fit(workflow_model, data = train_data)

# view the model summary
tidy(lm_fit)

# calculate performance metrics for linear regression
cat("RMSE is:", sqrt(mse_impl(lm_fit, test_data, age)))

####

# Make predictions on the test set using the linear model
lm_predictions <- predict(lm_fit, new_data = test_data)

# Combine predictions with actual values (truth) into a tibble
results <- tibble(
  truth = test_data$age,  # Actual values (truth)
  estimate = lm_predictions$.pred  # Predicted values (estimate)
)

# Now use yardstick's rsq function to calculate R-squared
rsq_result <- rsq(results, truth = truth, 
                  estimate = estimate)

# Print the R-squared result
rsq_result

## --- cross-validation for robust evaluation (k-folds) ---

# perform 10-fold cross-validation
#v/k same thing

folds <- vfold_cv(train_data, v = 10)

# fit models using cross-validation
cv_results <- fit_resamples(workflow_model, 
                            resamples = folds)

# collect and visualize metrics
cv_metrics <- collect_metrics(cv_results)

cv_metric_plot <- ggplot(cv_metrics, aes(x = .metric, y = mean, color = .metric)) +
  geom_boxplot() +
  labs(title = "Cross-Validation Performance")

# saving plot
ggsave("figures/cv_metric_plot.pdf",
       plot = cv_metric_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- exploring different fold numbers --- 

fold_numbers <- seq(2, 10, 1)

cv_results_by_fold <- map_dfr(fold_numbers, function(k_value) {
  # Create cross-validation with k folds
  k_folds <- vfold_cv(train_data, v = k_value)
  
  # fit models using cross-validation
  cv_results <- fit_resamples(workflow_model, resamples = k_folds)
  
  # collect metrics and add k value
  cv_metrics <- collect_metrics(cv_results)
  cv_metrics$.k <- k_value
  
  return(cv_metrics)
})

# plot performance by number of folds
cv_folds_plot <- ggplot(cv_results_by_fold, aes(x = .k, y = mean, color = .metric)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ .metric, scales = "free_y") +
  labs(title = "Performance by Number of Cross-Validation Folds",
       x = "Number of Folds (k)",
       y = "Mean Performance")

# saving plot
ggsave("figures/cv_folds_plot.pdf",
       plot = cv_folds_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- final model evaluation ---

# use 5-fold cross-validation for final assessment
final_k <- vfold_cv(train_data, v = 5)

# fit with prediction saving
final_cv_results <- fit_resamples(
  workflow_model, 
  resamples = final_k,
  control = control_resamples(save_pred = TRUE)
)

# examine performance metrics
collect_metrics(final_cv_results)

# get all predictions across folds for visualization
cv_predictions <- collect_predictions(final_cv_results)

# visualize predictions vs actual values across all folds
cv_predictions_plot <- ggplot(cv_predictions, aes(x = age, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Cross-Validation Predictions vs Actual Age",
       x = "Actual Age (years)",
       y = "Predicted Age (years)")

# saving plot
ggsave("figures/cv_predictions_plot.pdf",
       plot = cv_predictions_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")
