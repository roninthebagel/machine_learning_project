## --- defining the model with tidymodel ---

# define the linear regression model
linear_model <- linear_reg() |> 
  set_engine("lm")  # We are using the "lm" engine for linear regression

# preprocess the data with a repcipe 
# centering the predictors (methylation values for each CpG site) in the recipe
age_recipe <- recipe(age ~ ., data = ch4) |> 
  step_center(all_predictors())  # Centering the predictors

## --- create a workflow ---

# creating a workflow
workflow_model <- workflow() |> 
  add_model(linear_model) |> 
  add_recipe(age_recipe)

## --- fit the model ---

# fitting the model
fit_model <- fit(workflow_model, data = ch4)

## --- evaluate the model ---

# evaluating the model's performance
fit_model_summary <- tidy(fit_model)
fit_model_summary

## --- visualising our predictions ---

# Get predictions on the training data
predictions_lm <- augment(fit_model, new_data = ch4)

# plot observed vs. predicted values
predictions_plot <- ggplot(data = ch4, aes(x = age, y = predictions_lm$.pred)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Observed vs Predicted Bat Age")

# saving plot
ggsave("figures/observed_vs_predicted_bat_age.pdf",
       plot = predictions_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

## --- performance metrics ---

# creating numerical metrics
mse <- mean((ch4$age - predictions_lm$.pred)^2)

# A cleaner function for calculating MSE
mse_impl <- function(model, data, predictor) {
  augment(model, new_data = data) |> 
    mutate(squared_error = (.pred - {{predictor}})^2) |> 
    summarise(mse = mean(squared_error)) |> 
    pull(mse)
}

mse <-  mse_impl(fit_model, ch4, age)

rmse <- sqrt(mse)
rmse

# Get more comprehensive model statistics
glance(fit_model)

## --- check model assumptions ---

# creating visual fit models
fit_model |> 
  extract_fit_engine() |> 
  check_model()
