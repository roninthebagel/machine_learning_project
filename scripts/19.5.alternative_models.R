## --- alternative models ---

## --- lasso regression (r1 regularisation) ---
# define a Lasso regression model with a specific penalty
lasso_model <- linear_reg(penalty = 0.1, mixture = 1) |> 
  set_engine("glmnet")

# create a workflow with the Lasso model
workflow_model_lasso <- workflow() |> 
  add_model(lasso_model) |> 
  add_recipe(age_recipe)

## lasso model error

# fit the Lasso model
fit_lasso <- fit(workflow_model_lasso, data = ch4)

# check model performance
mse_impl(fit_lasso, ch4, Age)
