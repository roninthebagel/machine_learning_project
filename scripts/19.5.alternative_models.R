## --- alternative models --- ##

## --- lasso regression (l1 regularisation) --- ##

# define a Lasso regression model with a specific penalty
lasso_model <- linear_reg(penalty = 0.1, mixture = 1) |> 
  set_engine("glmnet")

translate(lasso_model, engine = "glmnet")

print(lasso_model)

# create a workflow with the Lasso model
workflow_model_lasso <- workflow() |> 
  add_model(lasso_model) |> 
  add_recipe(age_recipe)

# fit the Lasso model
fit_lasso <- fit(workflow_model_lasso, data = ch4)

# check model performance
mse_impl(fit_lasso, ch4, age)


## --- ridge regression (l2 regularisation) --- ##

# define a Ridge regression model
ridge_model <- linear_reg(penalty = 0.1, mixture = 0) |> 
  set_engine("glmnet")

# create a workflow with the Ridge model
workflow_model_ridge <- workflow() |> 
  add_model(ridge_model) |> 
  add_recipe(age_recipe)

# fit the Ridge model
fit_ridge <- fit(workflow_model_ridge, data = ch4)

# check model performance
mse_impl(fit_ridge, ch4, age) 
