## --- exploratory data analysis ---

# read the data set into R
ch4 <- read_xlsx(here("data", "DNA methylation data.xlsm"), sheet = 1)

# explore the first few rows
head(ch4)

#__________________________----

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

