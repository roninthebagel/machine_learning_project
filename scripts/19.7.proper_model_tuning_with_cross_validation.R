## --- proper model training with cross-validation --- ##

# tuning hyperparameters into lasso model

# define parameter grid for Lasso regression
lasso_grid <- tibble(penalty = 10^seq(-3, 0, length.out = 10))

# create tunable Lasso model
lasso_tune <- linear_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")

# create workflow
lasso_workflow <- workflow() |> 
  add_model(lasso_tune) |> 
  add_recipe(age_recipe)

# set up cross-validation folds
set.seed(234)
folds <- vfold_cv(train_data, v = 5)

# tune model
lasso_results <- tune_grid(
  lasso_workflow,
  resamples = folds,
  grid = lasso_grid
)

# visualize tuning results
lasso_tuning_plots <- autoplot(lasso_results)

ggsave("figures/lasso_tuning_plots.pdf",
       plot = lasso_tuning_plots, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")

# adding penalties

# select best penalty value
best_penalty <- select_best(lasso_results, metric = "rmse")

# finalize workflow with best parameters
final_lasso <- finalize_workflow(lasso_workflow, best_penalty)

# fit final model on training data
final_fit <- fit(final_lasso, data = train_data)

# evaluate on test data
final_results <- augment(final_fit, new_data = test_data)
rmse(final_results, truth = age, estimate = .pred)

## --- which features matter most? --- ##

# examining which CpG sites are most important for age prediction

# extract coefficients from the Lasso model
lasso_coefs <- tidy(final_fit) |> 
  filter(term != "(Intercept)") |> 
  mutate(abs_estimate = abs(estimate)) |> 
  arrange(desc(abs_estimate))

# plot the most important CpG sites
top_n_sites <- 10

top_10_sites_plot <- ggplot(head(lasso_coefs, top_n_sites), 
       aes(x = reorder(term, abs_estimate), y = estimate, fill = estimate > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = paste("Top", top_n_sites, "CpG Sites for Age Prediction"),
       x = "CpG Site",
       y = "Coefficient Value",
       fill = "Increases with Age") +
  theme_minimal()

ggsave("figures/top_10_sites_plot.pdf",
       plot = top_10_sites_plot, 
       width = 25,
       height = 10, 
       units = "cm", 
       device = "pdf")
