tune_regression_model <- function(mpa_model_regression, formula_regression){
  
  # Extracting the training and testing indices
  cv_folds <- vfold_cv(mpa_model_regression, v = 5)
  
  # Model specification for regression
  rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  
  # Create a recipe for preprocessing
  mpa_recipe <- recipe(formula_regression, 
                       data = mpa_model_regression)
  
  # Combine the recipe and model into a workflow
  wflow <- workflow() %>%
    add_recipe(mpa_recipe) %>%
    add_model(rf_spec)
  
  # Define the Hyperparameter Grid
  hyperparameter_grid <- grid_regular(
    mtry(range = c(1, 18)),
    min_n(range = c(1, 10)),
    levels = 5
  )
  
  # Tune the Model Using Cross-Validation
  tune_results <- tune_grid(
    wflow,
    resamples = cv_folds,
    grid = hyperparameter_grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE, parallel_over = "everything")
  )
  
  # Select the best hyperparameters based on rsq_trad
  best_params <- select_best(tune_results, metric = "rsq")
  print(best_params)
  
  # Return the best hyperparameters
  return(best_params)
  
}