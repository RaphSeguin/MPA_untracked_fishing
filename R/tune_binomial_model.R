tune_binomial_model <- function(mpa_model, formula_binomial){
  
  # Extracting the training and testing indices
  cv_folds <- vfold_cv(mpa_model, v = 5, strata = fishing_presence)
  
  # Model specification
  rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
    set_engine("ranger", probability = TRUE) %>%
    set_mode("classification")
  
  # Create a recipe for preprocessing
  mpa_recipe <- recipe(formula_binomial, 
                       data = mpa_model) 
  
  # Combine the recipe and model into a workflow
  wflow <- workflow() %>%
    add_recipe(mpa_recipe) %>%
    add_model(rf_spec)
  
  # Step 4: Define the Hyperparameter Grid
  hyperparameter_grid <- grid_regular(
    mtry(range = c(1, 20)),
    min_n(range = c(1, 10)),
    levels = 5
  )
  
  # Step 5: Tune the Model Using Spatial Cross-Validation
  tune_results <- tune_grid(
    wflow,
    resamples = cv_folds,
    grid = hyperparameter_grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE, parallel_over = "everything")
  )
  
  # View best hyperparameters
  best_params <- select_best(tune_results, metric = "roc_auc")
  print(best_params)
  
  #And now choose optimal classification threshold
  
  # Step 1: Train the Model with Best Hyperparameters
  best_mtry <- best_params$mtry
  best_min_n <- best_params$min_n
  
  # Prepare output as a dataframe
  results <- data.frame(
    mtry = best_mtry,
    min_n = best_min_n
  )
  
  return(results)
  
}