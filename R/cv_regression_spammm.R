cv_regression_spammm <- function(mpa_model_regression, folds_regression, 
                          best_params_regression, formula_regression){
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create 10-fold cross-validation folds
  folds <- vfold_cv(mpa_model_regression_no_val, v = 10)
  
  #Cross validation loop
  cross_validation_loop_regression <- mclapply(1:10,function(k){
    
    # Extract the training and testing data for the k-th fold
    fold <- folds$splits[[k]]
    train <- analysis(fold)mod
    test <- assessment(fold)
    
    #Run model
    #Whole model AIC = 163
    
    #Simple AIC = 73
    system.time({
    mod <- fitme(mpa_fishing_GFW_log ~ fishing_2022_log + fishing +
                   seamount_distance + mean_sst + mean_chl +
                   depth + ais_reception_positions_per_day_class_A +
                    gdp + dist_to_port + Matern(1 | X + Y),
                                      data = train, family=gaussian(),
                                      control.HLfit=list(NbThreads = 4))
    })
    
    # Predict on the test set
    test_preds_log <- predict(mod, newdata = test, re.form = NA)
    
    # Predict on the validation set
    val_preds_log <- predict(mod, newdata = validation_set, re.form =  NA)
    
    # Backtransform predictions using smearing coefficient
    test$preds_log <- test_preds_log
    test$preds <- exp(test_preds_log)
    
    validation_set$preds_log <- val_preds_log
    validation_set$preds <- exp(val_preds_log)
    
    # Calculate performance metrics on backtransformed predictions for test set
    rmse <- caret::RMSE(test$preds, test$mpa_fishing_GFW)
    mae <- caret::MAE(test$preds, test$mpa_fishing_GFW)
    medae <- median(abs(test$mpa_fishing_GFW - test$preds))
    r2 <- caret::R2(test$preds, test$mpa_fishing_GFW)
    
    # Calculate performance metrics on validation set
    rmse_val <- caret::RMSE(validation_set$preds, validation_set$mpa_fishing_GFW)
    mae_val <- caret::MAE(validation_set$preds, validation_set$mpa_fishing_GFW)
    r2_val <- caret::R2(validation_set$preds, validation_set$mpa_fishing_GFW)
    
    # Return a list of performance metrics
    performance_output <- data.frame(
      Fold = k,
      RMSE = rmse,
      MAE = mae,
      MedAE = medae,
      R2 = r2,
      RMSE_VAL = rmse_val,
      MAE_VAL = mae_val,
      R2_VAL = r2_val
    )
    
  }, mc.cores = 2)
  
  return(cross_validation_loop_regression)
  
  
}