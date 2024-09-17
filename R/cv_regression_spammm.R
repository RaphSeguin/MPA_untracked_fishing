cv_regression_spammm <- function(folds_regression, formula_regression, validation_set, pred_var){
  
  # Cross validation loop for spaMM
  cross_validation_loop_regression <- mclapply(1:nrow(folds_regression), function(k){
    
    # Extract the training and testing data for the k-th fold
    fold <- folds_regression$splits[[k]]
    train <- analysis(fold)
    test <- assessment(fold)
    
    # Run model
    mod <- lmer(formula_regression,
                 data = train)
               # control.HLfit = list(NbThreads = 4))
    
    # Predict on the test set and validation set
    test_preds_log <- predict(mod, newdata = test, re.form = NA)
    val_preds_log <- predict(mod, newdata = validation_set, re.form = NA)
    
    # Backtransform predictions using the exponential function
    test$preds <- exp(test_preds_log)
    validation_set$preds <- exp(val_preds_log)
    
    write.csv(test, file = "test.csv")

    
    # Calculate performance metrics on the test set
    rmse <- caret::RMSE(test$preds, test[[pred_var]])
    mae <- caret::MAE(test$preds, test[[pred_var]])
    medae <- median(abs(test[[pred_var]] - test$preds))
    r2 <- caret::R2(test$preds, test[[pred_var]])
    
    # Calculate performance metrics on the validation set
    rmse_val <- caret::RMSE(validation_set$preds, validation_set[[pred_var]])
    mae_val <- caret::MAE(validation_set$preds, validation_set[[pred_var]])
    r2_val <- caret::R2(validation_set$preds, validation_set[[pred_var]])
    
    # Return performance metrics as a data frame
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
    
    return(performance_output)
  }, mc.cores = 2)
  
  # Combine results from all folds into a single data frame
  results_cv_regression <- do.call(rbind, cross_validation_loop_regression) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  
  # Compute and print mean R2 and R2_VAL
  mean_r2 <- mean(results_cv_regression$R2)
  mean_r2_val <- mean(results_cv_regression$R2_VAL)
  
  cat("Mean R2 across folds: ", mean_r2, "\n")
  cat("Mean R2_VAL across folds: ", mean_r2_val, "\n")
  
  return(results_cv_regression)
}
