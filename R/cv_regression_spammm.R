#' Cross-Validation for Regression Model (spaMM)
#'
#' This function performs **k-fold cross-validation** for a regression model using **linear mixed-effects modeling** (`lmer()`) 
#' and evaluates its performance on both test and validation datasets.
#'
#' @param folds_regression A resampling object (`rsample` object) containing cross-validation folds.
#' @param formula_regression A formula specifying the regression model.
#' @param validation_set A dataframe containing the validation dataset for additional performance evaluation.
#' @param pred_var A string specifying the name of the response variable in the dataset.
#'
#' @return A dataframe (`results_cv_regression`) containing cross-validation performance metrics:
#' - `Fold`: Fold number.
#' - `RMSE`: Root Mean Squared Error (test set).
#' - `MAE`: Mean Absolute Error (test set).
#' - `MedAE`: Median Absolute Error (test set).
#' - `R2`: R-squared score (test set).
#' - `RMSE_VAL`: Root Mean Squared Error (validation set).
#' - `MAE_VAL`: Mean Absolute Error (validation set).
#' - `R2_VAL`: R-squared score (validation set).
#'
#' @details
#' 1. **Splits data into training and testing sets** for each cross-validation fold.
#' 2. **Trains a linear mixed-effects regression model** (`lmer()`) on the training data.
#' 3. **Predicts outcomes** on both the test set and validation set.
#' 4. **Back-transforms log predictions** using the exponential function.
#' 5. **Computes performance metrics**:
#'    - RMSE, MAE, Median AE, and R² for both test and validation sets.
#' 6. **Aggregates results across all folds** and calculates mean R² and R² for validation.
#' 7. **Prints overall model performance** (`mean R²` and `mean R²_VAL`).
#'
#' @examples
#' cv_results <- cv_regression_spammm(folds_regression, log(vessel_size) ~ ., validation_set, "vessel_size")
#'

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
    test_preds_log <- predict(mod, newdata = test, re.form = ~0 )
    val_preds_log <- predict(mod, newdata = validation_set, re.form = ~0 )
    
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
