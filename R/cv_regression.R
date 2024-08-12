cv_regression <- function(mpa_model_regression, folds_regression, 
                          best_params_regression, formula_regression, validation_set){
  
  #Cross validation loop
  cross_validation_loop_regression <- mclapply(1:10,function(k){
    
    # extracting the training and testing indices
    trainSet <- which(folds_regression != k) # training set indices
    testSet <- which(folds_regression == k) # testing set indices
    
    train <- mpa_model_regression_no_val[trainSet, ]
    test <- mpa_model_regression_no_val[testSet, ]
    
    #Run model
    mod_rf = ranger(formula_regression,
                 data = train,
                 num.threads = 2, 
                 num.trees= 1000,
                 mtry = best_params_regression$mtry,            # Use best mtry
                 min.node.size = best_params_regression$min_n,  # Use best min_n
                 importance = "permutation"
    )
    
    # Run linear model
    mod_lm <- lm(mpa_fishing_GFW_log ~ fishing + fishing_2022_log, data = train)

    # Predict on test set with random forest
    test_preds_rf_log <- predict(mod_rf, test)$predictions
    smearing_coefficient_rf <- mean(exp(train$mpa_fishing_GFW_log - predict(mod_rf, train)$predictions))
    test$preds_rf <- exp(test_preds_rf_log) * smearing_coefficient_rf
    
    # Predict on test set with linear model
    test_preds_lm_log <- predict(mod_lm, test)
    smearing_coefficient_lm <- mean(exp(train$mpa_fishing_GFW_log - predict(mod_lm, train)))
    test$preds_lm <- exp(test_preds_lm_log) 
    
    # Calculate performance metrics for random forest model
    rmse_rf <- caret::RMSE(test$preds_rf, test$mpa_fishing_GFW)
    mae_rf <- caret::MAE(test$preds_rf, test$mpa_fishing_GFW)
    r2_rf <- caret::R2(test$preds_rf, test$mpa_fishing_GFW)
    
    # Calculate performance metrics for linear model
    rmse_lm <- caret::RMSE(test$preds_lm, test$mpa_fishing_GFW)
    mae_lm <- caret::MAE(test$preds_lm, test$mpa_fishing_GFW)
    r2_lm <- caret::R2(test$preds_lm, test$mpa_fishing_GFW)
    
    # Test both models on validation set (extrapolation)
    val_preds_rf_log <- predict(mod_rf, validation_set)$predictions
    validation_set$preds_rf <- exp(val_preds_rf_log) * smearing_coefficient_rf
    
    val_preds_lm_log <- predict(mod_lm, validation_set)
    validation_set$preds_lm <- exp(val_preds_lm_log)
    
    # Performance metrics on validation set
    rmse_rf_val <- caret::RMSE(validation_set$preds_rf, validation_set$mpa_fishing_GFW)
    mae_rf_val <- caret::MAE(validation_set$preds_rf, validation_set$mpa_fishing_GFW)
    r2_rf_val <- caret::R2(validation_set$preds_rf, validation_set$mpa_fishing_GFW)
    
    rmse_lm_val <- caret::RMSE(validation_set$preds_lm, validation_set$mpa_fishing_GFW)
    mae_lm_val <- caret::MAE(validation_set$preds_lm, validation_set$mpa_fishing_GFW)
    r2_lm_val <- caret::R2(validation_set$preds_lm, validation_set$mpa_fishing_GFW)
    
    # Collect performance metrics
    performance_output <- data.frame(
      Fold = k,
      RMSE_RF = rmse_rf,
      MAE_RF = mae_rf,
      R2_RF = r2_rf,
      RMSE_LM = rmse_lm,
      MAE_LM = mae_lm,
      R2_LM = r2_lm,
      RMSE_RF_VAL = rmse_rf_val,
      MAE_RF_VAL = mae_rf_val,
      R2_RF_VAL = r2_rf_val,
      RMSE_LM_VAL = rmse_lm_val,
      MAE_LM_VAL = mae_lm_val,
      R2_LM_VAL = r2_lm_val
    )

    # # Extract variable importance and calculate as a percentage
    # var_imp <- mod$variable.importance
    # var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)
    # 
    # return(list(performance = performance_output, importance = var_imp_df))
    
  }, mc.cores = 2)
  
  return(cross_validation_loop_regression)
  
  
}