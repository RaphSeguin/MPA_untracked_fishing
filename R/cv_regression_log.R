cv_regression_log <- function(mpa_model_regression, folds_regression, 
                          best_params_regression, formula_regression){
  
  #Cross validation loop
  cross_validation_loop_regression <- lapply(1:10,function(k){
    
    # extracting the training and testing indices
    # this way only works with foldID
    trainSet <- which(folds_regression != k) # training set indices
    testSet <- which(folds_regression == k) # testing set indices
    
    train <- mpa_model_regression[trainSet, ]
    test <- mpa_model_regression[testSet, ] 
    
    #Run model
    mod = ranger(formula_regression,
                 data = train,
                 num.threads = 2, 
                 num.trees= 1000,
                 mtry = best_params_regression$mtry,            # Use best mtry
                 min.node.size = best_params_regression$min_n,  # Use best min_n
                 importance = "permutation"
    )
    
    # Predict on test set
    test_preds_log <- predict(mod, test)$predictions
    # test$preds <- exp(test_preds_log)
    
    test$preds <- test_preds_log
    
    # Calculate performance metrics on backtransformed predictions
    rmse <- caret::RMSE(test$preds, test$mpa_fishing_GFW_log)
    mae <- caret::MAE(test$preds, test$mpa_fishing_GFW_log)
    medae <- median(abs(test$mpa_fishing_GFW_log - test$preds))
    r2 <- caret::R2(test$preds, test$mpa_fishing_GFW_log)
    
    # Calculate rsq_trad
    ss_residual <- sum((test$mpa_fishing_GFW_log - test$preds)^2)
    ss_total <- sum((test$mpa_fishing_GFW_log - mean(test$mpa_fishing_GFW_log))^2)
    rsq_trad <- 1 - (ss_residual / ss_total)
    
    # Calculate nrmse (normalized RMSE)
    sd_outcome <- sd(exp(test$mpa_fishing_GFW_log))
    nrmse <- rmse / sd_outcome
    
    # Calculate CVRMSE (Coefficient of Variation of RMSE)
    mean_outcome <- mean(test$mpa_fishing_GFW_log)
    cvrmse <- rmse / mean_outcome
    
    # Return a list of performance metrics
    performance_output <- data.frame(
      Fold = k,
      RMSE = rmse,
      MAE = mae,
      MedAE = medae,
      R2 = r2,
      RSQ_TRAD = rsq_trad,
      NRMSE = nrmse,
      CVRMSE = cvrmse
    )
    
    #Analyze residuals
    
    # Residuals for each observation
    # residuals <- test$mpa_fishing_GFW - test$preds
    # 
    # # Plot Residuals vs. Predicted Values
    # ggplot(data = test, aes(x = test$preds, y = residuals)) +
    #   geom_point() +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #   labs(title = "Residuals vs. Predicted Values",
    #        x = "Predicted Fishing Effort",
    #        y = "Residuals") +
    #   theme_minimal()
    # 
    # #Plot Residuals vs. Observed Values
    # ggplot(data = test, aes(x = test$mpa_fishing_GFW, y = residuals)) +
    #   geom_point() +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #   labs(title = "Residuals vs. Observed Values",
    #        x = "Observed Fishing Effort",
    #        y = "Residuals") +
    #   theme_minimal()
    # 
    # #Histogram of Residuals
    # ggplot(data = test, aes(x = residuals)) +
    #   geom_histogram(bins = 30, fill = "blue", color = "black") +
    #   labs(title = "Histogram of Residuals",
    #        x = "Residuals",
    #        y = "Frequency") +
    #   theme_minimal()
    # 
    # #Check for Heteroscedasticity
    # ggplot(data = test, aes(x = test$preds, y = abs(residuals))) +
    #   geom_point() +
    #   geom_smooth(method = "loess", color = "red") +
    #   labs(title = "Residuals Absolute Value vs. Predicted Values",
    #        x = "Predicted Fishing Effort",
    #        y = "Absolute Residuals") +
    #   theme_minimal()
    
    #Residuals vs. Feature Plots
    # ggplot(data = test, aes(x = fishing_2022, y = residuals)) +
    #   geom_point() +
    #   geom_smooth(method = "loess", color = "red") +
    #   labs(title = "Residuals vs. Feature",
    #        x = "Some Feature",
    #        y = "Residuals") +
    #   theme_minimal()
    
    # qqnorm(residuals)
    # qqline(residuals, col = "red")
    # 
    
    # Extract variable importance and calculate as a percentage
    var_imp <- mod$variable.importance
    var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)
    
    return(list(performance = performance_output, importance = var_imp_df))
    
  })
  
  return(cross_validation_loop_regression)
  
  
}