cv_binomial <- function(mpa_model, spatial_folds, best_params_binomial, optimal_cutoff, formula_binomial){
  
  #Test final model performance
  cross_validation_binomial <- lapply(1:10,function(k){
    
    # extracting the training and testing indices
    trainSet <- which(spatial_folds != k) # training set indices
    testSet <- which(spatial_folds == k) # testing set indices
    
    train <- mpa_model[trainSet, ] 
    test <- mpa_model[testSet, ] 
    
    # Train the model using the best hyperparameters
    mod <- ranger(
      formula_binomial,
      data = train,
      mtry = best_params_binomial$mtry,            # Use best mtry
      min.node.size = best_params_binomial$min_n,  # Use best min_n
      num.trees = 1000,
      probability = TRUE,                 # To get probabilities for thresholding
      num.threads = 4,
      importance = "permutation"
    )
    
    # Predict on the test set with probabilities
    score <- predict(mod, test, type = "response")
    
    # Apply the optimal cutoff threshold to classify
    test$preds <- ifelse(score$predictions[, "Fishing"] >= optimal_cutoff, "Fishing", "No_fishing")
    test$preds <- factor(test$preds, levels = c("No_fishing", "Fishing"))
    
    # Evaluate model
    CM <- caret::confusionMatrix(test$preds, test$fishing_presence)
    
    # Extract metrics
    accuracy <- CM$overall['Accuracy']
    kappa <- CM$overall['Kappa']
    precision <- CM$byClass['Pos Pred Value']  # Precision
    recall <- CM$byClass['Sensitivity']        # Recall
    f1 <- 2 * (precision * recall) / (precision + recall)  # F1 Score
    
    # Calculate ROC AUC
    roc_obj <- pROC::roc(response = test$fishing_presence, 
                         predictor = score$predictions[, "Fishing"])
    roc_auc <- pROC::auc(roc_obj)
    
    # Return a list of performance metrics
    performance_output <- data.frame(
      Fold = k,
      Accuracy = accuracy,
      Kappa = kappa,
      Precision = precision,
      Recall = recall,
      F1_Score = f1,
      ROC_AUC = roc_auc
    )
    
    print(performance_output)
    
    # Calculate variable importance
    var_imp <- mod$variable.importance
    var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)
    
    # # Create ggplot of variable importance
    # importance_plot <- ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    #   geom_bar(stat = "identity") +
    #   coord_flip() +
    #   labs(title = paste("Variable Importance for Fold", k),
    #        x = "Variable",
    #        y = "Importance (%)") +
    #   theme_minimal()
    # 
    # # Save the plot
    # ggsave(filename = paste0("figures/variable_importance_fold_", k, ".png"), plot = importance_plot)
    # 
    return(list(performance = performance_output, importance = var_imp_df))
    
  })
  
  return(cross_validation_binomial)
  
}