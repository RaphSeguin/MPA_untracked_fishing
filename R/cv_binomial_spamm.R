cv_binomial_spamm <- function(mpa_model, folds, optimal_cutoff, formula_binomial, pred_var){
  
  #Test final model performance
  cross_validation_binomial <- lapply(1:nrow(folds),function(k){
    
    # Extract the training and testing data for the k-th fold
    fold <- folds$splits[[k]]
    train <- analysis(fold)
    test <- assessment(fold)
    
    # Train the model using the best hyperparameters
    mod_binomial <- glm(formula_binomial,
                 data = train, family=binomial())
    
    # Predict on the test set with probabilities
    predicted_probabilities <- predict(mod_binomial, newdata = test, type = "response")
    
    # Apply the optimal cutoff threshold to classify
    test$preds <- factor(ifelse(predicted_probabilities >= optimal_cutoff, "Fishing", "No_fishing"), 
                         levels = c("No_fishing", "Fishing"))
    
    # Evaluate model performance
    CM <- caret::confusionMatrix(test$preds, test[[pred_var]])
    
    # Extract performance metrics
    accuracy <- CM$overall['Accuracy']
    kappa <- CM$overall['Kappa']
    precision <- CM$byClass['Pos Pred Value']  # Precision
    recall <- CM$byClass['Sensitivity']        # Recall
    f1 <- 2 * (precision * recall) / (precision + recall)  # F1 Score
    
    # Calculate ROC AUC using predicted probabilities
    roc_obj <- pROC::roc(response = test[[pred_var]], predictor = predicted_probabilities)
    roc_auc <- pROC::auc(roc_obj)
    
    # Return performance metrics as a data frame
    performance_output <- data.frame(
      Fold = k,
      Accuracy = accuracy,
      Kappa = kappa,
      Precision = precision,
      Recall = recall,
      F1_Score = f1,
      ROC_AUC = roc_auc
    )
    
    return(performance_output)
    
  })
  
  # Combine results from all folds into a single data frame
  results_cv_binomial <- do.call(rbind, cross_validation_binomial) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  # Compute and print mean R2 and R2_VAL
  mean_F1 <- mean(results_cv_binomial$F1_Score)
  mean_acc <- mean(results_cv_binomial$Accuracy)
  
  cat("Mean F1 across folds: ", mean_F1, "\n")
  cat("Mean Accuracy across folds: ", mean_acc, "\n")
  
  return(results_cv_binomial)
}