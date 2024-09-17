find_cutoff_binomial <- function(mpa_model, folds, formula_binomial, pred_var) {
  
  cutoffs <- seq(0.1, 0.9, by = 0.01)
  
  # Function to calculate F1 scores for each cutoff
  calculate_f1 <- function(probs, truth, cutoffs) {
    sapply(cutoffs, function(cutoff) {
      # Classify based on the current cutoff
      preds <- factor(ifelse(probs >= cutoff, "Fishing", "No_fishing"), levels = c("No_fishing", "Fishing"))
      # Create a confusion matrix
      conf_matrix <- caret::confusionMatrix(preds, truth)
      # Return the F1 score for this cutoff
      return(conf_matrix$byClass["F1"])
    })
  }
  
  # Loop through the folds and calculate optimal cutoff for each fold
  optimal_cutoffs <- sapply(1:length(folds), function(k) {
    
    # Extract the training and testing data for the k-th fold
    fold <- folds$splits[[k]]
    train_data <- analysis(fold)
    test_data <- assessment(fold)
    
    # Train the GLM model on the k-th fold
    model <- glm(formula_binomial, data = train_data, family = binomial())
    
    # Predict probabilities on the test set
    predicted_probabilities <- predict(model, newdata = test_data, type = "response",re.form = ~0)
    
    # Calculate F1 scores for all cutoff values
    f1_scores <- calculate_f1(predicted_probabilities, test_data[[pred_var]], cutoffs)
    
    # Find the cutoff that maximizes the F1 score
    optimal_cutoff <- cutoffs[which.max(f1_scores)]
    return(optimal_cutoff)
  })
  
  # Average the optimal cutoffs across all folds
  average_optimal_cutoff <- mean(optimal_cutoffs)
  
  return(average_optimal_cutoff)
}
