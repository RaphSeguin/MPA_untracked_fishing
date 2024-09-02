find_cutoff_binomial <- function(mpa_model, folds, formula_binomial) {
  
  cutoffs <- seq(0.1, 0.9, by = 0.01)
  
  # Function to calculate F1 scores for each cutoff
  calculate_f1 <- function(probs, truth, cutoffs) {
    sapply(cutoffs, function(cutoff) {
      preds <- factor(ifelse(probs >= cutoff, "Fishing", "No_fishing"), levels = c("No_fishing", "Fishing"))
      conf_matrix <- caret::confusionMatrix(preds, truth)
      return(conf_matrix$byClass["F1"])
    })
  }
  
  # Loop through the folds and calculate optimal cutoff
  optimal_cutoffs <- sapply(1:length(folds), function(k) {
    
    # Extract the training and testing indices
    fold <- folds$splits[[k]]
    train_data <- analysis(fold)
    test_data <- assessment(fold)
    
    # Train the spaMM model on the kth fold
    model <- fitme(formula_binomial, 
                   data = train_data, 
                   family = binomial(),
                   control.HLfit = list(NbThreads = 4))
    
    # Make predictions on the test data
    predicted_probabilities <- predict(model, newdata = test_data, type = "response")
    
    # Calculate F1 scores for all cutoffs
    f1_scores <- calculate_f1(predicted_probabilities, test_data$fishing_presence, cutoffs)
    
    # Return the optimal cutoff for this fold
    optimal_cutoff <- cutoffs[which.max(f1_scores)]
    return(optimal_cutoff)
  })
  
  # Average the optimal cutoffs across folds
  average_optimal_cutoff <- mean(optimal_cutoffs)
  
  return(average_optimal_cutoff)
}
