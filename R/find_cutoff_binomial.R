find_cutoff_binomial <- function(mpa_model, folds){
  
  cutoffs <- seq(0.1, 0.9, by = 0.01)
  
  # Function to calculate F1 scores for each cutoff
  calculate_f1 <- function(probs, truth, cutoffs) {
    
    sapply(cutoffs, function(cutoff) {
      
      preds <- factor(ifelse(probs >= cutoff, "Fishing", "No_fishing"), levels = c("No_fishing", "Fishing"))
      conf_matrix <- caret::confusionMatrix(preds, truth)
      f1 <- conf_matrix$byClass["F1"]
      return(f1)
      
    })
  }
  
  #Loop through spatial folds and calculate cutoff
  optimal_cutoffs <- sapply(1:10, function(k) {
    
    # extracting the training and testing indices
    # this way only works with foldID
    trainSet <- which(folds != k) # training set indices
    testSet <- which(folds == k) # testing set indices
    
    train_data <-  mpa_model[trainSet, ] 
    test_data <-mpa_model[testSet, ]
    
    # Train the model on the kth fold
    model <- ranger(
      formula_binomial, 
      data = train_data, 
      mtry = best_params_binomial$mtry, 
      min.node.size = best_params_binomial$min_n,
      num.trees = 500,
      probability = TRUE
    )
    
    # Make predictions on the test data
    predictions <- predict(model, data = test_data, type = "response")
    probs <- predictions$predictions[, "Fishing"]
    
    # Calculate F1 scores for all cutoffs
    f1_scores <- calculate_f1(probs, test_data$fishing_presence, cutoffs)
    
    # Return the optimal cutoff for this fold
    optimal_cutoff <- cutoffs[which.max(f1_scores)]
    return(optimal_cutoff)
  })
  
  # Average the optimal cutoffs across folds
  average_optimal_cutoff <- mean(optimal_cutoffs)
  
  return(average_optimal_cutoff)
  
}

