#' Find Optimal Cutoff for Binomial Model
#'
#' This function determines the optimal probability cutoff for classifying fishing presence 
#' in a binomial logistic regression model using **cross-validation**.
#'
#' @param mpa_model A dataframe containing the data for modeling.
#' @param folds A resampling object (`rsample` object) containing cross-validation folds.
#' @param formula_binomial A formula specifying the binomial logistic regression model.
#' @param pred_var A string specifying the name of the response variable in `mpa_model`.
#'
#' @return A numeric value representing the **average optimal cutoff** across all cross-validation folds.
#'
#' @details
#' 1. **Defines a sequence of probability cutoffs** from `0.1` to `0.9` (step `0.01`).
#' 2. **Trains a binomial logistic regression model** (`glm()`) for each fold.
#' 3. **Predicts probabilities** on the test set.
#' 4. **Computes F1 scores** for each cutoff using `caret::confusionMatrix()`.
#' 5. **Selects the cutoff that maximizes the F1 score** for each fold.
#' 6. **Averages the optimal cutoffs across all folds** to determine the final value.
#'
#' @examples
#' optimal_cutoff <- find_cutoff_binomial(mpa_model, folds, fishing_presence_2023 ~ ., "fishing_presence_2023")
#'

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
