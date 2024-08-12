predict_fishing_presence <- function(mpa_model,formula_binomial, best_params_binomial, optimal_cutoff){
  
  #Train full model
  mod_binomial_final <- ranger(
    formula_binomial,
    data = mpa_model,
    mtry = best_params_binomial$mtry,            # Use best mtry
    min.node.size = best_params_binomial$min_n,  # Use best min_n
    num.trees = 1000,
    probability = TRUE,                 # To get probabilities for thresholding
    num.threads = 4,
    importance = "permutation"
  )
  
  #Predict updated
  mpa_model_predict_binomial <- mpa_model %>%
    #Update the number of vessels with the unmatched number of vessels
    dplyr::rename(fishing_matched = "fishing",
                  fishing = "sum_all") %>%
    #Predicting only for MPAs where at least one matched or unmatched was found
    filter(fishing > 0) 
  
  #predict on mpas with unmatched
  score <- predict(mod_binomial_final, mpa_model_predict_binomial, type = "response")
  
  # Apply the optimal cutoff threshold to classify
  mpa_model_predict_binomial$fishing_presence_predicted <- ifelse(score$predictions[, "Fishing"] >= optimal_cutoff, 
                                                                  "Fishing", "No_fishing")
  
  mpa_model_predict_binomial$fishing_presence_predicted <- factor(mpa_model_predict_binomial$fishing_presence_predicted, 
                                                                  levels = c("No_fishing", "Fishing"))
  
  summary(mpa_model_predict_binomial)
  
  return(mpa_model_predict_binomial)
  
}