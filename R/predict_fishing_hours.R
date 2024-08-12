predict_fishing_hours <- function(mpa_model_regression, formula_regression,
                                  best_params_regression, mpa_fishing_presence){
  
  #Train full model
  mod_regression_final <- fitme(mpa_fishing_GFW_log ~ fishing_2022_log * fishing + 
                                  iucn_cat + area_correct + length_matched + 
                 seamount_distance + mean_sst + sd_sst + mean_chl + sd_chl + 
                 depth +  + ais_reception_positions_per_day_class_A + 
                  dist_to_shore + gdp + dist_to_port +  Matern(1 | X + Y), 
               data = mpa_model_regression, family=gaussian(),
               control.HLfit=list(NbThreads = 6))
  
  save(mod_regression_final, file = "output/mod_regression_final.Rdata")
  
  #Check residuals
  residuals <- residuals(mod, type = "pearson")
  fitted_values <- fitted(mod)
  
  # Residuals vs Fitted
  ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values",
         y = "Residuals") +
    theme_minimal()
  
  # Q-Q plot of residuals
  ggplot(data = NULL, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q-Q Plot of Residuals",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Residuals vs a specific predictor, e.g., `fishing`
  ggplot(data = mpa_model_regression, aes(x = fishing_2022_log, y = residuals)) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    labs(title = "Residuals vs Fishing",
         x = "fishing_2022_log",
         y = "Residuals") +
    theme_minimal()
  
  # Plot Residuals Absolute Value vs Fitted Values
  ggplot(data = NULL, aes(x = fitted_values, y = abs(residuals))) +
    geom_point() +
    geom_smooth(method = "loess", color = "red") +
    labs(title = "Absolute Residuals vs Fitted Values",
         x = "Fitted Values",
         y = "Absolute Residuals") +
    theme_minimal()
  
  # Histogram of residuals
  ggplot(data = NULL, aes(x = residuals)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    labs(title = "Histogram of Residuals",
         x = "Residuals",
         y = "Frequency") +
    theme_minimal()
  
  #Predict updated
  mpa_model_predict_regression  <- mpa_fishing_presence %>%
    mutate(fishing_matched = log(fishing_matched),
           fishing = log(fishing)) %>%
    filter(fishing_presence_predicted == "Fishing") %>% 
    #Predict only on MPAs with at least one unmatched fishing vessel
    filter(unmatched_fishing > 0) %>%
    #Update the number of vessels with the unmatched number of vessels, and length of all vessels
    dplyr::select(-c(length_matched)) %>%
    dplyr::rename(length_matched = "length_all") 
  
  # Predict on new data
  # score <- predict(mod_regression_final, mpa_model_predict_regression)$predictions
  
  # Predict on the test set
  score <- predict(mod_regression_final, newdata = mpa_model_predict_regression)
  
  # Get predictions with confidence intervals
  ci <- get_intervals(mod, newdata = test, intervals = "predVar", level = 0.95)
  
  # Backtransform the predictions using the smearing coefficient
  mpa_model_predict_regression$predicted_fishing_effort_log <- score
  mpa_model_predict_regression$predicted_fishing_effort <- exp(score) 
  
  truc <- mpa_model_predict_regression %>% dplyr::select(mpa_fishing_GFW_log, predicted_fishing_effort_log,
                                                         mpa_fishing_GFW, predicted_fishing_effort,
                                                         fishing_2022,fishing_matched, fishing)
  return(mpa_model_predict_regression)
  
  
  
}