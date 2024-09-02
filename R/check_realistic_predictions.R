check_realistic_predictions <- function(formula_regression, fishing_var, validation_set, pred_var){
  
  # Calculate the min and max for the specified column
  min_value <- min(mpa_model[[fishing_var]], na.rm = TRUE) + 1
  max_value <- max(mpa_model[[fishing_var]], na.rm = TRUE)
  
  #Adjust model data for regression
  mpa_model_check_preds <- mpa_model %>%
    filter(AIS_fishing_2022 > 0 & fishing_2022 > 0) %>%
    mutate(fishing_2022_new = fishing_2022 - runif(n(), min = 0, max = fishing_2022 - 1e-8)) %>%
    mutate(fishing_2022_log = log(fishing_2022_new)) %>%
    distinct(X, Y, .keep_all = TRUE)

  # Run model
  mod <- lmer(formula_regression,
              data = mpa_model_check_preds)
  
  #Predict on dataset with real information
  real_data <- mpa_model %>%
    filter(AIS_fishing_2022 > 0 & fishing_2022 > 0) %>%
    distinct(X, Y, .keep_all = TRUE)
  
  #Predict on real data
  test_preds_log <- predict(mod, newdata = real_data, re.form = NA)
  real_data$preds <- exp(test_preds_log)
  
  #Compare predictions
  ggplot(real_data, aes(log(preds), log(AIS_fishing_2022))) + 
    geom_point()
  
  # Calculate performance metrics on the test set
  rmse <- caret::RMSE(real_data$preds, real_data[[pred_var]])
  mae <- caret::MAE(real_data$preds, real_data[[pred_var]])
  medae <- median(abs(real_data[[pred_var]] - real_data$preds))
  r2 <- caret::R2(real_data$preds, real_data[[pred_var]])
  
  
}
