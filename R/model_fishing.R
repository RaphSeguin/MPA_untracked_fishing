model_fishing <- function(){
  
  #Prep data for models
  mpa_model <- prep_model_data(MPA_covariates, mpa_wdpa_fishing)
  
  #Create spatial folds for spatial cross validation
  spatial_folds <- create_spatial_folds(MPA_covariates)
  folds <- spatial_folds$folds_ids
  
  #-----Binomial model-------
  
  #Formula for binomial model
  # Response variable
  yvar <- "fishing_presence"
  
  # Predictor variables
  xvars <- c("iucn_cat", "area_correct", 
             "seamount_distance", "mean_sst", "sd_sst", 
             "mean_chl", "sd_chl", "depth", 
             "ais_reception_positions_per_day_class_A", 
             "ais_reception_positions_per_day_class_B",
             "dist_to_shore", "gdp", "X", "Y", 
             "dist_to_port", "fishing")
  
  # Create the formula 
  formula_binomial <- as.formula(paste(yvar, "~", paste(xvars, collapse = " + ")))
  
  #Tune binomial model
  best_params_binomial <- tune_binomial_model(mpa_model, formula_binomial)
  save(best_params_binomial, file = "output/best_params_binomial.Rdata")
  
  #Find the optimal cutoff threshold
  optimal_cutoff <- find_cutoff_binomial(mpa_model, folds)

  #Train and test the binomial model
  binomial_performance <- cv_binomial(mpa_model, folds, best_params_binomial, optimal_cutoff, formula_binomial)
  
  # Combine performance metrics across all folds into a single dataframe
  performance_metrics <- do.call(rbind, lapply(binomial_performance, function(res) res$performance))
  
  # Calculate the average of each performance metric across the folds
  average_performance_df <- data.frame(
    Mean_Accuracy = mean(performance_metrics$Accuracy),
    Mean_Kappa = mean(performance_metrics$Kappa),
    Mean_Precision = mean(performance_metrics$Precision),
    Mean_Recall = mean(performance_metrics$Recall),
    Mean_F1_Score = mean(performance_metrics$F1_Score),
    Mean_ROC_AUC = mean(performance_metrics$ROC_AUC)
  )
  
  # Combine variable importance across all folds into a single dataframe
  all_importance <- do.call(rbind, lapply(binomial_performance, function(res) res$importance))
  
  # Calculate the average importance for each variable across the folds
  average_importance <- aggregate(Importance ~ Variable, data = all_importance, mean)
  
  # Calculate the percentage of the average importance
  average_importance$Importance_Percentage <- (average_importance$Importance / sum(average_importance$Importance)) * 100
  
  # View the dataframe of average variable importance
  print("Average Variable Importance:")
  print(average_importance)
  
  #-----Regression model-------
  
  #Add image count? 
  
  #Bilan
  
  #If i train my model on MPAs with fishing and fishing vessels only the importance of fishing vessels
  #is more recognized
  
  #If I remove fihsing hours in 2022 then the model is bad, especially at higher values, but predicts more fishing hours
  
  #Formula for binomial model
  # Response variable
  yvar <- "mpa_fishing_GFW_log"
  
  # Predictor variables
  xvars <- c("iucn_cat", "area_correct", "length_matched",
             "seamount_distance", "mean_sst", "sd_sst", 
             "mean_chl", "sd_chl", "depth", "fishing_2022_log",
             "ais_reception_positions_per_day_class_A", 
             "ais_reception_positions_per_day_class_B",
             "dist_to_shore", "gdp", "X", "Y", 
             "dist_to_port", "fishing")
  
  # Create the formula 
  formula_regression <- as.formula(paste(yvar, "~", paste(xvars, collapse = " + ")))

  #Adjust model data for regression
  mpa_model_regression <- mpa_model %>%
    filter(mpa_fishing_GFW > 0) %>%
    filter(fishing > 0) %>%
    mutate(fishing = arm::rescale(log(fishing)),
           sum_all = arm::rescale(log(sum_all))) %>%
    distinct(X,Y, .keep_all = T)
  
  #Validation set
  # Determine the 90th quantile of the fishing column
  high_fishing_threshold <- quantile(mpa_model_regression$mpa_fishing_GFW, 0.9)
  
  # Create a stable validation set with high fishing values
  validation_set <- mpa_model_regression[mpa_model_regression$mpa_fishing_GFW > high_fishing_threshold, ]
  
  # Remove the validation set from the main dataset for cross-validation
  mpa_model_regression_no_val <- mpa_model_regression %>% filter(!id_iucn %in% validation_set$id_iucn)
  
  #Check for correlation between vars
  check_correlation(mpa_model_regression_no_val)
  
  #Spatial folds for regression
  spatial_folds_regression <- create_spatial_folds_regression(MPA_covariates, mpa_model_regression_no_val)
  folds_regression = spatial_folds_regression$folds_ids

  #Tune regression model
  best_params_regression <- tune_regression_model(mpa_model_regression, formula_regression) 
  save(best_params_regression, file = "output/best_params_regression.Rdata")
  
  #Regression performance
  regression_performance <- cv_regression(mpa_model_regression, folds_regression, 
                                          best_params_regression, formula_regression)
  
  # Combine performance metrics across all folds into a single dataframe
  performance_metrics <- do.call(rbind, lapply(regression_performance, function(res) res$performance))
  
  # Calculate the average of each performance metric across the folds
  average_performance_df <- data.frame(
    Mean_RMSE = mean(performance_metrics$RMSE),
    Mean_MAE = mean(performance_metrics$MAE),
    Mean_MedAE = mean(performance_metrics$MedAE),
    Mean_R2 = mean(performance_metrics$R2),
    Mean_RSQ_TRAD = mean(performance_metrics$RSQ_TRAD),
    Mean_NRMSE = mean(performance_metrics$NRMSE),
    Mean_CVRMSE = mean(performance_metrics$CVRMSE)
  )
  
  # Combine variable importance across all folds into a single dataframe
  all_importance <- do.call(rbind, lapply(regression_performance, function(res) res$importance))
  
  # Calculate the average importance for each variable across the folds
  average_importance <- all_importance %>%
    group_by(Variable) %>%
    summarise(MeanImportance = mean(Importance)) %>%
    arrange(desc(MeanImportance))
  
  # Calculate the percentage of the average importance
  average_importance$Importance_Percentage <- (average_importance$MeanImportance / sum(average_importance$MeanImportance)) * 100
  
  #Predicting fishing presence and number of fishing hours
  mpa_fishing_presence <- predict_fishing_presence(mpa_model,formula_binomial, best_params_binomial, optimal_cutoff)
  
  #Predicting the number of fishing hours
  mpa_fishing_hours <- predict_fishing_hours()
  
  
}