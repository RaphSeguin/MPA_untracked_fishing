model_fishing <- function(){
  
  #Prep data for models
  sf::sf_use_s2(F)
  mpa_model <- prep_model_data(MPA_covariates, mpa_wdpa)
  
  # Percentage change
  temp_2022 <- mpa_model %>%
    st_drop_geometry() %>%
    mutate(percentage_change_2022 = (AIS_fishing_2022 - AIS_fishing_2021) / AIS_fishing_2021 * 100) %>%
    filter(AIS_fishing_2021 > 1000 | AIS_fishing_2022 > 1000) %>%
    filter(percentage_change_2022 > 100)

  #
  temp_2023 <- mpa_model %>%
    st_drop_geometry() %>%
    mutate(percentage_change_2023 = (AIS_fishing_2023 - AIS_fishing_2022) / AIS_fishing_2022 * 100) %>%
    filter(AIS_fishing_2023 > 1000 | AIS_fishing_2022 > 1000) %>%
    filter(percentage_change_2023 > 100)

  mpa_model <- mpa_model %>% filter(!id_iucn %in% temp_2022$id_iucn) %>% filter(!id_iucn %in% temp_2023$id_iucn)
  
  #-----Binomial model-------
  
  #Formula for binomial model
  # Response variable
  formula_binomial_2022 <- as.formula(fishing_presence_2022 ~ AIS_fishing_2021_log + 
                                        SAR_matched_presence_2022 + depth + dist_to_shore + 
                                        mean_sst +  mean_chl + 
                                   ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B)
  
  formula_binomial_2023 <- as.formula(fishing_presence_2023 ~ AIS_fishing_2022_log + 
                                        SAR_matched_presence_2023 + depth + dist_to_shore + 
                                        mean_sst +  mean_chl + 
                                        ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B)
                                        # Matern(1 | X + Y))
  
  #Tune binomial model
  # best_params_binomial <- tune_binomial_model(mpa_model, formula_binomial)
  # save(best_params_binomial, file = "output/best_params_binomial.Rdata")
  
  # Create 10-fold cross-validation folds
  sf::sf_use_s2(T)
  folds <-  spatial_clustering_cv(mpa_model, v = 10)

  #Find the optimal cutoff threshold
  optimal_cutoff_2022 <- find_cutoff_binomial(mpa_model, folds, formula_binomial_2022,"fishing_presence_2022")
  optimal_cutoff_2023 <- find_cutoff_binomial(mpa_model, folds, formula_binomial_2023,"fishing_presence_2023")

  #Train and test the binomial model
  binomial_performance_2022 <- cv_binomial_spamm(mpa_model, folds, optimal_cutoff_2022, formula_binomial_2022, "fishing_presence_2022")
  write.csv(binomial_performance_2022, file = "figures/supp/binomial_performance_2022.csv")
  
  binomial_performance_2023 <- cv_binomial_spamm(mpa_model, folds, optimal_cutoff_2023, formula_binomial_2023, "fishing_presence_2023")
  write.csv(binomial_performance_2023, file = "figures/supp/binomial_performance_2023.csv")
  #-----Regression model-------
  
  # Formula for regression model
  formula_regression_2022 <- as.formula(AIS_fishing_2022_log ~ AIS_fishing_2021_log + fishing_2022_log +
                                          depth + dist_to_shore + 
                                   mean_sst +  mean_chl + 
                                   ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B +
                                     (1|parent_iso))
                                   # Matern(1 | X + Y))
  
  formula_regression_2023 <- as.formula(AIS_fishing_2023_log ~ AIS_fishing_2022_log + fishing_2023_log +
                                     depth + dist_to_shore  + 
                                     mean_sst +  mean_chl+ 
                                     ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B+
                                       (1|parent_iso))
                                     # Matern(1 | X + Y))

  #Adjust model data for regression
  mpa_model_regression_2022 <- mpa_model %>%
    filter(AIS_fishing_2022 > 0 & fishing_2022 > 0) %>%
    distinct(X, Y, .keep_all = TRUE)
  
  mpa_model_regression_2023 <- mpa_model %>%
    filter(AIS_fishing_2023 > 0 & fishing_2023 > 0) %>%
    distinct(X, Y, .keep_all = TRUE)
  
  # Validation set
  high_fishing_threshold <- quantile(mpa_model_regression_2022$AIS_fishing_2022, 0.9)
  validation_set_2022 <- mpa_model_regression_2022[mpa_model_regression_2022$AIS_fishing_2022 > high_fishing_threshold, ]
  mpa_model_regression_no_val_2022 <- mpa_model_regression_2022 %>% dplyr::filter(!id_iucn %in% validation_set_2022$id_iucn)
  
  # Validation set
  high_fishing_threshold <- quantile(mpa_model_regression_2023$AIS_fishing_2023, 0.9)
  validation_set_2023 <- mpa_model_regression_2023[mpa_model_regression_2023$AIS_fishing_2023 > high_fishing_threshold, ]
  mpa_model_regression_no_val_2023 <- mpa_model_regression_2023 %>% filter(!id_iucn %in% validation_set_2023$id_iucn)
  
  set.seed(234)
  # Create 10-fold cross-validation folds
  folds_regression_2022 <- spatial_clustering_cv(mpa_model_regression_no_val_2022, v = 10)
  folds_regression_2023 <- spatial_clustering_cv(mpa_model_regression_no_val_2023, v = 10)
  
  # Cross-validation for regression
  regression_performance_2022 <- cv_regression_spammm(folds_regression_2022, formula_regression_2022, validation_set_2022, "AIS_fishing_2022")
  write.csv(regression_performance_2022, file = "figures/supp/regression_performance_2022.csv")
  
  regression_performance_2023 <- cv_regression_spammm(folds_regression_2023, formula_regression_2023, validation_set_2023, "AIS_fishing_2023")
  write.csv(regression_performance_2023, file = "figures/supp/regression_performance_2023.csv")
  
  sf_use_s2(F)
  
  #Predicting fishing presence and number of fishing hours
  fishing_presence_2022 <- predict_fishing_presence(mpa_model,formula_binomial_2022, optimal_cutoff_2022, 2022)
  fishing_presence_2023 <- predict_fishing_presence(mpa_model,formula_binomial_2023, optimal_cutoff_2023, 2023)
  
  #Predicting the number of fishing hours
  fishing_hours_2022 <- predict_fishing_hours(mpa_model %>% st_drop_geometry(), 
                                              mpa_model_regression_2022 %>% st_drop_geometry(), 
                                              formula_regression_2022, fishing_presence_2022, 2022)
  
  summary(fishing_hours_2022)
  
  fishing_hours_2023 <- predict_fishing_hours(mpa_model %>% st_drop_geometry(), 
                                              mpa_model_regression_2023 %>% st_drop_geometry(), 
                                              formula_regression_2023, fishing_presence_2023, 2023)
  
  summary(fishing_hours_2023)
  
  #Plot figures for both models
  plot_figures_binomial_model(mpa_model, formula_binomial_2022, formula_binomial_2023)
  plot_figures_regression_model(mpa_model_regression, 
                                mpa_model_regression_2022, mpa_model_regression_2023,
                                formula_regression_2022,formula_regression_2023)
  
  #Plot figures
  figures_fishing_predictions(mpa_model,fishing_presence_2022, fishing_presence_2023, fishing_hours_2022, fishing_hours_2023)
  
  plot_fishing_density(mpa_model,
                       fishing_presence_2022, fishing_presence_2023,
                       fishing_hours_2022, fishing_hours_2023)
  
  #Predictions by IUCN category
  figures_fishing_predictions_iucn(mpa_model,
                                   fishing_presence_2022, fishing_presence_2023,
                                   fishing_hours_2022, fishing_hours_2023)
  
  #Plot performance of model across all folds
  plot_performance_distribution()
  
}
