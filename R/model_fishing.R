#' Train and Evaluate Fishing Prediction Models
#'
#' This function **trains, evaluates, and visualizes** binomial and regression models 
#' for fishing presence and fishing effort predictions across the years **2022, 2023, and 2024**.
#'
#' @return Saves the following outputs:
#' - `figures/supp/spatial_blocks_biomial.jpg`: Visualization of spatial blocks for binomial model cross-validation.
#' - `figures/supp/binomial_performance.csv`: Performance metrics for binomial models.
#' - `figures/supp/spatial_blocks_A4.jpg`: Visualization of spatial blocks for regression model cross-validation.
#' - `figures/supp/regression_performance.csv`: Performance metrics for regression models.
#' - `figures/supp/full_performance.jpg`: Combined visualization of binomial and regression model performance.
#' - Various plots for model results, including IUCN category-specific predictions.
#'
#' @details
#' 1. **Prepares MPA model data** (`mpa_model`).
#' 2. **Filters out extreme changes** in AIS fishing effort.
#' 3. **Trains binomial models**:
#'    - Defines logistic regression formulas for predicting fishing presence.
#'    - Uses **spatial cross-validation (10-fold)** to validate models.
#'    - Determines **optimal cutoff thresholds** for classification.
#'    - Evaluates model performance using **F1 Score, Precision, Recall, ROC AUC**.
#' 4. **Trains regression models**:
#'    - Defines mixed-effects formulas to predict fishing effort.
#'    - Uses **spatial cross-validation (10-fold)** for validation.
#'    - Evaluates model performance using **RMSE, MAE, MedAE, R²**.
#' 5. **Predicts fishing presence and hours** for **2022, 2023, and 2024**.
#' 6. **Generates various figures**:
#'    - Model coefficient plots.
#'    - Spatial fishing effort predictions.
#'    - Fishing effort density visualizations.
#'    - IUCN category-based fishing predictions.
#'    - Model performance across validation folds.
#' 7. **Saves results as CSV files and high-resolution images**.
#'
#' @examples
#' model_fishing()
#'


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
    
    temp_2023 <- mpa_model %>%
      st_drop_geometry() %>%
      mutate(percentage_change_2023 = (AIS_fishing_2023 - AIS_fishing_2022) / AIS_fishing_2022 * 100) %>%
      filter(AIS_fishing_2023 > 1000 | AIS_fishing_2022 > 1000) %>%
      filter(percentage_change_2023 > 100)
    
    temp_2024 <- mpa_model %>%
      st_drop_geometry() %>%
      mutate(percentage_change_2024 = (AIS_fishing_2024 - AIS_fishing_2023) / AIS_fishing_2023 * 100) %>%
      filter(AIS_fishing_2024 > 1000 | AIS_fishing_2023 > 1000) %>%
      filter(percentage_change_2024 > 100)
    
    mpa_model <- mpa_model %>%
      filter(!id_iucn %in% temp_2022$id_iucn) %>%
      filter(!id_iucn %in% temp_2023$id_iucn) %>%
      filter(!id_iucn %in% temp_2024$id_iucn)
    
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
    
    formula_binomial_2024 <- as.formula(fishing_presence_2024 ~ AIS_fishing_2023_log + 
                                          SAR_matched_presence_2024 + depth + dist_to_shore + 
                                          mean_sst +  mean_chl + 
                                          ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B)
    
    # Create 10-fold cross-validation folds
    
    set.seed(123)
    
    sf::sf_use_s2(T)
    folds <- spatial_clustering_cv(mpa_model, v = 10)
    
    spatial_blocks_biomial <- autoplot(folds) + 
      theme_map(base_size = 20) + 
      theme(legend.position = "bottom") 
    
    ggsave(spatial_blocks_biomial, file = "figures/supp/spatial_blocks_biomial.jpg", width = 297, height = 210, units = "mm", dpi = 300)
    
    # Find the optimal cutoff threshold
    optimal_cutoff_2022 <- find_cutoff_binomial(mpa_model, folds, formula_binomial_2022, "fishing_presence_2022")
    optimal_cutoff_2023 <- find_cutoff_binomial(mpa_model, folds, formula_binomial_2023, "fishing_presence_2023")
    optimal_cutoff_2024 <- find_cutoff_binomial(mpa_model, folds, formula_binomial_2024, "fishing_presence_2024")
    
    # Train and test the binomial model
    binomial_performance_2022 <- cv_binomial_spamm(mpa_model, folds, optimal_cutoff_2022, formula_binomial_2022, "fishing_presence_2022")
    binomial_performance_2022 <- binomial_performance_2022 %>% mutate(year = 2022)
    
    binomial_performance_2023 <- cv_binomial_spamm(mpa_model, folds, optimal_cutoff_2023, formula_binomial_2023, "fishing_presence_2023")
    binomial_performance_2023 <- binomial_performance_2023 %>% mutate(year = 2023)
    
    binomial_performance_2024 <- cv_binomial_spamm(mpa_model, folds, optimal_cutoff_2024, formula_binomial_2024, "fishing_presence_2024")
    binomial_performance_2024 <- binomial_performance_2024 %>% mutate(year = 2024)
    
    binomial_performance <- rbind(binomial_performance_2022, binomial_performance_2023, binomial_performance_2024)
    write.csv(binomial_performance, file = "figures/supp/binomial_performance.csv")
    
    #-----Regression model-------
    
    # Formula for regression model
    formula_regression_2022 <- as.formula(AIS_fishing_2022_log ~ AIS_fishing_2021_log + fishing_2022_log +
                                            depth + dist_to_shore + 
                                            mean_sst +  mean_chl + 
                                            ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B +
                                            (1|parent_iso))
    
    formula_regression_2023 <- as.formula(AIS_fishing_2023_log ~ AIS_fishing_2022_log + fishing_2023_log +
                                            depth + dist_to_shore  + 
                                            mean_sst +  mean_chl+ 
                                            ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B+
                                            (1|parent_iso))
    
    formula_regression_2024 <- as.formula(AIS_fishing_2024_log ~ AIS_fishing_2023_log + fishing_2024_log +
                                            depth + dist_to_shore  + 
                                            mean_sst +  mean_chl+ 
                                            ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B+
                                            (1|parent_iso))
    
    # Adjust model data for regression
    mpa_model_regression_2022 <- mpa_model %>%
      filter(AIS_fishing_2022 > 0 & fishing_2022 > 0) %>%
      distinct(X, Y, .keep_all = TRUE)
    
    mpa_model_regression_2023 <- mpa_model %>%
      filter(AIS_fishing_2023 > 0 & fishing_2023 > 0) %>%
      distinct(X, Y, .keep_all = TRUE)
    
    mpa_model_regression_2024 <- mpa_model %>%
      filter(AIS_fishing_2024 > 0 & fishing_2024 > 0) %>%
      distinct(X, Y, .keep_all = TRUE)
    
    # Validation set
    high_fishing_threshold <- quantile(mpa_model_regression_2022$AIS_fishing_2022, 0.9)
    validation_set_2022 <- mpa_model_regression_2022[mpa_model_regression_2022$AIS_fishing_2022 > high_fishing_threshold, ]
    mpa_model_regression_no_val_2022 <- mpa_model_regression_2022 %>% filter(!id_iucn %in% validation_set_2022$id_iucn)
    
    high_fishing_threshold <- quantile(mpa_model_regression_2023$AIS_fishing_2023, 0.9)
    validation_set_2023 <- mpa_model_regression_2023[mpa_model_regression_2023$AIS_fishing_2023 > high_fishing_threshold, ]
    mpa_model_regression_no_val_2023 <- mpa_model_regression_2023 %>% filter(!id_iucn %in% validation_set_2023$id_iucn)
    
    high_fishing_threshold <- quantile(mpa_model_regression_2024$AIS_fishing_2024, 0.9)
    validation_set_2024 <- mpa_model_regression_2024[mpa_model_regression_2024$AIS_fishing_2024 > high_fishing_threshold, ]
    mpa_model_regression_no_val_2024 <- mpa_model_regression_2024 %>% filter(!id_iucn %in% validation_set_2024$id_iucn)
    
    set.seed(234)
    # Create 10-fold cross-validation folds
    folds_regression_2022 <- spatial_clustering_cv(mpa_model_regression_no_val_2022, v = 10)
    folds_regression_2023 <- spatial_clustering_cv(mpa_model_regression_no_val_2023, v = 10)
    folds_regression_2024 <- spatial_clustering_cv(mpa_model_regression_no_val_2024, v = 10)
    
    spatial_blocks_regression_2022 <- autoplot(folds_regression_2022) + 
      theme_map(base_size = 16) + 
      theme(legend.position = "bottom") 
    
    spatial_blocks_regression_2023 <- autoplot(folds_regression_2023) + 
      theme_map(base_size = 16) + 
      theme(legend.position = "bottom") 
    
    spatial_blocks_regression_2024 <- autoplot(folds_regression_2024) + 
      theme_map(base_size = 16) + 
      theme(legend.position = "bottom") 
    
    # Arrange plots in a vertical format
    combined_plot <- spatial_blocks_regression_2022 / 
      spatial_blocks_regression_2023 / 
      spatial_blocks_regression_2024 
    
    ggsave( "figures/supp/spatial_blocks_A4.jpg", combined_plot, width = 210, height = 297, units = "mm", dpi = 300)
    
    # Cross-validation for regression
    regression_performance_2022 <- cv_regression_spammm(folds_regression_2022, formula_regression_2022, validation_set_2022, "AIS_fishing_2022")
    regression_performance_2022 <- regression_performance_2022 %>% mutate(year = 2022)
    write.csv(regression_performance_2022, file = "figures/supp/regression_performance_2022.csv")
    
    regression_performance_2023 <- cv_regression_spammm(folds_regression_2023, formula_regression_2023, validation_set_2023, "AIS_fishing_2023")
    regression_performance_2023 <- regression_performance_2023 %>% mutate(year = 2023)
    write.csv(regression_performance_2023, file = "figures/supp/regression_performance_2023.csv")
    
    regression_performance_2024 <- cv_regression_spammm(folds_regression_2024, formula_regression_2024, validation_set_2024, "AIS_fishing_2024")
    regression_performance_2024 <- regression_performance_2024 %>% mutate(year = 2024)
    
    regression_performance <- rbind(regression_performance_2022, regression_performance_2023, regression_performance_2024)
    write.csv(regression_performance, file = "figures/supp/regression_performance.csv")
    
    sf_use_s2(F)
    
    # Predicting fishing presence and number of fishing hours
    fishing_presence_2022 <- predict_fishing_presence(mpa_model, formula_binomial_2022, optimal_cutoff_2022, 2022)
    fishing_presence_2023 <- predict_fishing_presence(mpa_model, formula_binomial_2023, optimal_cutoff_2023, 2023)
    fishing_presence_2024 <- predict_fishing_presence(mpa_model, formula_binomial_2024, optimal_cutoff_2024, 2024)
    
    #
    nrow(fishing_presence_2022 %>% filter(fishing_presence_2022 == "Fishing"))/nrow(fishing_presence_2022)
    nrow(fishing_presence_2022 %>% filter(fishing_presence_predicted_2022 == "Fishing"))/nrow(fishing_presence_2022)
    nrow(fishing_presence_2022 %>% filter(fishing_presence_predicted_2022 == "Fishing" & fishing_presence_2022 =="No_fishing"))
    
    nrow(fishing_presence_2023 %>% filter(fishing_presence_2023 == "Fishing"))/nrow(fishing_presence_2023)
    nrow(fishing_presence_2023 %>% filter(fishing_presence_predicted_2023 == "Fishing"))/nrow(fishing_presence_2023)
    nrow(fishing_presence_2023 %>% filter(fishing_presence_predicted_2023 == "Fishing" & fishing_presence_2023 =="No_fishing"))
    
    nrow(fishing_presence_2024 %>% filter(fishing_presence_2024 == "Fishing"))/nrow(fishing_presence_2024)
    nrow(fishing_presence_2024 %>% filter(fishing_presence_predicted_2024 == "Fishing"))/nrow(fishing_presence_2024)
    nrow(fishing_presence_2024 %>% filter(fishing_presence_predicted_2024 == "Fishing" & fishing_presence_2024 =="No_fishing"))
    
    # Predicting the number of fishing hours
    fishing_hours_2022 <- predict_fishing_hours(mpa_model %>% st_drop_geometry(), mpa_model_regression_2022 %>% st_drop_geometry(), formula_regression_2022, fishing_presence_2022, 2022)
    fishing_hours_2023 <- predict_fishing_hours(mpa_model %>% st_drop_geometry(), mpa_model_regression_2023 %>% st_drop_geometry(), formula_regression_2023, fishing_presence_2023, 2023)
    fishing_hours_2024 <- predict_fishing_hours(mpa_model %>% st_drop_geometry(), mpa_model_regression_2024 %>% st_drop_geometry(), formula_regression_2024, fishing_presence_2024, 2024)
    
    sum(fishing_hours_2022$AIS_fishing_2022 + fishing_hours_2023$AIS_fishing_2023 + fishing_hours_2024$AIS_fishing_2024)
    sum(fishing_hours_2022$predicted_fishing_effort_2022)
    sum(fishing_hours_2023$predicted_fishing_effort_2023)
    sum(fishing_hours_2024$predicted_fishing_effort_2024)
    
    predicted <-  sum(fishing_hours_2022$predicted_fishing_effort_2022 + fishing_hours_2023$predicted_fishing_effort_2023 + fishing_hours_2024$predicted_fishing_effort_2024)
    ancient <- sum(fishing_hours_2022$AIS_fishing_2022 + fishing_hours_2023$AIS_fishing_2023 + fishing_hours_2024$AIS_fishing_2024)
    
    (predicted - ancient)/ancient

  #Plot figures for both models
  plot_figures_binomial_model(mpa_model, formula_binomial_2022, formula_binomial_2023, formula_binomial_2024)
  
  plot_figures_regression_model(mpa_model_regression, 
                                mpa_model_regression_2022, mpa_model_regression_2023,mpa_model_regression_2024,
                                formula_regression_2022,formula_regression_2023, formula_regression_2024)
  
  #Plot figures
  figures_fishing_predictions(mpa_model,fishing_presence_2022, fishing_presence_2023, fishing_presence_2024,
                              fishing_hours_2022, fishing_hours_2023, fishing_hours_2024)
  
  plot_fishing_density(mpa_model,
                       fishing_presence_2022, fishing_presence_2023,fishing_presence_2024,
                       fishing_hours_2022, fishing_hours_2023, fishing_hours_2024)
  
  #Predictions by IUCN category
  figures_fishing_predictions_iucn(mpa_model,
                                   fishing_presence_2022, fishing_presence_2023, fishing_presence_2024,
                                   fishing_hours_2022, fishing_hours_2023, fishing_hours_2024)
  
  #Plot performance of model across all folds
  plot_performance_distribution()
  
  
  
}
