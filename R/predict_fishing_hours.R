#' Predict Fishing Effort Using a Regression Model
#'
#' This function trains a **linear mixed-effects regression model** to predict fishing effort 
#' in Marine Protected Areas (MPAs) based on SAR and AIS data.
#'
#' @param mpa_model A dataframe containing MPA data, including environmental and socioeconomic covariates.
#' @param mpa_model_regression A dataframe used to train the regression model.
#' @param formula_regression A formula specifying the regression model.
#' @param mpa_fishing_presence A dataframe containing observed and predicted fishing presence data.
#' @param year An integer specifying the year for which predictions are made.
#'
#' @return A dataframe (`fishing_effort_predicted`) containing:
#' - `id_iucn`: Unique MPA identifier.
#' - `AIS_fishing_<year>`: Observed fishing effort from AIS data.
#' - `predicted_fishing_effort_<year>`: Predicted fishing effort.
#'
#' @details
#' 1. **Dynamically defines relevant column names** based on the input `year`.
#' 2. **Trains a linear mixed-effects regression model** (`lmer()`) on the full dataset.
#' 3. **Processes model coefficients**, renaming variables for clarity.
#' 4. **Filters MPAs for prediction**, selecting those with unmatched fishing detections and predicted fishing presence.
#' 5. **Predicts fishing effort** using the trained model.
#' 6. **Back-transforms log-transformed predictions** using the exponential function.
#' 7. **Combines observed and predicted fishing effort** into a final dataset.
#' 8. **Ensures predicted effort is not lower than AIS-observed effort**.
#' 9. **Saves model outputs** to `figures/supp/mod_regression_final_output_<year>.csv`.
#'
#' @examples
#' fishing_effort_predictions <- predict_fishing_hours(mpa_model, mpa_model_regression, 
#'                                                     log(fishing_2023) ~ ., mpa_fishing_presence, 2023)
#'

predict_fishing_hours <- function(mpa_model, mpa_model_regression, formula_regression, mpa_fishing_presence, year){
  
  # Dynamically create the relevant column names based on the year
  fishing_log_col <- paste0("fishing_", year, "_log")
  sum_all_log_col <- paste0("sum_all_", year, "_log")
  unmatched_fishing_col <- paste0("unmatched_fishing_", year)
  fishing_presence_predicted_col <- paste0("fishing_presence_predicted_", year)
  AIS_fishing_col <- paste0("AIS_fishing_", year)
  predicted_fishing_effort_col <- paste0("predicted_fishing_effort_", year)
  
  #Train full model
  mod_regression_final <- lmer(formula_regression, data = mpa_model_regression)
  
  # Process the model outputà
  mod_regression_final_output <- broom.mixed::tidy(mod_regression_final,effects = "fixed",conf.int = T) %>%
    dplyr::rename(Variable = "term") %>%
    mutate(Variable = case_when(
      Variable == "AIS_fishing_2021_log" ~ "AIS-observed fishing effort in 2021",
      Variable == "AIS_fishing_2022_log" ~ "AIS-observed fishing effort in 2022",
      Variable == "fishing_2022_log" ~ "Number of vessel detections",
      Variable == "fishing_2023_log" ~ "Number of vessel detections",
      Variable == "ais_reception_positions_per_day_class_A" ~ "AIS reception: Type A transponders (pings/day)", 
      Variable == "ais_reception_positions_per_day_class_B" ~ "AIS reception: Type B transponders (pings/day)",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "depth" ~ "Depth",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "Bathymetry" ~ "Depth",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate(across(where(is.numeric), round, digits = 2)) 
  
  write.csv(mod_regression_final_output, file = paste0("figures/supp/mod_regression_final_output",year,".csv"))
  
  # Predict fishing effort on the updated data
  mpa_model_predict_regression <- mpa_fishing_presence %>%
    st_drop_geometry() %>% 
    left_join(mpa_model, by = "id_iucn") %>%
    # Remove the existing column if it exists to avoid duplication
    dplyr::select(-all_of(fishing_log_col)) %>%
    # Recode variables: rename sum_all_*year*_log to fishing_*year*_log
    dplyr::rename(!!fishing_log_col := !!sym(sum_all_log_col)) %>%
    # Select only MPAs with predicted fishing 
    filter(!!sym(unmatched_fishing_col) > 0) %>%
    filter(!!sym(fishing_presence_predicted_col) == "Fishing") %>%
    dplyr::select(-all_of(fishing_presence_predicted_col))
  
  # Predict on the new data
  score <- predict(mod_regression_final, newdata = mpa_model_predict_regression, re.form= NA, allow.new.levels=T)

  # Backtransform the predictions using the exponential function
  mpa_model_predict_regression[[predicted_fishing_effort_col]] <- as.vector(exp(score))
  
  fishing_effort_predicted <- mpa_model %>%
    # Remove MPAs which we predicted on for final dataset
    filter(!id_iucn %in% mpa_model_predict_regression$id_iucn) %>%
    # For other MPAs, set known fishing effort from AIS 
    mutate(!!predicted_fishing_effort_col := !!sym(AIS_fishing_col)) %>%
    bind_rows(mpa_model_predict_regression) %>%
    # If predicted fishing effort is lower than AIS, set AIS
    mutate(!!predicted_fishing_effort_col := ifelse(!!sym(predicted_fishing_effort_col) < !!sym(AIS_fishing_col),
                                                    !!sym(AIS_fishing_col), !!sym(predicted_fishing_effort_col))) %>%
    dplyr::select(id_iucn, !!sym(AIS_fishing_col), !!sym(predicted_fishing_effort_col))
  
  return(fishing_effort_predicted)
  
}
