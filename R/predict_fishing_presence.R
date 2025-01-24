#' Predict Fishing Presence Using a Binomial Model
#'
#' This function trains a **binomial logistic regression model** to predict fishing presence 
#' in Marine Protected Areas (MPAs) based on SAR and AIS data.
#'
#' @param mpa_model A dataframe containing the data for modeling.
#' @param formula_binomial A formula specifying the binomial logistic regression model.
#' @param optimal_cutoff A numeric value representing the best probability threshold for classification.
#' @param year An integer specifying the year for which predictions are made.
#'
#' @return A dataframe (`fishing_presence_data`) containing:
#' - `id_iucn`: Unique MPA identifier.
#' - `fishing_presence_<year>`: Observed fishing presence.
#' - `fishing_presence_predicted_<year>`: Predicted fishing presence.
#'
#' @details
#' 1. **Defines relevant column names** based on the input `year`.
#' 2. **Trains a binomial logistic regression model** (`glm()`) on the full dataset.
#' 3. **Processes model coefficients**, renaming variables for clarity.
#' 4. **Filters MPAs for prediction**, keeping only those with unmatched fishing detections but no recorded fishing presence.
#' 5. **Predicts fishing presence probabilities** for these MPAs.
#' 6. **Applies the optimal probability cutoff** to classify fishing presence.
#' 7. **Combines observed and predicted fishing presence** into a final dataset.
#' 8. **Saves model outputs** to `figures/supp/mod_binomial_final_output_<year>.csv`.
#'
#' @examples
#' fishing_predictions <- predict_fishing_presence(mpa_model, fishing_presence_2023 ~ ., 0.5, 2023)
#'

predict_fishing_presence <- function(mpa_model,formula_binomial, optimal_cutoff, year){
  
  #create the relevant column names based on the year
  SAR_presence_col <- paste0("SAR_matched_presence_", year)
  SAR_all_presence_col <- paste0("SAR_all_presence_", year)
  fishing_presence_col <- paste0("fishing_presence_", year)
  fishing_presence_predicted_col <- paste0("fishing_presence_predicted_", year)
  
  #Train full model
  mod_binomial_final <- glm(formula_binomial,
                              data = mpa_model, family=binomial())
  
  # Process the model outputà
  mod_binomial_final_output <- tidy(mod_binomial_final) %>%
    dplyr::rename(Variable = "term") %>%
    mutate(Variable = case_when(
      Variable == "AIS_fishing_2021_log" ~ "AIS-observed fishing effort in 2021",
      Variable == "AIS_fishing_2022_log" ~ "AIS-observed fishing effort in 2022",
      Variable == "SAR_matched_presence_2022SAR" ~ "Presence of SAR vessel detections",
      Variable == "SAR_matched_presence_2023SAR" ~ "Presence of SAR vessel detections",
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
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001", p_value)) 
  
  write.csv(mod_binomial_final_output, file = paste0("figures/supp/mod_binomial_final_output_",year,".csv"))
  
  #Creatign data to predict on
  mpa_model_predict_binomial <- mpa_model %>%
    # Remove the existing column if it exists to avoid duplication
    dplyr::select(-all_of(SAR_presence_col)) %>%
    # Recode variables: rename sum_all_*year*_log to fishing_*year*_log
    dplyr::rename(!!SAR_presence_col := !!sym(SAR_all_presence_col)) %>%
    # Keep only records where at least one fishing vessel was detected and no fishing presence was recorded
    filter(!!sym(paste0("unmatched_fishing_", year)) > 0, !!sym(fishing_presence_col) == "No_fishing")
  
  # Make predictions
  predicted_probabilities <- predict(mod_binomial_final, newdata = mpa_model_predict_binomial, type = "response")
  
  # Apply the optimal cutoff threshold to classify fishing presence
  mpa_model_predict_binomial <- mpa_model_predict_binomial %>%
    mutate(!!fishing_presence_predicted_col := factor(
      ifelse(predicted_probabilities >= optimal_cutoff, "Fishing", "No_fishing"),
      levels = c("No_fishing", "Fishing")
    ))
  
  # Combine observed and predicted fishing presence
  fishing_presence_data <- mpa_model %>%
    # Exclude rows where predictions were made
    filter(!id_iucn %in% mpa_model_predict_binomial$id_iucn) %>%
    # Use observed fishing presence for these cases
    mutate(!!fishing_presence_predicted_col := !!sym(fishing_presence_col)) %>%
    # Join with the predictions
    bind_rows(mpa_model_predict_binomial) %>%
    dplyr::select(id_iucn, !!sym(fishing_presence_col), !!fishing_presence_predicted_col)
  
  # Return the data with predicted fishing presence
  return(fishing_presence_data)
}