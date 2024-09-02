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
  score <- predict(mod_regression_final, newdata = mpa_model_predict_regression, re.form=~0, allow.new.levels=T)
  
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
