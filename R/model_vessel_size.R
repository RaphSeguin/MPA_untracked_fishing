#' Model Vessel Size in MPAs
#'
#' This function fits a **Gaussian spatial mixed-effects model** to predict 
#' the **average vessel size (length)** within Marine Protected Areas (MPAs).
#'
#' @param mpa_vessel_model A dataframe containing vessel activity, environmental, and socioeconomic covariates for MPAs.
#'
#' @return A fitted model (`mod_spamm_size`) predicting vessel size, with outputs saved as:
#' - `output/mod_spamm_size.Rdata`: The fitted model object.
#' - `figures/supp/mod_spamm_size.csv`: Model coefficients summary.
#'
#' @details
#' 1. **Filters MPAs with vessel presence** (`average_length > 0`).
#' 2. **Removes duplicate geometries** to avoid redundancy.
#' 3. **Filters MPAs within IUCN categories "I" to "VI"**.
#' 4. **Removes missing values** (`na.omit()`).
#' 5. **Applies a log transformation** to `average_length` (`log(average_length + 1)`).
#' 6. **Fits a Gaussian GLMM** using `spaMM::fitme()`:
#'    - Predictors: MPA size, productivity, SST, GDP, HDI, travel time, human footprint, depth, distance to shore.
#'    - Random effect: `parent_iso` (country-level variation).
#'    - Spatial structure: `Matern(1 | X + Y)`.
#' 7. **Processes model output**:
#'    - Extracts and renames coefficients for interpretability.
#'    - Rounds numerical values and formats p-values.
#'    - Saves results to a CSV file.
#'
#' @examples
#' vessel_size_model <- model_vessel_size(mpa_vessel_model)
#'

model_vessel_size <- function(mpa_vessel_model){
  
  mpa_vessel_model_size <- mpa_vessel_model %>%
    #Only keep MPAs with fishing vessels inside
    filter(average_length > 0) %>%
    #Distinct geometries
    distinct(X, Y, .keep_all = T) %>%
    #Only keep IUCN cat
    filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
    na.omit() %>%
    mutate(average_length = log(average_length +1))
  
  hist(mpa_vessel_model_size$average_length)
  
  mpa_vessel_model_size$iucn_cat <- relevel(mpa_vessel_model_size$iucn_cat, ref = "VI")
  
  avail_thr <- parallel::detectCores(logical=FALSE) - 1L
  
  #Problem is 754`
  mod_spamm_size <- fitme(average_length ~  iucn_cat + marine + area_correct + 
                            mean_chl + sd_chl + mean_sst + sd_sst + gdp + HDI + hf + MarineEcosystemDependency + 
                            depth + dist_to_shore + travel_time + (1|parent_iso) + Matern(1 | X + Y),
                     data = mpa_vessel_model_size, 
                     family = gaussian(), 
                     control.HLfit = list(NbThreads = 6))
  
  save(mod_spamm_size, file = "output/mod_spamm_size.Rdata")
  
  mod_spamm_size_output <- summary(mod_spamm_size, details = list(p_value = TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = case_when(
      Variable == "HDI" ~ "Human development index",
      Variable == "iucn_catI" ~ "IUCN category I",
      Variable == "area_correct" ~ "MPA size", 
      Variable == "depth" ~ "Depth",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      Variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      Variable == "conflicts" ~ "Conflicts",
      Variable == "hf" ~ "Human footprint",
      Variable == "Chla" ~ "Primary productivity",
      Variable == "gis_m_area" ~ "MPA size",
      Variable == "Bathymetry" ~ "Depth",
      Variable == "SST" ~ "Sea surface temperature",
      Variable == "DistToCoast" ~ "Distance to coast",
      Variable == "salinity" ~ "Salinity",
      Variable == "gdp" ~ "Gross domestic product",
      Variable == "travel_time" ~ "Travel time to the nearest city",
      Variable == "marine2" ~ "Fully marine or both marine and terrestrial MPA",
      Variable == "iucn_catII" ~ "IUCN category II",
      Variable == "iucn_catIV" ~ "IUCN category IV",
      Variable == "iucn_catV" ~ "IUCN category V",
      Variable == "iucn_catVI" ~ "IUCN category VI",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001", p_value)) 
  
  write.csv(mod_spamm_size_output, "figures/supp/mod_spamm_size.csv")
  
  return(mod_spamm_size)
  
}