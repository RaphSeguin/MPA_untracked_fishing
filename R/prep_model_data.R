#' Prepare MPA Data for Modeling
#'
#' This function processes Marine Protected Area (MPA) data by integrating fishing effort, 
#' environmental, and socioeconomic covariates, preparing it for statistical modeling.
#'
#' @param MPA_covariates A dataframe containing environmental and socioeconomic covariates for MPAs.
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return A processed dataframe (`mpa_model`) containing key covariates for modeling, 
#' including vessel activity, SAR presence, environmental factors, and socioeconomic indicators.
#'
#' @details
#' 1. **Merges fishing effort data** from `MPA_covariates` with `mpa_wdpa`.
#' 2. **Creates categorical variables** for fishing presence (`Fishing` vs. `No_fishing`).
#' 3. **Applies log transformation** to fishing effort and SAR detection variables.
#' 4. **Handles missing and infinite values**, replacing them with zero.
#' 5. **Standardizes and rescales numeric variables** (SST, depth, area, etc.).
#' 6. **Extracts spatial coordinates** (`X, Y`) from MPA centroids for spatial modeling.
#' 7. **Joins MPA locations** back into the dataset as an `sf` object.
#'
#' @examples
#' mpa_model_data <- prep_model_data(MPA_covariates, mpa_wdpa)
#'

prep_model_data <- function(MPA_covariates, mpa_wdpa){
  
  # Prep data for model
  mpa_model <- MPA_covariates %>%
    st_drop_geometry() %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    st_as_sf() %>%
    # Transform variables
    #Coding factor for fishing presence
    mutate(fishing_presence_2022 = as.factor(ifelse(AIS_fishing_2022 == 0, "No_fishing", "Fishing")),
           fishing_presence_2023 = as.factor(ifelse(AIS_fishing_2023 == 0, "No_fishing", "Fishing")),
           fishing_presence_2024 = as.factor(ifelse(AIS_fishing_2024 == 0, "No_fishing", "Fishing")),
           fishing_presence_2022 = relevel(fishing_presence_2022, ref = "No_fishing"),
           fishing_presence_2023 = relevel(fishing_presence_2023, ref = "No_fishing"),
           fishing_presence_2024 = relevel(fishing_presence_2024, ref = "No_fishing"),
           #number of vessels and log it
           unmatched_fishing_2022 = ifelse(is.na(unmatched_fishing_2022), 0, unmatched_fishing_2022),
           unmatched_fishing_2023 = ifelse(is.na(unmatched_fishing_2023), 0, unmatched_fishing_2023),
           unmatched_fishing_2024 = ifelse(is.na(unmatched_fishing_2024), 0, unmatched_fishing_2024),
           fishing_2022 = ifelse(is.na(fishing_2022), 0, fishing_2022),
           fishing_2022_log = log(fishing_2022), 
           sum_all_2022 = ifelse(is.na(sum_all_2022), 0, sum_all_2022),
           sum_all_2022_log = log(sum_all_2022),
           fishing_2023 = ifelse(is.na(fishing_2023), 0, fishing_2023),
           fishing_2023_log = log(fishing_2023), 
           sum_all_2023 = ifelse(is.na(sum_all_2023), 0, sum_all_2023),
           sum_all_2023_log = log(sum_all_2023),
           fishing_2024 = ifelse(is.na(fishing_2024), 0, fishing_2024),
           fishing_2024_log = log(fishing_2024), 
           sum_all_2024 = ifelse(is.na(sum_all_2024), 0, sum_all_2024),
           sum_all_2024_log = log(sum_all_2024),
           #Add SAR presence factor
           SAR_matched_presence_2022 = as.factor(ifelse(fishing_2022 == 0,"No_SAR","SAR")),
           SAR_matched_presence_2023 = as.factor(ifelse(fishing_2023 == 0,"No_SAR","SAR")),
           SAR_matched_presence_2024 = as.factor(ifelse(fishing_2024 == 0,"No_SAR","SAR")),
           SAR_all_presence_2022 =  as.factor(ifelse(sum_all_2022 == 0,"No_SAR","SAR")),
           SAR_all_presence_2023 =  as.factor(ifelse(sum_all_2023 == 0,"No_SAR","SAR")),
           SAR_all_presence_2024 =  as.factor(ifelse(sum_all_2024 == 0,"No_SAR","SAR")),
           #AIS fishing effort in hours log
           AIS_fishing_2021_log = log(AIS_fishing_2021),
           AIS_fishing_2022_log = log(AIS_fishing_2022),
           AIS_fishing_2023_log = log(AIS_fishing_2023),
           AIS_fishing_2024_log = log(AIS_fishing_2024),
           AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023 + AIS_fishing_2024,
           fishing_all = fishing_2022 + fishing_2023 + fishing_2024, 
           sd_chl = ifelse(is.na(sd_chl), 0, sd_chl)) %>%
    #Replace NaN with 0
    mutate(across(c(fishing_2022_log, fishing_2023_log, fishing_2024_log, sum_all_2022_log,
                    sum_all_2023_log, sum_all_2024_log, AIS_fishing_2021_log,
                    AIS_fishing_2022_log, AIS_fishing_2023_log, AIS_fishing_2024_log),
                  ~ ifelse(is.nan(.) | (is.infinite(.) & . < 0), 0, .))) %>%
    # Select variables of interest
    dplyr::select(id_iucn, parent_iso,fishing_2022, fishing_2022_log, sum_all_2022, sum_all_2022_log, fishing_presence_2022,
                  SAR_matched_presence_2022, SAR_matched_presence_2023, SAR_matched_presence_2024, SAR_all_presence_2022, SAR_all_presence_2023, SAR_all_presence_2024,
                  fishing_2023, fishing_2023_log, sum_all_2023, sum_all_2023_log, fishing_presence_2023,
                  fishing_2024, fishing_2024_log, sum_all_2024, sum_all_2024_log, fishing_presence_2024,
                  unmatched_fishing_2022, unmatched_fishing_2023, unmatched_fishing_2024, 
                  AIS_fishing_2021, AIS_fishing_2021_log, 
                  AIS_fishing_2022, AIS_fishing_2022_log,
                  AIS_fishing_2023, AIS_fishing_2023_log,
                  AIS_fishing_2024, AIS_fishing_2024_log,
                  AIS_fishing_all, fishing_all,iucn_cat,
                  mean_chl, sd_chl, mean_sst, sd_sst,area_correct,
                  seamount_distance, depth, dist_to_shore, travel_time, 
                  hf, HDI, MarineEcosystemDependency, gdp, 
                  ais_reception_positions_per_day_class_A, ais_reception_positions_per_day_class_B) %>%
    # Transform variables as factors and rescale others
    mutate(across(c(mean_sst, sd_sst, mean_chl, sd_chl, depth, 
                    seamount_distance,  travel_time, dist_to_shore, area_correct,
                    hf, gdp, 
                    ais_reception_positions_per_day_class_A, ais_reception_positions_per_day_class_B),
                  rescale),
           parent_iso = as.factor(parent_iso))

  # Drop geometry and add coordinates
  mpa_model_coordinates <- mpa_model %>% st_centroid() %>% st_coordinates()
  
  mpa_model <- mpa_model %>%
    st_drop_geometry() %>%
    bind_cols(mpa_model_coordinates) %>%
    distinct(X, Y, .keep_all = T) %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn) %>% st_centroid(), by = "id_iucn") %>%
    st_as_sf()
  
  return(mpa_model)
}
