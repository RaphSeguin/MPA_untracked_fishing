#' Prepare Vessel Data for Modeling
#'
#' This function processes Marine Protected Area (MPA) and vessel-related data to create a dataset 
#' suitable for statistical modeling and spatial analysis.
#'
#' @param MPA_final_vars A dataframe containing processed fishing activity statistics for MPAs.
#' @param MPA_covariates A dataframe containing environmental and socioeconomic covariates for MPAs.
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return A dataframe (`mpa_vessel_model`) with vessel activity, environmental, and socioeconomic covariates, 
#' along with spatial coordinates (`X`, `Y`).
#'
#' @details
#' 1. Merges vessel activity data with MPA metadata.
#' 2. Joins environmental and socioeconomic covariates.
#' 3. Converts depth values to absolute depth (positive values).
#' 4. Transforms selected numerical variables using log transformation (`log10(x + 1)`).
#' 5. Filters out MPAs with IUCN category "III".
#' 6. Keeps only MPAs within IUCN categories "I" to "VI".
#' 7. Converts categorical variables (`iucn_cat`, `marine`, `parent_iso`) to factors.
#' 8. Computes centroid coordinates (`X`, `Y`) for each MPA.
#'
#' @examples
#' vessel_data <- prep_vessel_data(MPA_final_vars, MPA_covariates, mpa_wdpa)
#'


prep_vessel_data <- function(MPA_final_vars, MPA_covariates, mpa_wdpa){
  
  # Prep data for model
  mpa_vessel_model <- MPA_final_vars %>%
    dplyr::select(id_iucn, iucn_cat, fishing:SAR_presence,average_length) %>%
    st_drop_geometry() %>%
    left_join(mpa_wdpa %>% dplyr::select(c(id_iucn, marine, parent_iso)), by = "id_iucn") %>%
    left_join(MPA_covariates %>% dplyr::select(id_iucn:ais_reception_positions_per_day_class_B), 
              by = "id_iucn") %>%
    st_as_sf() %>%
    # Transform variables
    mutate(sd_chl = ifelse(is.na(sd_chl), 0, sd_chl)) %>%
    filter(iucn_cat != "III") %>%
    mutate(depth = as.numeric(ifelse(depth >= 0, 0, depth))) %>%
    mutate(depth = abs(depth)) %>%
    # Select variables of interest
    dplyr::select(id_iucn, iucn_cat, marine, area_correct, average_length,
                  travel_time, HDI, MarineEcosystemDependency, hf, gdp,
                  SAR_presence, sum_all, fishing, unmatched_fishing,
                  mean_chl, sd_chl, mean_sst, sd_sst,parent_iso, 
                  seamount_distance, depth, dist_to_shore, dist_to_port) %>%
    # Transform variables as factors and rescale others
    mutate(across(c(iucn_cat, marine,parent_iso), as.factor),
           across(c(mean_sst, sd_sst, mean_chl, sd_chl, depth, area_correct,gdp,
                    seamount_distance, dist_to_port, dist_to_shore, 
                    HDI, hf, travel_time, hf, MarineEcosystemDependency),
                  ~ log10(. + 1))) %>%
    #Only keep IUCN cat
    filter(iucn_cat %in% c("I","II","III","IV","V","VI")) 
    
  # Drop geometry and add coordinates
  mpa_vessel_model_coordinates <- mpa_vessel_model %>% st_centroid() %>% st_coordinates()
  
  mpa_vessel_model <- mpa_vessel_model %>%
    st_drop_geometry() %>%
    bind_cols(mpa_vessel_model_coordinates) %>%
    distinct(X, Y, .keep_all = T)
  
  return(mpa_vessel_model)
}
  
