prep_model_data <- function(MPA_covariates, mpa_wdpa_fishing){
  
  # Prep data for model
  mpa_model <- MPA_covariates %>%
    st_drop_geometry %>%
    left_join(mpa_wdpa_fishing, by = "id_iucn") %>%
    #Join with MPAs for geometry
    left_join(mpa_wdpa %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    st_as_sf() %>%
    #Transform vars 
    mutate(fishing_presence = ifelse(mpa_fishing_GFW == 0, "No_fishing", "Fishing"),
           fishing_presence = as.factor(fishing_presence),
           fishing = ifelse(is.na(fishing), 0, fishing),
           mpa_fishing_GFW_log = log(mpa_fishing_GFW + 1),
           fishing_2022_log = log(fishing_2022 + 1),
           length_matched = ifelse(is.na(length_matched),0,length_matched)) %>%
    #Select variables of interest
    dplyr::select(id_iucn, iucn_cat, fishing, unmatched_fishing, sum_all, fishing_presence, 
                  mpa_fishing_GFW,mpa_fishing_GFW_log,fishing_2022, fishing_2022_log,
                  status_yr, iso3, area_correct, marine,mean_sst:travel_time, 
                  ais_reception_positions_per_day_class_A,
                  ais_reception_positions_per_day_class_B, length_matched, 
                  length_unmatched, length_all) %>%
    #Transform variables - as factor/rescale/transform sd to 0
    mutate(across(c(iucn_cat, iso3, marine, lme, ecoregion), as.factor),
           across(c(area_correct, mean_sst, sd_sst, mean_chl, sd_chl, depth, 
                    mean_anom, sd_anom, seamount_distance, mean_wind_speed, sd_wind_speed,
                    dist_to_port, dist_to_shore, abyssal, shelf, slope, sediment,
                    ais_reception_positions_per_day_class_A, ais_reception_positions_per_day_class_B,
                    gdp, travel_time, length_matched, length_unmatched, length_all), rescale),
           sd_chl = ifelse(is.na(sd_chl), 0, sd_chl)) 
  
  #Drop geometry
  mpa_model_coordinates <- mpa_model %>% st_centroid() %>% st_coordinates()
  
  mpa_model <- mpa_model %>% st_drop_geometry %>% bind_cols(mpa_model_coordinates) %>%
    mutate(X = rescale(X),
           Y = rescale(Y))
  
  return(mpa_model)
  
}