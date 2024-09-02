prep_vessel_data <- function(MPA_final_vars, MPA_covariates, mpa_wdpa){
  
  # Prep data for model
  mpa_vessel_model <- MPA_final_vars %>%
    dplyr::select(id_iucn, fishing:SAR_presence,average_length) %>%
    st_drop_geometry() %>%
    left_join(mpa_wdpa %>% dplyr::select(c(id_iucn, iucn_cat, marine, parent_iso)), by = "id_iucn") %>%
    left_join(MPA_covariates %>% dplyr::select(id_iucn:dist_to_shore,
                                               ais_reception_positions_per_day_class_A:hf), 
              by = "id_iucn") %>%
    st_as_sf() %>%
    # Transform variables
    mutate(sd_chl = ifelse(is.na(sd_chl), 0, sd_chl)) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I",iucn_cat)) %>%
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
  
