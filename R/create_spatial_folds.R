create_spatial_folds <- function(MPA_covariates){
  
  # Prep data for model
  mpa_model <- MPA_covariates %>%
    st_drop_geometry %>%
    left_join(mpa_wdpa_fishing, by = "id_iucn") %>%
    #Join with MPAs for geometry
    left_join(mpa_wdpa %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    st_as_sf() %>%
    #Transform vars 
    #Transform vars 
    mutate(fishing_presence = ifelse(mpa_fishing_GFW == 0, "No_fishing", "Fishing"),
           fishing_presence = as.factor(fishing_presence),
           fishing = ifelse(is.na(fishing), 0, fishing),
           mpa_fishing_GFW_log = log(mpa_fishing_GFW),
           fishing_2022_log = log(fishing_2022 + 1), 
           length_matched = ifelse(is.na(length_matched),0,length_matched)) %>%
    #Select variables of interest
    dplyr::select(id_iucn, iucn_cat, fishing, unmatched_fishing, sum_all, fishing_presence, 
                  mpa_fishing_GFW,mpa_fishing_GFW_log,
                  fishing_2022,fishing_2022_log,
                  status_yr, iso3, area_correct, marine,mean_sst:travel_time, n_non_fishing,
                  ais_reception_positions_per_day_class_A,n_non_fishing, 
                  ais_reception_positions_per_day_class_B, length_matched, 
                  length_unmatched, length_all) %>%
    #Transform variables - as factor/rescale/transform sd to 0
    mutate(across(c(iucn_cat, iso3, marine, lme, ecoregion), as.factor),
           # across(c(area_correct, mean_sst, sd_sst, mean_chl, sd_chl, depth, n_non_fishing,
           #          mean_anom, sd_anom, seamount_distance, mean_wind_speed, sd_wind_speed,
           #          dist_to_port, dist_to_shore, abyssal, shelf, slope, sediment, 
           #          ais_reception_positions_per_day_class_A, ais_reception_positions_per_day_class_B,
           #          gdp, travel_time, length_matched, length_unmatched, length_all), log+1),
           sd_chl = ifelse(is.na(sd_chl), 0, sd_chl)) 
  
  # #Investigate spatial autocorelation
  # sac <- blockCV::cv_spatial_autocor(x = mpa_model,
  #                    column = "fishing_presence",
  #                    plot = T)
  # 
  #Size of blocks need to be 200 000 meters
  spatial_folds  <- blockCV::cv_spatial(mpa_model,
                                                column = "fishing_presence",
                                                size = 200000, # Define the block size based on your study's scale
                                                k = 10, # Number of folds
                                                selection = "random",
                                                iteration = 100) # Number of iterations
  
  #Plot
  # plot_cv_folds <- blockCV::cv_plot(cv = spatial_folds,
  # x = mpa_model)
  # ggsave(plot_cv_folds, file = "figures/plot_cv_folds.jpg")
  
  return(spatial_folds)
  
}