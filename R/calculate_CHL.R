calculate_CHL <- function(mpa_wdpa){
  
  # Example for loading NetCDF data
  nc_files <- list.files(path = "data/covariates/CHL_covariates", pattern = "\\.nc$", full.names = TRUE)
  
  # Load and process each NetCDF file
  chl_data <- map_dfr(nc_files, function(file) {
    tidync(file) %>%
      hyper_tibble() %>%
      dplyr::select(lon, lat, chlor_a)
  })
  
  # Calculate mean and standard deviation of SST
  chl_summary <- chl_data %>%
    group_by(lon, lat) %>%
    summarise(
      mean_chl = mean(chlor_a, na.rm = TRUE),
      sd_chl = sd(chlor_a, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # # Convert sst_summary to an sf object
  chl_sf <- st_as_sf(chl_summary, coords = c("lon", "lat"), crs = 4326)
  
  #Calculacate centroids of mpa
  mpa_centroids <- st_centroid(mpa_wdpa)
  
  # # Spatial join of SST points with MPA polygons
  chl_mpa <- st_join(mpa_centroids, chl_sf, join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(mean_chl, sd_chl)
  
  return(chl_mpa)
  
}