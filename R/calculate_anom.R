calculate_anom <- function(mpa_wdpa){
  
  # Example for loading NetCDF data
  nc_files <- list.files(path = "data/covariates/SST_anomalies/", pattern = "\\.nc$", full.names = TRUE)
  
  # Load and process each NetCDF file
  anom_data <- map_dfr(nc_files, function(file) {
    tidync(file) %>%
      hyper_tibble(force = T) %>%
      dplyr::select(lon, lat, anom)
  })
  
  # Calculate mean and standard deviation of anom
  anom_summary <- anom_data %>%
    group_by(lon, lat) %>%
    summarise(
      mean_anom = mean(anom, na.rm = TRUE),
      sd_anom = sd(anom, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # # Convert anom_summary to an sf object
  anom_sf <- st_as_sf(anom_summary, coords = c("lon", "lat"), crs = 4326)
  
  #Calculacate centroids of mpa
  mpa_centroids <- st_centroid(mpa_wdpa)
  
  # # Spatial join of SST points with MPA polygons
  anom_mpa <- st_join(mpa_centroids, anom_sf, join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(mean_anom, sd_anom)
  
  return(anom_mpa)
  
}