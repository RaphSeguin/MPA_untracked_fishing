calculate_SST <- function(mpa_wdpa){
  
  # Example for loading NetCDF data
  nc_files <- list.files(path = "data/covariates/SST_covariates", pattern = "\\.nc$", full.names = TRUE)
  
  # Load and process each NetCDF file
  sst_data <- map_dfr(nc_files, function(file) {
    tidync(file) %>%
      hyper_tibble() %>%
      dplyr::select(lon, lat, sst)
  })
  
  # Calculate mean and standard deviation of SST
  sst_summary <- sst_data %>%
    group_by(lon, lat) %>%
    summarise(
      mean_sst = mean(sst, na.rm = TRUE),
      sd_sst = sd(sst, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # # Convert sst_summary to an sf object
  sst_sf <- st_as_sf(sst_summary, coords = c("lon", "lat"), crs = 4326)
  
  #Calculacate centroids of mpa
  mpa_centroids <- st_centroid(mpa_wdpa)
  
  # # Spatial join of SST points with MPA polygons
  sst_mpa <- st_join(mpa_centroids, sst_sf, join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(mean_sst, sd_sst)
  
  return(sst_mpa)
  
  # # Create raster from mean SST
  # sst_mean_raster <- rotate(rasterFromXYZ(sst_summary %>% dplyr::select(lon, lat, mean_sst)))
  # # Create raster from SD SST
  # sst_sd_raster <- rotate(rasterFromXYZ(sst_summary %>% dplyr::select(lon, lat, sd_sst)))
  # 
  # # Assign CRS (assuming WGS84)
  # crs(sst_mean_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  # crs(sst_sd_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  # 
  # # Define a function that replaces NA with the nearest non-NA value
  # fill_na_nearest <- function(x) {
  #   if (is.na(x[5])) {  # Assuming a 3x3 window, the center value is at index 5
  #     return(na.omit(x)[1])  # Replace with the first non-NA value
  #   } else {
  #     return(x[5])  # Keep the original value if it's not NA
  #   }
  # }
  # 
  # # Apply the focal function to fill NA values with the nearest non-NA value
  # sst_mean_raster_filled <- focal(sst_mean_raster, w = matrix(1, 3, 3), fun = fill_na_nearest, pad = TRUE, na.rm = FALSE)
  # 
  # # Extract mean SST within each MPA
  # mean_sst_in_mpa <-  exact_extract(sst_mean_raster_filled, mpas, 'mean', progress = TRUE)
  # 
  # # Extract SD SST within each MPA
  # sd_sst_in_mpa <- exact_extract(sst_sd_raster, mpas, 'mean', progress = TRUE)
  # 
  
}
