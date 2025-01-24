#' Calculate Sea Surface Temperature (SST) for MPAs
#'
#' This function extracts and processes Sea Surface Temperature (SST) data from NetCDF files 
#' to compute mean and standard deviation values for each Marine Protected Area (MPA).
#'
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return A dataframe with two columns:
#' - `mean_sst`: Mean Sea Surface Temperature for each MPA.
#' - `sd_sst`: Standard deviation of Sea Surface Temperature for each MPA.
#'
#' @details
#' 1. Loads SST data from NetCDF files in `data/covariates/SST_covariates/`.
#' 2. Extracts longitude (`lon`), latitude (`lat`), and SST values.
#' 3. Computes the mean and standard deviation of SST at each location.
#' 4. Converts SST data into an `sf` object for spatial processing.
#' 5. Joins SST data with MPAs using the nearest centroid.
#'
#' @examples
#' sst_mpa <- calculate_SST(mpa_wdpa)
#'

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
  
}
