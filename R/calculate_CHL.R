#' Calculate Chlorophyll-a (CHL) for MPAs
#'
#' This function extracts and processes Chlorophyll-a (CHL) data from NetCDF files 
#' to calculate mean and standard deviation values for each Marine Protected Area (MPA).
#'
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return A dataframe with two columns:
#' - `mean_chl`: Mean Chlorophyll-a concentration for each MPA.
#' - `sd_chl`: Standard deviation of Chlorophyll-a concentration for each MPA.
#'
#' @details
#' 1. Loads CHL data from NetCDF files in `data/covariates/CHL_covariates/`.
#' 2. Extracts longitude (`lon`), latitude (`lat`), and CHL concentration (`chlor_a`).
#' 3. Computes the mean and standard deviation of CHL at each location.
#' 4. Converts CHL data into an `sf` object for spatial processing.
#' 5. Joins CHL data with MPAs using the nearest centroid.
#'
#' @examples
#' chl_mpa <- calculate_CHL(mpa_wdpa)
#'

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