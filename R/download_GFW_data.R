#' Download GFW Fishing Effort Data
#'
#' This function downloads Global Fishing Watch (GFW) fishing effort data 
#' for each country's Marine Protected Areas (MPAs) over a specified year.
#'
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#' @param years A vector of years for which to download data (e.g., `c(2021, 2022, 2023, 2024)`).
#'
#' @return Saves GFW fishing effort data for each country-year combination as `.Rdata` files.
#'
#' @details
#' - MPAs are grouped by country to optimize downloads.
#' - A `tryCatch` mechanism ensures errors do not halt execution.
#' - Failed downloads are logged for troubleshooting.
#' - A delay (`Sys.sleep`) prevents API rate-limiting.
#'
#' @examples
#' download_GFW_data(mpa_wdpa, years = c(2021, 2022, 2023, 2024))
#'
#' @export

download_GFW_data <- function(mpa_wdpa, years) {
  
  key <- gfw_auth()
  
  # Group MPA geometries by country for efficient downloading
  mpa_wdpa_union_country <- mpa_wdpa %>%
    group_by(iso3) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_make_valid()
  
  mpa_geometry <- mpa_wdpa_union_country$geometry
  
  # Function to download GFW fishing effort for a given year
  download_for_year <- function(year) {
    
    failed_iterations <- c()  # Track failed downloads
    
    fishing_effort <- lapply(seq_along(mpa_geometry), function(i) {
      
      Sys.sleep(15)  # Prevent rate-limiting
      
      print(paste0("Downloading Year: ", year, " - Country Index: ", i))
      
      temp_geometry <- st_as_sf(mpa_geometry[i])
      
      # Attempt to download data, retrying failed cases
      fishing_effort_mpa <- tryCatch({
        get_raster(
          spatial_resolution = 'HIGH',
          temporal_resolution = 'YEARLY',
          group_by = 'FLAG',
          start_date = paste0(year, "-01-01"),
          end_date = paste0(year, "-12-31"),
          region = temp_geometry,
          region_source = 'USER_SHAPEFILE',
          key = key
        )
      }, error = function(e) {
        failed_iterations <<- c(failed_iterations, i)
        return(NULL)
      })
      
      # Save only if download was successful
      if (!is.null(fishing_effort_mpa)) {
        save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/", year, "_fishing_effort_mpa_", i, ".Rdata"))
      }
    })
    
    # Log failed downloads
    if (length(failed_iterations) > 0) {
      message(paste("Failed downloads for year", year, "at indices:", paste(failed_iterations, collapse = ", ")))
    }
  }
  
  # Run download for all specified years
  lapply(years, download_for_year)
}
