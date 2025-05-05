#' Normalize SAR Detections
#'
#' This function normalizes SAR detections in MPAs by accounting for the number 
#' of satellite images covering each detection.
#'
#' @param SAR_mpa A dataframe containing SAR detections within MPAs.
#' @param SAR_footprints_sf An `sf` object representing the spatial footprint of SAR images.
#'
#' @return The input `SAR_mpa` dataframe with two new columns:
#' - `image_count`: Number of satellite images covering each detection.
#' - `normalized_detection`: Detection value normalized by the image count.
#'
#' @details
#' 1. Converts `SAR_mpa` to an `sf` object.
#' 2. Joins detections with SAR image footprints.
#' 3. Counts the number of images covering each detection.
#' 4. Normalizes detections by dividing by the image count.
#' 5. Merges results back into `SAR_mpa`.
#'
#' @examples
#' SAR_mpa_normalized <- normalize_detections_all(SAR_mpa, SAR_footprints_sf)
#'
#' @export


normalize_detections_all <- function(SAR_mpa, SAR_footprints_sf){
  
  #Convert to sf 
  SAR_mpa_sf <- SAR_mpa %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    dplyr::mutate(temp_id = paste0("id_",1:nrow(SAR_mpa))) %>%
    dplyr::select(temp_id)
  
  #Join detections with spatial footprint
  detections_with_scenes <- SAR_mpa_sf %>%
    st_join(SAR_footprints_sf)
  
  # Count how many images cover each detection
  detections_image_count <- detections_with_scenes %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    summarise(image_count = n()) %>%
    ungroup()
  
  # Assuming each detection is unique (no duplicates in geometry)
  detections_normalized <- SAR_mpa_sf %>%
    st_drop_geometry() %>%
    left_join(detections_image_count, by = "temp_id") 
  
  SAR_mpa$image_count <- detections_normalized$image_count
  
  return(SAR_mpa)
  
}