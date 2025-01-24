#' Normalize SAR Detections in EEZ
#'
#' This function normalizes SAR detections in unprotected Exclusive Economic Zones (EEZ) by 
#' calculating the number of overlapping satellite images for each detection and dividing the 
#' detection by the image count.
#'
#' @param SAR_outside_mpas An `sf` object representing SAR detections outside MPAs, with spatial 
#' coordinates (`lon`, `lat`).
#' @param SAR_footprints_sf An `sf` object representing satellite footprints covering the EEZ.
#'
#' @return An updated `sf` object of SAR detections with two additional attributes:
#' - `image_count_EEZ`: The number of satellite images covering each detection.
#' - `normalized_detection_EEZ`: The normalized detection value (1 divided by the image count).
#'
#' @details
#' The function performs the following steps:
#' 1. Creates unique IDs for each SAR detection.
#' 2. Joins SAR detections with satellite footprints to count overlapping images.
#' 3. Normalizes detections by dividing 1 by the image count.
#' 4. Adds the calculated attributes (`image_count_EEZ` and `normalized_detection_EEZ`) to the original dataset

normalize_detections_EEZ <- function(SAR_outside_mpas, SAR_footprints_sf){
  
  #Create unique id for each detection
  SAR_eez <- SAR_outside_mpas %>%
    dplyr::mutate(temp_id = row_number()) %>%
    dplyr::select(temp_id)
  
  #Join detections with spatial footprint
  detections_with_scenes_eez <- SAR_eez %>%
    st_join(SAR_footprints_sf)
  
  # Count how many images cover each detection
  detections_image_count_EEZ <- detections_with_scenes_eez %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    summarise(image_count_eez = n()) %>%
    ungroup()
  
  # Assuming each detection is unique (no duplicates in geometry)
  detections_normalized_eez <- SAR_eez %>%
    st_drop_geometry() %>%
    left_join(detections_image_count_EEZ, by = "temp_id") %>%
    mutate(normalized_detection_EEZ = 1 / image_count_eez)
  
  SAR_outside_mpas$image_count_EEZ <- detections_normalized_eez$image_count_eez
  SAR_outside_mpas$normalized_detection_EEZ <- detections_normalized_eez$normalized_detection_EEZ
  
  return(SAR_outside_mpas)
  
}