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
    left_join(detections_image_count, by = "temp_id") %>%
    mutate(normalized_detection = 1 / image_count)
  
  SAR_mpa$image_count <- detections_normalized$image_count
  SAR_mpa$normalized_detection <- detections_normalized$normalized_detection
  
  return(SAR_mpa)
  
}