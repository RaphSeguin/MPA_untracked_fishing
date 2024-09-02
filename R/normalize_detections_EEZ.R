normalize_detections_EEZ <- function(SAR_outside_mpas, SAR_footprints_sf){
  
  #Create unique id for each detection
  SAR_eez <- SAR_outside_mpas %>%
    dplyr::mutate(temp_id = paste0("id_",1:nrow(SAR_outside_mpas))) %>%
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