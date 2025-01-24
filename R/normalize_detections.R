normalize_detections <- function(SAR_mpa, SAR_footprints_sf){  
  
  #Convert to sf 
  SAR_mpa_sf <- SAR_mpa %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    dplyr::mutate(temp_id = paste0("id_",1:nrow(SAR_mpa))) %>%
    dplyr::select(year, temp_id)
  
  #Join detections with spatial footprint
  detections_with_scenes_2022 <- SAR_mpa_sf %>%
    filter(year == 2022) %>%
    st_join(SAR_footprints_sf %>% filter(str_detect(date, "2022")))
  
  detections_with_scenes_2023 <- SAR_mpa_sf %>%
    filter(year == 2023) %>%
    st_join(SAR_footprints_sf %>% filter(str_detect(date, "2023")))
  
  detections_with_scenes_2024 <- SAR_mpa_sf %>%
    filter(year == 2024) %>%
    st_join(SAR_footprints_sf %>% filter(str_detect(date, "2024")))
  
  # Count how many images cover each detection
  detections_image_count_2022 <- detections_with_scenes_2022 %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    summarise(image_count_2022 = n()) %>%
    ungroup()
  
  detections_image_count_2023 <- detections_with_scenes_2023 %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    summarise(image_count_2023 = n()) %>%
    ungroup()
  
  detections_image_count_2024 <- detections_with_scenes_2024 %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    summarise(image_count_2024 = n()) %>%
    ungroup()
  
  # Assuming each detection is unique (no duplicates in geometry)
  detections_normalized_2022 <- SAR_mpa_sf %>%
    st_drop_geometry() %>%
    left_join(detections_image_count_2022, by = "temp_id") %>%
    mutate(normalized_detection_2022 = 1 / image_count_2022)
  
  detections_normalized_2023 <- SAR_mpa_sf %>%
    st_drop_geometry() %>%
    left_join(detections_image_count_2023, by = "temp_id") %>%
    mutate(normalized_detection_2023 = 1 / image_count_2023)
  
  detections_normalized_2024 <- SAR_mpa_sf %>%
    st_drop_geometry() %>%
    left_join(detections_image_count_2024, by = "temp_id") %>%
    mutate(normalized_detection_2024 = 1 / image_count_2024)
  
  SAR_mpa$normalized_detection_2022 <- detections_normalized_2022$normalized_detection_2022
  SAR_mpa$normalized_detection_2023 <- detections_normalized_2023$normalized_detection_2023
  SAR_mpa$normalized_detection_2024 <- detections_normalized_2024$normalized_detection_2024
  
  SAR_mpa$image_count_2022 <- detections_normalized_2022$image_count_2022
  SAR_mpa$image_count_2023 <- detections_normalized_2023$image_count_2023
  SAR_mpa$image_count_2024 <- detections_normalized_2024$image_count_2024
  
  return(SAR_mpa)
  
}
