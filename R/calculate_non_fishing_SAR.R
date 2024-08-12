calculate_non_fishing_SAR <- function(){
  
  # #Add number of non fishing boats as covariates
  # Specify the base directory containing the year folders
  path <- "data/SAR_data/"
  #Load csv file names
  csv_names <- list.files(path = path,
                          pattern = "*.csv",
                          full.names = TRUE)

  # # Append all SAR data into dataframe
  SAR_non_fishing <- lapply(csv_names, read_csv) %>%
    bind_rows() %>%
    #Only keep non-fishing vessels with defined category
    mutate(matched_category = as.factor(matched_category)) %>%
    filter(!matched_category %in% c("unmatched","fishing","noisy_vessel","discrepancy")) %>%
    st_as_sf(coords = c("lon","lat"),crs = 4326) %>%
    #Join with mpa data
    st_join(mpa_wdpa, left = F) %>%
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    dplyr::mutate(temp_id = paste0("id_",1:nrow(SAR_non_fishing))) %>%
    dplyr::select(id_iucn, temp_id) 
  
  #Normalize detections
  SAR_non_fishing_normalized <- SAR_non_fishing %>%
    #Join with footprint to get number of images taken at each detection
    st_join(SAR_footprints_sf, join = st_intersects) %>%
    st_drop_geometry() %>%
    group_by(temp_id) %>%
    reframe(image_count = n()) %>%
    ungroup()
  
  # Non fishing SAR vessels per MPA
  mpa_SAR_non_fishing <- SAR_non_fishing_normalized %>%
    mutate(detection_normalized = 1/image_count) %>%
    left_join(SAR_non_fishing, by = "temp_id") %>%
    group_by(id_iucn) %>%
    reframe(n_non_fishing = sum(detection_normalized)) %>%
    ungroup()
  
  save(mpa_SAR_non_fishing, file = "output/mpa_SAR_non_fishing.Rdata")

  return(mpa_SAR_non_fishing)
  
  
}