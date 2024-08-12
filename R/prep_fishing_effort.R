prep_fishing_effort <- function(){
  
  path = (here::here("data/GFW_fishing_effort/"))
  setwd(path)
  
  # Function to load and ensure consistent data types
  load_and_cast <- function(file) {
    data <- get(load(file))
    
    # Ensure consistent data types across all relevant columns
    data <- data %>%
      mutate(
        Lat = as.numeric(Lat),  # Ensure 'Lat' is numeric
        Lon = as.numeric(Lon),  # Ensure 'Lon' is numeric (if needed)
        `Time Range` = as.character(`Time Range`),  # Ensure 'Time Range' is character
        `Vessel IDs` = as.character(`Vessel IDs`),  # Ensure 'Vessel IDs' is character
        `Apparent Fishing Hours` = as.numeric(`Apparent Fishing Hours`)  # Ensure 'Apparent Fishing Hours' is numeric
      )
    
    return(data)
  }
  
  # Load all .RData files and combine them into one dataframe
  fishing_effort_GFW <- list.files() %>%
    map_df(~ load_and_cast(.x))
  
  fishing_effort_GFW_clean <- fishing_effort_GFW %>%
    clean_names() %>%
    filter(time_range == 2023) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  setwd(here())
  
  #Join GFW fishing effort with MPAs
  mpa_wdpa_fishing <- mpa_wdpa %>%
    #Keep only MPAs from SAR dataset
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    dplyr::select(id_iucn) %>%
    #Join with AIS fishing effort
    st_join(fishing_effort_GFW_clean) %>%
    group_by(id_iucn) %>%
    reframe(mpa_fishing_GFW = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    mutate(mpa_fishing_GFW = ifelse(is.na(mpa_fishing_GFW), 0, mpa_fishing_GFW)) 
  
  save(mpa_wdpa_fishing, file = "output/mpa_wdpa_fishing.Rdata")
  
  return(mpa_wdpa_fishing)
  

  
  
}