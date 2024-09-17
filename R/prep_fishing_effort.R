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

# Function to process fishing effort data for a given year and rename the column
process_fishing_effort <- function(mpa_wdpa, year) {
  
  path <- here::here("data/GFW_fishing_effort/")
  setwd(path)
  
  # Load all .RData files and combine them into one dataframe
  fishing_effort <- list.files() %>%
    map_df(~ load_and_cast(.x))
  
  # Clean and filter data for the selected year
  fishing_effort_clean <- fishing_effort %>%
    clean_names() %>%
    filter(time_range == year) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Join GFW fishing effort with MPAs
  MPA_wdpa_fishing <- mpa_wdpa %>%
    # Keep only MPAs from SAR dataset
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    dplyr::select(id_iucn) %>%
    # Join with AIS fishing effort
    st_join(fishing_effort_clean) %>%
    group_by(id_iucn) %>%
    reframe(!!paste0("AIS_fishing_", year) := sum(apparent_fishing_hours, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("AIS_fishing_", year) := ifelse(is.na(!!sym(paste0("AIS_fishing_", year))), 0, !!sym(paste0("AIS_fishing_", year))))
  
  setwd(here())
  
  return(MPA_wdpa_fishing)
}