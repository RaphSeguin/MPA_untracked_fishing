#' Load and Cast Fishing Effort Data
#'
#' This function loads an `.RData` file and ensures that specific columns 
#' have consistent data types.
#'
#' @param file A string representing the path to the `.RData` file.
#'
#' @return A dataframe with the following standardized columns:
#' - `Lat`: Numeric latitude.
#' - `Lon`: Numeric longitude.
#' - `Time Range`: Character time range.
#' - `Vessel IDs`: Character vessel IDs.
#' - `Apparent Fishing Hours`: Numeric fishing effort.
#'
#' @details
#' Ensures that `Lat`, `Lon`, `Time Range`, `Vessel IDs`, and `Apparent Fishing Hours` 
#' have the correct data types.
#'
#' @examples
#' data <- load_and_cast("data/GFW_fishing_effort_grid/file1.RData")
#'
#'

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


#' Process Fishing Effort Data for a Given Year
#'
#' This function processes Global Fishing Watch (GFW) fishing effort data for a 
#' specified year and links it to Marine Protected Areas (MPAs).
#'
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#' @param current_year The year for which fishing effort data should be processed.
#'
#' @return A dataframe with the following columns:
#' - `id_iucn`: Unique MPA identifier.
#' - `AIS_fishing_<year>`: Total AIS-detected fishing hours within the MPA for the selected year.
#'
#' @details
#' 1. Loads all `.RData` files containing fishing effort data.
#' 2. Standardizes column names and extracts the year from the `Time Range`.
#' 3. Filters data for the selected year.
#' 4. Converts data to an `sf` object for spatial analysis.
#' 5. Joins fishing effort data with MPA polygons.
#' 6. Aggregates total fishing hours for each MPA.
#' 7. Ensures missing values are replaced with zero.
#'
#' @examples
#' MPA_fishing_2023 <- process_fishing_effort(mpa_wdpa, 2023)

# Function to process fishing effort data for a given year and rename the column
process_fishing_effort <- function(mpa_wdpa, current_year) {
  
  path <- here::here("data/GFW_fishing_effort_grid/")
  setwd(path)
  
  # Load all .RData files and combine them into one dataframe
  fishing_effort <- list.files() %>%
    map_df(~ load_and_cast(.x)) %>%
    mutate()
  
  # Clean and filter data for the selected year
  fishing_effort_clean <- fishing_effort %>%
    clean_names() %>%
    mutate(year = str_extract(time_range, "^\\d{4}")) %>%
    filter(year == current_year) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Join GFW fishing effort with MPAs
  MPA_wdpa_fishing <- mpa_wdpa %>%
    # Keep only MPAs from SAR dataset
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    dplyr::select(id_iucn) %>%
    # Join with AIS fishing effort
    st_join(fishing_effort_clean) %>%
    group_by(id_iucn) %>%
    reframe(!!paste0("AIS_fishing_", current_year) := sum(apparent_fishing_hours, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(!!paste0("AIS_fishing_", current_year) := ifelse(is.na(!!sym(paste0("AIS_fishing_", current_year))), 0, !!sym(paste0("AIS_fishing_", current_year))))
  
  setwd(here())
  
  return(MPA_wdpa_fishing)
}