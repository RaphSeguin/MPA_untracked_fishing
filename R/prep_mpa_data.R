#' Prepare and Clean Marine Protected Areas (MPA) Data
#'
#' This function processes and cleans data for Marine Protected Areas (MPAs) obtained from 
#' the World Database on Protected Areas (WDPA). It filters marine-only MPAs, validates 
#' geometries, removes duplicates, and ensures compatibility with a defined study area.
#'
#' @param None The function does not take any arguments.
#'
#' @return
#' Saves two cleaned datasets to the `data/` directory:
#' - `mpa_wdpa.Rdata`: The cleaned and validated MPA dataset as an `sf` object.
#' - `mpa_wdpa_no_sf.Rdata`: A non-spatial version of the dataset without geometries.
#'
#' @details
#' The function handles several preprocessing tasks, such as:
#' - Filtering MPAs for marine regions created before 2022.
#' - Simplifying and validating geometries for spatial operations.
#' - Removing areas overlapping with coastal buffers (1 km from the shoreline).
#' - Removing duplicate MPAs, prioritizing stricter IUCN categories or larger areas.
#' - Ensuring all MPAs intersect with the SAR study area
#'

prep_mpa_data <- function() {

  #Download worldwide MPAs from WDPA
  world_mpas <- wdpa_fetch("global", wait = TRUE,
                           download_dir = rappdirs::user_data_dir("wdpar"))
  
  
  # Cleaning and preprocessing the MPAs database
  world_mpas_clean <- world_mpas %>%
    # Filter for marine MPAs only and those created before 2022
    dplyr::filter(MARINE %in% c(1, 2), STATUS_YR < 2022, ISO3 != "ABNJ") %>%
    clean_names() %>%
    # Filter relevant statuses and remove UNESCO biosphere reserves
    dplyr::filter(status %in% c("Designated", "Inscribed", "Established"), desig_eng != "UNESCO-MAB Biosphere Reserve") %>%
    # Rename and add geometry type, keeping only polygons
    dplyr::rename(parent_iso = parent_iso3) %>%
    # Set factor levels for IUCN categories and trim management plan field
    dplyr::mutate(
      iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned")),
      mang_plan = trimws(mang_plan)
    )
  
  save(world_mpas_clean, file = "data/world_mpas_clean.Rdata")
  
  #--------
  
  mpa_wdpa <- world_mpas_clean %>% st_transform(crs = 4326)
  
  #Create buffer 1km from shoreline
  ROI = ne_countries(returnclass = 'sf',scale="large") %>%
    st_combine()
  
  buffer_in_km <- 1
  buffer_as_arc_degrees<- buffer_in_km/40075*360
  
  coastalWaters = ROI %>%
    st_buffer(buffer_as_arc_degrees)  %>% st_wrap_dateline() %>% st_make_valid()
  
  #Removing parts of MPA 1km within shoreline
  mpa_wdpa_nocoastline <- st_difference(mpa_wdpa,coastalWaters)
  
  save(mpa_wdpa_nocoastline, file = "data/mpa_wdpa_nocoastline.Rdata")
  
  load("data/mpa_wdpa_nocoastline.Rdata")
  
  #Now keeping only MPAs which fall withing range of study area
  # #downloaded from GFW
  # load("data/SAR_footprints.Rdata")
  # 
  # # Step 1: Convert to sf and union the geometries
  # study_area <- SAR_footprints %>%
  #   st_as_sf(wkt = "footprint_wkt", crs = 4326) 
  # 
  # st_write(study_area, dsn = "data/study_area.shp")
  # 
  # study_area <- st_read("data/study_area.shp")
  # 
  # study_area_clean <- study_area %>%
  #   st_simplify(dTolerance = 0.1) %>%
  #   st_buffer(0.1) %>%
  #   st_union() 
  # 
  # st_write(study_area_clean, dsn = "data/study_area_clean.shp")
  
  study_area_clean <- st_read("data/study_area_clean.shp")

  mpa_wdpa_study_area <- st_intersection(mpa_wdpa_nocoastline, study_area_clean) %>%
    st_make_valid() 
  
  # truc <- mpa_wdpa_study_area %>% filter(parent_iso == "AUS")
  # plot <- ggplot() + geom_sf(data = truc)
  # ggsave(plot, file = "plot.jpg")
  #  
  mpa_wdpa_online <- mpa_wdpa_study_area %>%
    mutate(id_iucn = paste0(wdpaid, iucn_cat)) %>%
    #Only keeping MPAs with more than 1km^2 surface
    #Calculating new area
    mutate(area_correct = set_units(st_area(.),km^2),
           area_correct = as.numeric(area_correct)) %>%
    filter(area_correct > 1) %>%
    st_make_valid()  %>%
    #remove duplicated values of id
    distinct(id_iucn, .keep_all = TRUE)
  
  #Clean database
  mpa_wdpa <- mpa_wdpa_online %>%
    # Remove duplicate geometries by selecting the strictest IUCN category
    group_by(geometry) %>% arrange(iucn_cat) %>% slice(1) %>% ungroup() %>%
    st_transform(crs = 4326) %>%
    st_make_valid() 

  save(mpa_wdpa, file = "data/mpa_wdpa.Rdata")
  
  #MPA object not as sf to save space
  mpa_wdpa_no_sf <- mpa_wdpa %>% st_drop_geometry()
  save(mpa_wdpa_no_sf, file = "data/mpa_wdpa_no_sf.Rdata")

}