#' Calculate the Difference Between EEZ and MPAs
#'
#' This function calculates the unprotected areas within Exclusive Economic Zones (EEZ) by 
#' removing Marine Protected Areas (MPAs) and a 1 km coastal buffer. The resulting dataset 
#' includes EEZ regions that are outside both MPAs and the coastal buffer.
#'
#' @param eez An `sf` object representing the EEZ boundaries.
#' @param mpa_wdpa An `sf` object representing the Marine Protected Areas dataset.
#' @param SAR_footprints_sf An `sf` object representing the study area's SAR footprints, used 
#' to intersect EEZ with the study area.
#'
#' @return An `sf` object containing the unprotected EEZ areas, with additional attributes:
#' - `ISO_SOV1`: Sovereign country code.
#' - `eez_area`: Area of the unprotected EEZ in square kilometers.
#'
#' @details
#' The function performs the following steps:
#' 1. Intersects the EEZ boundaries with the study area defined by SAR footprints.
#' 2. Applies a 1 km buffer from the shoreline to exclude coastal regions.
#' 3. Removes all protected areas created before 2022 from the EEZ regions.
#' 4. Calculates the total area of unprotected EEZ for each country.
#'

eez_mpa_difference <- function(eez, mpa_wdpa, SAR_footprints_sf){
  
  #First, intersect EEZ with study area
  study_area <- SAR_footprints_sf %>% st_simplify(dTolerance = 0.1) %>% st_union()
  
  eez_study_area <- st_intersection(eez, study_area) %>%
    st_make_valid() 

  #Then, remove the 1km band from EEZ
  #Create buffer 1km from shoreline
  ROI = ne_countries(returnclass = 'sf',scale="large") %>%
    st_combine()
  
  buffer_in_km <- 1
  buffer_as_arc_degrees<- buffer_in_km/40075*360
  
  coastalWaters = ROI %>%
    st_buffer(buffer_as_arc_degrees)  %>% st_wrap_dateline() %>% st_make_valid()
  
  #Removing parts of EEZ 1km within shoreline
  eez_no_coastline <- st_difference(eez_study_area,coastalWaters)
  
  #Then, remove ALL protected areas created before 2022 from the EEZ 
  #This way only the unprotected part remains
  load("data/world_mpas_clean.Rdata")
  all_mpas = world_mpas_clean %>% st_union()
  save(all_mpas, file = "output/all_mpas.Rdata")
  
  #EEZ without MPAs
  eez_no_mpa <- st_difference(eez_no_coastline %>% st_transform(crs = st_crs(all_mpas)), all_mpas) %>%
    st_make_valid() 
  
  eez_no_mpa <- eez_no_mpa %>%
    mutate(eez_area = as.numeric(set_units(st_area(.),km^2))) %>%
    group_by(ISO_SOV1) %>%
    mutate(eez_area = sum(eez_area)) %>%
    ungroup()
  
  save(eez_no_mpa, file = "output/eez_no_mpa.Rdata")
  
  return(eez_no_mpa)

}