eez_mpa_difference <- function(eez, mpa_wdpa){
  
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
  
  #Removing parts of MPA 1km within shoreline
  eez_no_coastline <- st_difference(eez_study_area,coastalWaters)
  
  #Then, remove ALL protected areas created before 2017 from the EEZ 
  #This way only the unprotected part remains
  #Mpa clean
  mpa_wdpa_all <- bind_rows(st_read(dsn = "maps/WDPA/WDPA_Feb2024_Public_shp_0",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Feb2024_Public_shp_1",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Feb2024_Public_shp_2",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE)) %>%
    clean_names() %>%
    #Keep only marine and partially marines MPAs
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    #Only selecting MPAs created BEFORE 2017 
    filter(status_yr < 2022) %>%
    filter(!status %in% c("Proposed","Established","Not Reported")) %>%
    st_make_valid()

  #Union of all MPAs
  all_mpas = mpa_wdpa_all %>% st_union()
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