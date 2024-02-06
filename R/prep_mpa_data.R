prep_mpa_data <- function() {
  
  setwd(here())
  
  mpa_wdpa <- bind_rows(st_read(dsn = "maps/WDPA/WDPA_Nov2023_Public_shp_0",
                                layer = "WDPA_Nov2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Nov2023_Public_shp_1",
                                layer = "WDPA_Nov2023_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Nov2023_Public_shp_2",
                                layer = "WDPA_Nov2023_Public_shp-polygons",
                                quiet = TRUE)) %>%
    clean_names() %>%
    #Keep only marine and partially marines MPAs
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    #Only selecting MPAs created BEFORE 2017 
    filter(status_yr < 2017) %>%
    #No MPAs which are outside EEZs
    filter(iso3 != "ABNJ") %>%
    #No MPAs of IUCN CAT 3
    filter(iucn_cat != "III") %>%
    #Only with management plan
    filter(!status %in% c("Proposed","Established","Not Reported")) %>%
    #Selection 
    dplyr::select(id= wdpaid, name, desig_eng, iucn_cat, status_yr,status, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine) %>%
    dplyr::mutate(source = "wdpa",
                  marine = as.factor(marine),
                  id = as.factor(id)) %>%
    #%ake geometry valid
    st_make_valid() %>%
    #UNIQUE ID of ID + IUCN_CAT
    mutate(id_iucn = as.factor(paste0(id,"_",iucn_cat))) %>%
    distinct(id_iucn, .keep_all = T)
  
  #Create buffer 1km from shoreline
  ROI = ne_countries(returnclass = 'sf',scale="large") %>%
    st_combine() 
  
  buffer_in_km <- 1
  buffer_as_arc_degrees<- buffer_in_km/40075*360
  
  coastalWaters = ROI %>%
    st_buffer(buffer_as_arc_degrees)  %>% st_wrap_dateline() %>% st_make_valid()
  
  #Removing parts of MPA 1km within shoreline
  mpa_wdpa_nocoastline <- st_difference(mpa_wdpa,coastalWaters)
  
  #Only keeping MPAs with more than 1km^2 surface
  mpa_wdpa <- mpa_wdpa_nocoastline %>%
    #Calculating new area
    mutate(area_correct = set_units(st_area(mpa_wdpa_nocoastline),km^2),
           area_correct = as.numeric(area_correct)) %>%
    #Only areas higher than 1km
    filter(area_correct > 1)
  
  save(mpa_wdpa, file = "data/mpa_wdpa.Rdata")
  
  #MPA object not as sf to save space
  mpa_wdpa_no_sf <- mpa_wdpa %>% st_drop_geometry()
  save(mpa_wdpa_no_sf, file = "data/mpa_wdpa_no_sf.Rdata")

  
}