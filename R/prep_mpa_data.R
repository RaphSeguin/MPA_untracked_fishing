prep_mpa_data <- function() {
  
  setwd(here())
  
  mpa_wdpa <- bind_rows(st_read(dsn = "maps/WDPA/WDPA_Feb2024_Public_shp_0",
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
    #No MPAs which are outside EEZs
    filter(iso3 != "ABNJ") %>%
    #Only with management plan
    #Selection 
    dplyr::select(id=wdpaid, name, desig_eng, iucn_cat, status_yr,status, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine) %>%
    #%ake geometry valid
    st_make_valid() %>%
    #UNIQUE ID of ID + IUCN_CAT
    mutate(id_iucn = as.factor((paste0(id,"_",iucn_cat))))
  
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
  
  #Now keeping only MPAs which fall withing range of study area
  #downloaded from GFW
  load("data/SAR_footprints.Rdata")
  
  # Step 1: Convert to sf and union the geometries
  study_area <- SAR_footprints %>%
    st_as_sf(wkt = "footprint_wkt", crs = 4326) 
  
  st_write(study_area, dsn = "data/study_area.shp")
  
  study_area <- st_read("data/study_area.shp")
  
  study_area_clean <- study_area %>%
    st_simplify(dTolerance = 0.1) %>%
    st_buffer(0.1) %>%
    st_union() 
  
  st_write(study_area_clean, dsn = "data/study_area_clean.shp")
  
  study_area_clean <- st_read("data/study_area_clean.shp")

  mpa_wdpa_nocoastline_clean <- mpa_wdpa_nocoastline %>%
    #remove duplicated values of id
    group_by(id_iucn) %>%
    filter(n() == 1) %>%
    ungroup()
  
  mpa_wdpa_study_area <- st_intersection(mpa_wdpa_nocoastline_clean, study_area_clean) %>%
    st_make_valid() 
  
  mpa_wdpa <- mpa_wdpa_study_area %>%
    #Only keeping MPAs with more than 1km^2 surface
    #Calculating new area
    mutate(area_correct = set_units(st_area(mpa_wdpa_study_area),km^2),
           area_correct = as.numeric(area_correct)) %>%
    filter(area_correct > 1) %>%
    st_make_valid()  %>%
    #remove duplicated values of id
    group_by(id_iucn) %>%
    filter(n() == 1) %>%
    ungroup()
  
  #remove duplicate geometries (MPAs that have exactly the same geometry but maybe have diffrenet management regimes)
  # mpa_centroids <- st_centroid(mpa_wdpa)
  # 
  # mpa_wdpa_no_duplicates <- mpa_wdpa[!duplicated(mpa_centroids$geometry), ]
  
  save(mpa_wdpa, file = "data/mpa_wdpa.Rdata")
  
  #MPA object not as sf to save space
  mpa_wdpa_no_sf <- mpa_wdpa %>% st_drop_geometry()
  save(mpa_wdpa_no_sf, file = "data/mpa_wdpa_no_sf.Rdata")

}