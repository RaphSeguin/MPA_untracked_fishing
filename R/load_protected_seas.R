load_protected_seas <- function(){
  
  setwd(here())
  
  #Load attribute data
  protected_seas_data <- read.csv("data/Protected_Seas/ProtectedSeas_Navigator_Attributes_20240531.csv")
  
  mpa_protected_seas <- st_read("data/Protected_Seas/ProtectedSeas_Navigator_20240531_shp/ProtectedSeas_Navigator_20240531_shp.shp") %>%
    clean_names() %>% 
    dplyr::select(-c(site_name)) %>%
    left_join(protected_seas_data, by = "site_id")

  #Clean
  mpa_protected_seas_clean <- mpa_protected_seas %>%
    #Remove inshore only
    # filter(inshore_only != 3) %>%
    #Remove created after 2022
    # filter(year_est < 2022) %>%
    filter(!is.na(removal_of_marine_life_is_prohibited)) %>%
    filter(category_name == "IUCN MPA")
    
  #Create buffer 1km from shoreline
  ROI = ne_countries(returnclass = 'sf',scale="large") %>%
    st_combine()
  
  buffer_in_km <- 1
  buffer_as_arc_degrees<- buffer_in_km/40075*360
  
  coastalWaters = ROI %>%
    st_buffer(buffer_as_arc_degrees)  %>% st_wrap_dateline() %>% st_make_valid()
  
  #Removing parts of MPA 1km within shoreline
  mpa_protected_seas_nocoastline <- st_difference(mpa_protected_seas_clean,coastalWaters)
  
  save(mpa_protected_seas_nocoastline, file = "data/mpa_protected_seas_nocoastline.Rdata")
  
  #Overlap with study area
  study_area_clean <- st_read("data/study_area_clean.shp")
  
  mpa_protected_seas_study_area <- st_intersection(mpa_protected_seas_nocoastline, study_area_clean) %>%
    st_make_valid() 
  
  #Get date
  date <- mpa_wdpa %>% st_drop_geometry() %>% dplyr::select(wdpa_id = id, status_yr) %>% mutate(wdpa_id = as.character(wdpa_id))
  
  mpa_protected_seas <- mpa_protected_seas_study_area %>%
    #Only keeping MPAs with more than 1km^2 surface
    #Calculating new area
    mutate(area_correct = set_units(st_area(mpa_protected_seas_study_area),km^2),
           area_correct = as.numeric(area_correct)) %>%
    filter(area_correct > 1) %>%
    st_make_valid()  %>%
    #remove duplicated values of id
    group_by(site_id) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    left_join(date, by = "wdpa_id")
  
  #Save  
  save(mpa_protected_seas, file = "data/mpa_protected_seas.Rdata")
  
  
  
}