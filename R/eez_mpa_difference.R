eez_mpa_difference <- function(eez, mpa_wdpa, SAR_data_sf){
  
  #First, intersect EEZ with study area
  study_area <- read.csv("data/study_area.csv") %>%
    dplyr::select(study_area) %>%
    # pivot_longer(cols = c(study_area,study_area_02,study_area_05)) %>%
    dplyr::rename(geometry = "study_area") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    head(18713) %>%
    st_union()
  
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
    filter(status_yr < 2017) %>%
    filter(!status %in% c("Proposed","Established","Not Reported")) %>%
    st_make_valid()

  #Union of all MPAs
  all_mpas = mpa_wdpa_all %>% st_union()
  save(all_mpas, file = "output/all_mpas.Rdata")
  
  #EEZ without MPAs
  eez_no_mpa <- st_difference(eez_no_coastline %>% st_transform(crs = st_crs(all_mpas)), all_mpas) %>%
    st_make_valid()
  
  save(eez_no_mpa, file = "output/eez_no_mpa.Rdata")
  
  #Getting all points which are not in MPAs using unique ID 
  SAR_outside_mpas <- SAR_data %>%
    #If 80% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",
                             #If the match is unknown, we consider it as a fishing boat if fishing score > 0.8
                             ifelse(matched_category == "matched_unknown" & fishing_score >= 0.8, "matched_fishing",
                                    # #If it is fishing boat but the length is higher than the longest fishing boat in the world then matched_nonfishing
                                    ifelse(matched_category == "matched_fishing" & length_m > 145, "matched_unknown",
                                           matched_category)))) %>%
    #Also if unmatched_fishing and length higher than 80% quantile then delete it 
    mutate(category = ifelse(category == "unmatched_fishing" & length_m > quantile(SAR_mpa$length_m, 0.8),"matched_unknown",category)) %>%
    mutate(category = ifelse(category == "unmatched_fishing" & length_m >145,"matched_unknown",category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown")) %>%
    dplyr::filter(!unique_id %in% unique(SAR_stats$unique_id)) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) 
  
  #SAR points outisde
  SAR_eez <- st_join(SAR_outside_mpas, eez_no_mpa, left = F)
  
  #Get mean number of sar acquisiitons per eez and divide detections by it
  SAR_eez_clean <- SAR_eez %>%
    st_drop_geometry() %>%
    mutate(unique_id = as.factor(unique_id)) %>%
    distinct(unique_id, .keep_all = T) %>%
    mutate(detection_count = 1,
           detection_count = detection_count/overpasses_2017_2021) %>%
    #Calculate stats
    group_by(MRGID_SOV1, category) %>%
    mutate(match_count = sum(detection_count)) %>%
    ungroup() %>%
    pivot_wider(names_from = "category",values_from = "match_count") 
  
  #Function to coalescec columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  #Getting area of non protected eez 
  eez_area <- eez_no_mpa %>%
    dplyr::select(MRGID_SOV1) %>%
    mutate(area_unprotected = st_area(eez_no_mpa),
           area_unprotected = as.numeric(set_units(area_unprotected,km^2))) %>%
    st_drop_geometry() %>%
    #Sum of eez parts
    group_by(MRGID_SOV1) %>%
    mutate(area_unprotected = sum(area_unprotected, na.rm=T)) %>%
    ungroup() %>% 
    distinct(MRGID_SOV1,area_unprotected)
  
  #Calculating ratio for relative values only
  SAR_eez_ratio <- SAR_eez_clean %>%
    group_by(MRGID_SOV1) %>%
    #Keeeping only distinct values of columns of interest for each MPA
    distinct(MRGID_SOV1, unmatched_fishing, matched_fishing) %>%
    left_join(eez_area, by = "MRGID_SOV1") %>%
    #Coalescing columns
    summarise_all(coalesce_by_column) %>%
    replace(is.na(.), 0) %>% 
    #Illegal ratio
    mutate(sum_all = unmatched_fishing + matched_fishing,
           relative_sum_all = (sum_all/area_unprotected),
           unmatched_ratio = unmatched_fishing/(matched_fishing + unmatched_fishing),
           unmatched_relative = (unmatched_fishing/area_unprotected)) %>%
    #Keeping only the ratio
    dplyr::select(c(MRGID_SOV1,  matched_fishing,unmatched_fishing,sum_all,relative_sum_all, unmatched_ratio,unmatched_relative))
  
  SAR_eez_final <- SAR_eez_ratio %>%
    dplyr::select(-c(matched_fishing,unmatched_fishing)) %>%
    left_join(SAR_eez_clean, by = "MRGID_SOV1") %>%
    mutate(iucn_cat = "EEZ") 
  
  save(SAR_eez_final, file = "output/SAR_eez_final.Rdata")
  return(SAR_eez_final)
  

}