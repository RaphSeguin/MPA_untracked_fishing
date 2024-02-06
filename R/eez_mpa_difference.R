eez_mpa_difference <- function(eez, mpa_wdpa, SAR_data_sf){

  #Union of all MPAs
  all_mpas = mpa_wdpa %>% st_union()
  save(all_mpas, file = "output/all_mpas.Rdata")
  
  #EEZ without MPAs
  eez_no_mpa <- st_difference(eez %>% st_transform(crs = st_crs(all_mpas)), all_mpas) 
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
  
  #Joining with EEZ
  SAR_eez <- st_join(SAR_outside_mpas, eez_no_mpa, left = F)
  
  #Get mean number of sar acquisiitons per eez and divide detections by it
  SAR_eez_clean <- SAR_eez %>%
    st_drop_geometry() %>%
    distinct(unique_id, .keep_all = T) %>%
    #Mean number of overpasses per mpa
    group_by(MRGID_SOV1) %>%
    mutate(mean_overpasses = round(mean(overpasses_2017_2021))) %>%
    ungroup() %>%
    #Calculate stats
    add_count(MRGID_SOV1, category,name = "match_count") %>%
    #Dividing unmatched and matched by this mean
    mutate(match_count = match_count/mean_overpasses) %>%
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