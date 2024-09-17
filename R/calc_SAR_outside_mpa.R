calc_SAR_outside_mpa <- function(SAR_data_sf, eez_no_mpa){
  
  #Getting all points which are not in MPAs 
  SAR_outside_mpas <- SAR_data_sf %>%
    # Drop all factor levels except two
    mutate(matched_category = droplevels(factor(matched_category, levels = c("unmatched", "fishing"))),
           matched_category = as.character(matched_category)) %>%
    # If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = as.factor(ifelse(matched_category == "unmatched" & fishing_score >= 0.9, "unmatched_fishing",
                                       matched_category))) %>%
    # Keep only fishing and unmatched_fishing
    filter(category %in% c("fishing", "unmatched_fishing")) %>%
    # Also if unmatched_fishing and length higher than 90% quantile then delete it 
    filter(length_m < 145) %>%
    filter(length_m < quantile(SAR_data_sf$length_m, 0.95, na.rm = T)) %>%
    #Convert to sf and join with EEZ
    st_as_sf(coords = c("lon","lat"), crs = 4326) %>%
    st_join(eez_no_mpa, left = F) %>%
    distinct(unique_id, .keep_all = T)
  
  return(SAR_outside_mpas)
  
}
