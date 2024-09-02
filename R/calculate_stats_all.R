calculate_stats_all <- function(SAR_mpa_all){
  
  #For each MPA, calculating the number of unmatched fishing boats, the number of unmatched fishing boats/km^2 and the ratio between matched and unmatched
  SAR_stats <- SAR_mpa_all %>%
    st_drop_geometry() %>%
    # Only keep detections where at least 20 images were taken
    filter(image_count >= 20) %>%
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
    filter(length_m < quantile(SAR_mpa_all$length_m, 0.95, na.rm = T)) %>%
    # Calculate, using dynamic column names
    group_by(id_iucn, category) %>%
    mutate(match_count = sum(normalized_detection,na.rm = T)) %>%
    ungroup() %>%
    # Calculate stats
    pivot_wider(names_from = "category", values_from = "match_count") 
  
  # Function to coalesce columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  # Calculating ratio for relative values only
  SAR_stats_ratio <- SAR_stats %>%
    group_by(id_iucn) %>%
    # Keeping only distinct values of columns of interest for each MPA
    distinct(id_iucn, unmatched_fishing, fishing, gis_m_area) %>%
    # Coalescing columns
    summarise_all(coalesce_by_column) %>%
    # NAs are 0 
    replace(is.na(.), 0) %>%
    # Illegal ratio
    mutate(sum_all = unmatched_fishing + fishing,
           relative_sum_all = (sum_all / gis_m_area),
           unmatched_ratio = unmatched_fishing / (fishing + unmatched_fishing),
           unmatched_relative = (unmatched_fishing / gis_m_area)) %>%
    # Keeping only the ratio
    dplyr::select(c(id_iucn, fishing, unmatched_fishing, sum_all, relative_sum_all, unmatched_ratio, unmatched_relative))
  
  SAR_stats_final <- SAR_stats %>%
    dplyr::select(-c(fishing, unmatched_fishing)) %>%
    left_join(SAR_stats_ratio, by = "id_iucn") %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia", "Ib"), "I", iucn_cat))
  
  return(SAR_stats_final)
  
  
}