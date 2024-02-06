calculate_stats <- function(SAR_mpa){
  
  #For each MPA, calculating the number of unmatched fishing boats, the number of unmatched fishing boats/km^2 and the ratio between matched and unmatched
  SAR_stats <- SAR_mpa %>%
    #replacing id with id_iucn
    mutate(id_iucn = as.factor(id_iucn)) %>%
    #Remove IUCN cat because of lack of points
    filter(iucn_cat != "III") %>%
    #Cleaning data 
    #Converting country name
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
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
    #Mean number of overpasses per mpa
    group_by(id_iucn) %>%
    mutate(mean_overpasses = round(mean(overpasses_2017_2021))) %>%
    ungroup() %>%
    #Calculate stats
    add_count(id_iucn, category,name = "match_count") %>%
    #Dividing unmatched and matched by this mean
    mutate(match_count = match_count/mean_overpasses) %>%
    pivot_wider(names_from = "category",values_from = "match_count") 
    
  #Function to coalescec columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  #Calculating ratio for relative values only
  SAR_stats_ratio <- SAR_stats %>%
    group_by(id_iucn) %>%
    #Keeeping only distinct values of columns of interest for each MPA
    distinct(id_iucn, unmatched_fishing, matched_fishing,gis_m_area) %>%
    #Coalescing columns
    summarise_all(coalesce_by_column) %>%
    #NAs are 0 
    replace(is.na(.), 0) %>%
    #Illegal ratio
    mutate(sum_all = unmatched_fishing + matched_fishing,
           relative_sum_all = (sum_all/gis_m_area),
           unmatched_ratio = unmatched_fishing/(matched_fishing + unmatched_fishing),
           unmatched_relative = (unmatched_fishing/gis_m_area)) %>%
    #Keeping only the ratio
    dplyr::select(c(id_iucn, matched_fishing,unmatched_fishing,sum_all,relative_sum_all, unmatched_ratio,unmatched_relative))
  
  SAR_stats_final <- SAR_stats %>%
    dplyr::select(-c(matched_fishing,unmatched_fishing)) %>%
    left_join(SAR_stats_ratio, by = "id_iucn") %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat))
  
  return(SAR_stats_final)
  
  }