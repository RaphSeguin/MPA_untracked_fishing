calculate_stats_time <- function(SAR_mpa){
  
  #For each MPA, calculating the number of unmatched fishing boats, the number of unmatched fishing boats/km^2 and the ratio between matched and unmatched
  SAR_stats_time <- SAR_mpa %>%
    st_drop_geometry() %>%
    distinct(unique_id, .keep_all = T) %>%
    #Converting country name
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
    #If 80% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.9, "unmatched_fishing",
                             #If the match is unknown, we consider it as a fishing boat if fishing score > 0.8
                             # ifelse(matched_category == "matched_unknown" & fishing_score >= 0.9, "matched_fishing",
                                    # #If it is fishing boat but the length is higher than the longest fishing boat in the world then matched_nonfishing
                                    ifelse(matched_category == "matched_fishing" & length_m > 145, "matched_unknown",
                                           matched_category))) %>%
    #Also if unmatched_fishing and length higher than 80% quantile then delete it 
    mutate(category = ifelse(category == "unmatched_fishing" & length_m > quantile(SAR_mpa$length_m, 0.8),"matched_unknown",category)) %>%
    mutate(category = ifelse(category == "unmatched_fishing" & length_m >145,"matched_unknown",category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown")) %>% 
    #Add date, week and month info
    #Converting to date
    mutate(date = gsub( " .*$", "", timestamp),
           date = as.Date(date,format = "%Y-%m-%d")) %>%
    arrange(date) %>%
    #Creating week and month column to group data
    mutate(week = cut.Date(date, breaks = "12 days"),
           month = cut.Date(date, breaks = "1 month")) %>%
    # #Mean number of overpasses per 12 days
    # group_by(week) %>%
    # mutate(mean_overpasses = round(mean(overpasses_2017_2021))) %>%
    # ungroup() %>%
    #Calculate stats
    add_count(week, category,name = "match_count") %>%
    mutate(match_count = match_count) %>%
    pivot_wider(names_from = "category",values_from = "match_count") 
  
  #Function to coalescec columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  #Calculating ratio for relative values only
  SAR_stats_ratio_time <- SAR_stats_time %>%
    group_by(week) %>%
    #Keeeping only distinct values of columns of interest for each MPA
    distinct(week, unmatched_fishing, matched_fishing,gis_m_area) %>%
    #Coalescing columns
    summarise_all(coalesce_by_column) %>%
    #NAs are 0 
    replace(is.na(.), 0) %>%
    #Illegal ratio
    mutate(sum_all = unmatched_fishing+matched_fishing) %>%
    #Keeping only the ratio
    dplyr::select(c(week, sum_all))
  
  SAR_stats_time_final <- SAR_stats_time %>%
    left_join(SAR_stats_ratio_time, by = "week") 
  
  return(SAR_stats_time_final)
  
  
}