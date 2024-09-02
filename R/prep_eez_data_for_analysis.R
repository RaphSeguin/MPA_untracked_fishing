prep_eez_data_for_analysis <- function(SAR_eez_stats, eez){
  
  SAR_stats_eez <- SAR_eez_stats %>%
    #Add average length per mpa
    group_by(ISO_SOV1) %>%
    mutate(average_length = mean(length_m)) %>%
    ungroup() %>%
    #Select mpas
    distinct(ISO_SOV1, .keep_all = T) %>%
    dplyr::select(ISO_SOV1, average_length, fishing:unmatched_relative)
  
  EEZ_final_vars <- eez_no_mpa %>%
    st_drop_geometry() %>%
    dplyr::select(ISO_SOV1, SOVEREIGN1, eez_area) %>%
    #Join sar data
    left_join(SAR_stats_eez, by = "ISO_SOV1") %>%
    mutate(across(c(fishing, unmatched_fishing,
                    sum_all, unmatched_relative, relative_sum_all), ~ replace_na(., 0))) %>%
    #Metrics for both years
    mutate(SAR_presence = as.factor(ifelse(sum_all == 0, "No_SAR","SAR")),
           SAR_presence= fct_relevel(SAR_presence, c("No_SAR","SAR")),
           matched_density = fishing/eez_area,
           unmatched_density = unmatched_fishing/eez_area,
           SAR_density = sum_all/eez_area) %>%
    #Distinct MPAs
    distinct(ISO_SOV1, .keep_all = T) %>%
    filter(ISO_SOV1 != "SYC") %>%
    mutate(country = countrycode(ISO_SOV1,origin="iso3c",destination="country.name")) %>%
    mutate(iucn_cat = "EEZ")
  
  return(EEZ_final_vars)
  
  
}