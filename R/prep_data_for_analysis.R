prep_data_for_analysis <- function(SAR_stats, mpa_wdpa){
  
  SAR_stats_mpa <- SAR_stats %>%
    #Add average length per mpa
    group_by(id_iucn) %>%
    mutate(average_length = mean(length_m)) %>%
    ungroup() %>%
    #Select mpas
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, average_length, fishing:unmatched_relative)
  
  MPA_final_vars <- mpa_wdpa %>%
    st_drop_geometry() %>%
    dplyr::select(id_iucn, iucn_cat, status_yr, area_correct, mang_plan, parent_iso) %>%
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    #Join sar data
    left_join(SAR_stats_mpa, by = "id_iucn") %>%
    mutate(across(c(fishing, unmatched_fishing,
                  sum_all, unmatched_relative, relative_sum_all), ~ replace_na(., 0))) %>%
    #Metrics for both years
    mutate(SAR_presence = as.factor(ifelse(sum_all == 0, "No_SAR","SAR")),
           SAR_presence= fct_relevel(SAR_presence, c("No_SAR","SAR")),
           matched_density = fishing/area_correct,
           unmatched_density = unmatched_fishing/area_correct,
           SAR_density = sum_all/area_correct) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I",iucn_cat)) %>%
    mutate(iucn_cat = factor(iucn_cat, levels = level_order)) %>%
    #Distinct MPAs
    distinct(id_iucn, .keep_all = T) %>%
    #Management plan info
    mutate(management_plan = as.factor(ifelse(mang_plan %in% c(" Management plan is not implemented and not available",
                                                     "No","Management plan is not implented and not available",
                                                     "Not Reported"),"No management plan","Management plan"))) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name"))
  
  return(MPA_final_vars)
  
}