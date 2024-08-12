describe_results <- function(SAR_stats){
 
    #Data with unique info for each considered MPA
    MPA_data <- SAR_stats %>%
      distinct(id_iucn, .keep_all = T)
  
    #Total number of fishing vessels
    SAR_fishing <- SAR_data %>%
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
      dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown")) 
    
    nrow(SAR_fishing)
    
    #Total number of detections inside MPAs
    length(unique(SAR_stats$unique_id))
    
    #Ratio
    length(unique(SAR_stats$unique_id))/nrow(SAR_fishing)
    
    #Number of MPAs considered
    length(unique(mpa_wdpa$id_iucn))
    
    #Number of MPAs with fishing inside
    length(unique(MPA_data$id_iucn))
    
    #Ratio
    length(unique(MPA_data$id_iucn))/length(unique(mpa_wdpa$id_iucn))
    
    #As well as ratio surface wise
    surface_mpas <- sum((MPA_union %>%
      mutate(mpa_area = as.numeric(set_units(st_area(MPA_union %>% st_as_sf()),km^2))))$mpa_area)
    
    #For each IUCN category was it highest ?
    iucn_cats <- MPA_data %>%
      group_by(iucn_cat) %>%
      reframe(sum_all_median = mean(sum_all),
              sum_all_IQR = sd(sum_all),
              relative_sum_all_median = mean(relative_sum_all),
              relative_sum_all_IQR = sd(relative_sum_all),
              unmatched_fishing_median = mean(unmatched_fishing),
              unmatched_fishing_IQR = sd(unmatched_fishing),
              unmatched_relative_median = mean(unmatched_relative),
              unmatched_relative_IQR = sd(unmatched_relative),
              unmatched_ratio_mean = mean(unmatched_ratio),
              unmatched_ratio_sd = sd(unmatched_ratio),
              unmatched_ratio_median = mean(unmatched_ratio),
              unmatched_ratio_IQR = sd(unmatched_ratio)) %>%
      ungroup()
    
    format_power(sd(MPA_data$relative_sum_all), set_power = -3)
    
    #Density in MPAs
    range(MPA_data$relative_sum_all)
    mean(MPA_data$relative_sum_all)
    sd(MPA_data$relative_sum_all)
        
    #DEnsity outside MPas
    range(SAR_eez_final$relative_sum_all)
    mean(SAR_eez_final$relative_sum_all)
    sd(SAR_eez_final$relative_sum_all)
    
    #Vessel size
    range((SAR_stats %>% distinct(unique_id,.keep_all = T))$length_m)
    mean((SAR_stats %>% distinct(unique_id,.keep_all = T))$length_m)
    sd((SAR_stats %>% distinct(unique_id,.keep_all = T))$length_m)
    
    iucn_length <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      group_by(iucn_cat) %>%
      reframe(length_m_median = mean(length_m),
              length_m_IQR = sd(length_m)) %>%
      ungroup()
    
    #Vessel size outside
    range(SAR_eez_final$length_m)
    mean(SAR_eez_final$length_m)
    sd(SAR_eez_final$length_m)
    
    #Number of detections inside MPA
    SAR_fishing_unmatched <- SAR_fishing %>% filter(category == "unmatched_fishing")
    nrow(SAR_fishing_unmatched)
    
    #Ratio
    nrow(SAR_fishing_unmatched)/nrow(SAR_fishing)
    
    #Unmatched in MPAs
    SAR_mpa_unmatched <- SAR_stats %>% filter(matched_category == "unmatched")
    length(unique(SAR_mpa_unmatched$unique_id))
    
    length(unique(SAR_mpa_unmatched$unique_id))/length(unique(SAR_stats$unique_id))
    
    #Where was unmatched fishing in MPA
    length(unique(((MPA_data %>% filter(unmatched_fishing>0)))$id_iucn))
    
    length(unique(((MPA_data %>% filter(unmatched_fishing>0)))$id_iucn))/length(unique(MPA_data$id))
    
    #RAnge of unmatched fishing
    range(MPA_data$unmatched_relative)
    mean(MPA_data$unmatched_relative)
    sd(MPA_data$unmatched_relative)
    
    #Number of unmatched fishing relative
    range(SAR_eez_final$unmatched_relative,na.rm=T)
    mean(SAR_eez_final$unmatched_relative,na.rm=T)
    IQR(SAR_eez_final$unmatched_relative,na.rm=T)
    
    #Fration of unmatched vessels inside MPAs
    range(MPA_data$unmatched_ratio)
    mean(MPA_data$unmatched_ratio)
    sd(MPA_data$unmatched_ratio)
    
    range(SAR_eez_final$unmatched_ratio)
    mean(SAR_eez_final$unmatched_ratio)
    sd(SAR_eez_final$unmatched_ratio)
    
    #Get yearly values
    SAR_yearly <- SAR_mpa %>%
      distinct(unique_id, .keep_all = T) %>%
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
      #Add date, week and month info
      #Converting to date
      mutate(date = gsub( " .*$", "", timestamp),
             date = as.Date(date,format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      #Creating week and month column to group data
      mutate(year = cut.Date(date, breaks = "1 year")) %>%
      # #Mean number of overpasses per 12 days
      group_by(year) %>%
      mutate(mean_overpasses = round(mean(overpasses_2017_2021))) %>%
      ungroup() %>%
      #Calculate stats
      add_count(year, category,name = "match_count") %>%
      mutate(match_count = match_count/mean_overpasses) %>%
      pivot_wider(names_from = "category",values_from = "match_count") %>%
      distinct(year, matched_fishing,unmatched_fishing) %>%
      group_by(year) %>%
      summarise(unmatched_fishing= last(na.omit(unmatched_fishing)), 
                matched_fishing = last(na.omit(matched_fishing))) %>%
      ungroup() %>%
      mutate(sum = matched_fishing+unmatched_fishing)
    
    #For each country, calculating the fraction of all vessels that was found inside MPAs
    
    #Fraction of fleet that is unmatched outside and inside MPAs
    SAR_fraction_MPA <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      mutate(matched_category = ifelse(matched_category %in% c("matched_fishing","matched_unknown"),"matched_fishing","unmatched")) %>%
      dplyr::select(parent_iso, matched_category) %>%
      group_by(parent_iso,matched_category) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
      pivot_wider(names_from = "matched_category",values_from = "sum_country") %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(fraction = unmatched/(matched_fishing+unmatched))
    
    range(SAR_fraction_MPA$fraction)
    mean(SAR_fraction_MPA$fraction)
    sd(SAR_fraction_MPA$fraction)
    
    SAR_fraction <- SAR_eez_final %>%
      distinct(unique_id, .keep_all = T) %>%
      mutate(matched_category = ifelse(matched_category %in% c("matched_fishing","matched_unknown"),"matched_fishing","unmatched")) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      dplyr::rename(parent_iso = "ISO_TER1") %>%
      dplyr::select(parent_iso, matched_category) %>%
      group_by(parent_iso,matched_category) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
      pivot_wider(names_from = "matched_category",values_from = "sum_country") %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(fraction = unmatched/(matched_fishing+unmatched))
  
    range(SAR_fraction$fraction)
    mean(SAR_fraction$fraction)
    sd(SAR_fraction$fraction)
    
    #MPAS with no management plan
    mpa_no_management <- mpa_wdpa_no_sf %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      filter(management_plan == "No management plan")
    
    nrow(mpa_no_management)/nrow(mpa_wdpa_no_sf)
    
    #Density mpas with no management
    mpa_no_management_density <- SAR_stats %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      filter(management_plan == "No management plan")
    
    nrow(mpa_no_management_density)/nrow(SAR_stats %>% distinct(id_iucn))
    
    #MPAS with no iucn cat
    mpa_no_iucn<- mpa_wdpa_no_sf %>%
      distinct(id_iucn, .keep_all = T) %>%
      filter(!iucn_cat %in% c("Ia","Ib","II","III","IV","VI","V"))
    
    nrow(mpa_no_iucn)/nrow(mpa_wdpa_no_sf %>% distinct(id_iucn))
    
    #Density mpas with no iucn cat
    mpa_no_iucn_density <- SAR_stats %>%
      distinct(id_iucn, .keep_all = T) %>%
      filter(!iucn_cat %in% c("I","II","III","IV","VI","V"))
    
    nrow(mpa_no_iucn_density)/nrow(SAR_stats %>% distinct(id_iucn))
    
    #Number of presence/absence by iucn cat
    fishing_presence_iucn <- mpa_wdpa_no_sf %>%
      mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat)) %>%
      #sCheck if fhsing in MPA
      mutate(fishing_presence = as.factor(ifelse(id_iucn %in% MPA_data$id_iucn,"Fishing","No_fishing"))) %>%
      group_by(iucn_cat, fishing_presence) %>%
      reframe(n = n())
    
  }
