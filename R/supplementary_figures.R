supplementary_figures <- function(){
  
  #Data with unique info for each considered MPA
  MPA_data <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) 
  
  legend = c("I" = "#051D41",
             "II" = "#092C63",
             "III" = "#0E58C6",
             "IV" = "#2F79EE",
             "V" = "#5090EF",
             "VI"= "#93BAF8",
             "Not Applicable" = "#F29F79",
             "Not Assigned" = "#EF8B5B",
             "Not Reported" = "#D87458",
             "EEZ" = "#9B3742")
  
  #Description of MPA data
  (MPA_data_S1 <- mpa_wdpa_no_sf %>%
    distinct(id_iucn, .keep_all = T) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat)) %>%
    group_by(iucn_cat) %>%
    reframe(sum = n()) %>%
    ungroup() %>%
    ggplot() +
    geom_bar(aes(reorder(iucn_cat,sum),sum,fill=iucn_cat),stat='identity') +
    scale_fill_manual(values = legend)+
    coord_flip() +
    theme_minimal() +
    labs(x = " ",
          y = "Number of MPAs",
         fill = "IUCN Category")+
    theme(legend.position="bottom")) 
  
  ggsave(MPA_data_S1, file = "Figures/Supp/MPA_data_S1.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Description of MPA data
  (MPA_data_S2 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      group_by(country) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      arrange(-sum) %>%
      head(20) %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum),sum),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Number of MPAs"))
  
  ggsave(MPA_data_S2, file = "Figures/Supp/MPA_data_S2.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Description of MPA data
  (MPA_data_S3 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      # mutate(country = ifelse(parent_iso == 'PCN',"Pitcairn Islands",country)) %>%
      distinct(id_iucn, .keep_all = T) %>%
      group_by(country) %>%
      reframe(sum = sum(gis_m_area,na.rm=T)) %>%
      ungroup() %>%
      arrange(-sum) %>%
      head(20) %>%
      na.omit() %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum),sum),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = " "))
  
  ggsave(MPA_data_S3, file = "Figures/Supp/MPA_data_S3.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Management plan against relative
  (MPA_data_S4 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id_iucn, management_plan) %>%
      inner_join(MPA_data, by = "id_iucn") %>%
      group_by(management_plan) %>%
      na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,log(relative_sum_all))) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Density of fishing vessels"))
  
  ggsave(MPA_data_S4, file = "Figures/Supp/MPA_data_S4.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Management plan against relative
  (MPA_data_S5 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id_iucn, management_plan) %>%
      inner_join(MPA_data, by = "id_iucn") %>%
      group_by(management_plan) %>%
      na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,log(unmatched_relative))) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Density of not publicly tracked fishing vessels"))
  
  ggsave(MPA_data_S5, file = "Figures/Supp/MPA_data_S5.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Management plan against relative
  (MPA_data_S6 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id, management_plan) %>%
      inner_join(MPA_data, by = "id") %>%
      group_by(management_plan) %>%
      na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,unmatched_ratio)) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Fraction of not publicly tracked fishing vessels"))
  
  ggsave(MPA_data_S6, file = "Figures/Supp/MPA_data_S6.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #MCountry vs total
  (MPA_data_S7 <- MPA_data %>%
      filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      group_by(country) %>%
      reframe(mean = mean(relative_sum_all)) %>%
      ungroup() %>%
      arrange(-mean) %>%
      head(20) %>%
      ggplot() +
      geom_bar(aes(reorder(country,mean),mean),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Average density of fishing vessels inside MPAs"))
  
  ggsave(MPA_data_S7, file = "Figures/Supp/MPA_data_S7.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Unmatched
  (MPA_data_S8 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      group_by(country) %>%
      reframe(mean = mean(unmatched_relative)) %>%
      ungroup() %>%
      arrange(-mean) %>%
      head(20) %>%
      ggplot() +
      geom_bar(aes(reorder(country,mean),mean),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "Average density of not publicly tracked fishing vessels inside MPAs"))
  
  ggsave(MPA_data_S8, file = "Figures/Supp/MPA_data_S8.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Across time
  (MPA_data_S9 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr)) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,log(relative_sum_all),group = status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Density of fishing vessels inside MPAs"))
      # scale_x_discrete(breaks = every_nth(MPA_data$status_yr,5)))
      # scale_x_continuous(breaks=seq(1902, 2016, 5)))
      # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  
  ggsave(MPA_data_S9, file = "Figures/Supp/MPA_data_S9.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  (MPA_data_S10 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr)) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,log(unmatched_relative),group = status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Density of not publicly tracked fishing vessels inside MPAs"))
      
  ggsave(MPA_data_S10, file = "Figures/Supp/MPA_data_S10.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  (MPA_data_S11 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr),group =) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,unmatched_ratio,group =status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Fraction of not publicly tracked fishing vessels inside MPAs"))

  ggsave(MPA_data_S11, file = "Figures/Supp/MPA_data_S11.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Length distribution of detected fishing vessels
  
  level_order_size <- c("0-15m","15-25m","25-35m","35-45","45-55",">55") 
  
  #Length outside maps
  (MPA_data_S12 <- SAR_eez_final %>%
      
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      group_by(size_class) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_bar(aes(factor(size_class,level=level_order_size),sum,group=size_class),stat='identity')+
      theme_minimal() +
      labs(x = "Size class",
           y = "Number of detections"))
  
  ggsave(MPA_data_S12, file = "Figures/Supp/MPA_data_S12.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #IN maps
  (MPA_data_S13 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      group_by(size_class) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_bar(aes(factor(size_class,level=level_order_size),sum,group=size_class),stat='identity')+
      theme_minimal() +
      labs(x = "Size class",
           y = "Number of detections"))
  
  ggsave(MPA_data_S13, file = "Figures/Supp/MPA_data_S13.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Distribution outside MPAs
  (MPA_data_S14 <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                      ifelse(length_m < 25, "15-25m",
                                             ifelse(length_m < 35, "25-35m",
                                                    ifelse(length_m < 45,"35-45",
                                                           ifelse(length_m < 55, "45-55",">55")))))) %>%
    ggplot() +
    geom_violin(aes(factor(size_class,level=level_order_size),length_m,group=size_class))+
    theme_minimal() +
    labs(x = "Size class",
         y = "Vessel length"))

  ggsave(MPA_data_S14, file = "Figures/Supp/MPA_data_S14.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Distribution in MPAs
  (MPA_data_S15 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      ggplot() +
      geom_violin(aes(factor(size_class,level=level_order_size),length_m,group=size_class))+
      theme_minimal() +
      labs(x = "Size class",
           y = "Vessel length"))
  
  ggsave(MPA_data_S15, file = "Figures/Supp/MPA_data_S15.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #Fraction of fleet that is inside MPA
  SAR_inside_mpas <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "inside_mpa") %>%
    dplyr::select(parent_iso,matched_category, type)
  
  (MPA_data_S16 <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1") %>%
    dplyr::select(parent_iso, matched_category, type) %>%
    bind_rows(SAR_inside_mpas) %>%
    #Group by type and parent_iso
    group_by(type, parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    #Widen data to calculate some stuff
    pivot_wider(names_from = "type",values_from="sum_country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(sum_country = inside_mpa + outside_mpa,
           fraction_country = inside_mpa/(outside_mpa+inside_mpa)) %>%
    filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
    arrange(-fraction_country) %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    #Pivot longer for plots
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw")+
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Number of fishing vessels detected inside EEZ",
         fill = "Fraction of fishing vessels detected: ") +
    theme(legend.position = "top"))
  
  ggsave(MPA_data_S16, file = "Figures/Supp/MPA_data_S16.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  #Fraction of dark fleet that is inside MPA
  (MPA_data_S17 <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1") %>%
    dplyr::select(parent_iso, matched_category, type) %>%
    filter(matched_category=="unmatched") %>%
    bind_rows(SAR_inside_mpas) %>%
    #Group by type and parent_iso
    group_by(type, parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    #Widen data to calculate some stuff
    pivot_wider(names_from = "type",values_from="sum_country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(sum_country = inside_mpa + outside_mpa,
           fraction_country = inside_mpa/(outside_mpa+inside_mpa)) %>%
    filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
    arrange(-fraction_country) %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    #Pivot longer for plots
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw")+
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Number of not publicly tracked fishing vessels detected inside EEZ",
         fill = "Fraction of not publicly tracked fishing vessels detected: ") +
    theme(legend.position = "top"))
  
  ggsave(MPA_data_S17, file = "Figures/Supp/MPA_data_S17.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #All vessels
  (MPA_data_S19 <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1") %>%
    dplyr::select(parent_iso, matched_category, type) %>%
    # filter(matched_category=="unmatched") %>%
    bind_rows(SAR_inside_mpas) %>%
    #Group by type and parent_iso
    group_by(parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    arrange(-sum_country) %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,sum_country), sum_country),stat="identity") +
    theme_minimal(base_size = 14) +
    coord_flip() +
    labs(x = " ",
         y = "Number of vessels detected in EEZ"))
  
  ggsave(MPA_data_S19, file = "Figures/Supp/MPA_data_S19.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  #All vessels unmatched
  (MPA_data_SX <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1") %>%
    dplyr::select(parent_iso, matched_category, type) %>%
    filter(matched_category=="unmatched") %>%
    bind_rows(SAR_inside_mpas) %>%
    #Group by type and parent_iso
    group_by(parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    arrange(-sum_country) %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,sum_country), sum_country),stat="identity") +
    theme_minimal(base_size = 14) +
    coord_flip() +
    labs(x = " ",
         y = "Number of unmatched vessels detected in EEZ"))
  
  ggsave(MPA_data_SX, file = "Figures/Supp/MPA_data_SX.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  #Inside MPAs
  (MPA_data_S20 <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "inside_mpa") %>%
    dplyr::select(parent_iso,matched_category, type) %>%
    group_by(parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    arrange(-sum_country) %>%
    #Deleteing joint regime to avoid double counting in same EEZ
    filter(parent_iso != "FRA;ITA;MCO") %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,sum_country), sum_country),stat="identity") +
    theme_minimal(base_size = 14) +
    coord_flip() +
    labs(x = " ",
           y = "Number of fishing vessels detected in MPAs"))
  
  ggsave(MPA_data_S20, file = "Figures/Supp/MPA_data_S20.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  #Inside MPAs unmatched
  (MPA_data_S21 <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "inside_mpa") %>%
    dplyr::select(parent_iso,matched_category, type) %>%
    filter(matched_category=="unmatched") %>%
    group_by(parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    arrange(-sum_country) %>%
    #Deleteing joint regime to avoid double counting in same EEZ
    filter(parent_iso != "FRA;ITA;MCO") %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,sum_country), sum_country),stat="identity") +
    theme_minimal(base_size = 14) +
    coord_flip() +
    labs(x = " ",
         y = "Number of not publicly tracked vessels detected in MPAs"))
  
  ggsave(MPA_data_S21, file = "Figures/Supp/MPA_data_S21.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  #Matched vs unmatched
  MPA_data_S22 <- MPA_data %>% 
    ggplot(aes(log(matched_fishing), log(unmatched_fishing),size = gis_m_area,fill = iucn_cat)) +
    geom_point(alpha = 0.5,shape=21, color="black") +
    scale_size(range = c(.5, 20), name="MPA area (km2)") + 
    scale_fill_viridis(discrete=TRUE, option="A") +
    theme_minimal(base_size = 12) +
    theme(legend.position="bottom") +
    labs(x = "Sum of publicly tracked fishing vessels in MPAs (log-transformed)",
         y = "Sum of not publicly tracked fishing vessels in MPAs (log-transformed)",
         fill = "IUCN category") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE))
  
  ggsave(MPA_data_S22, file = "Figures/Supp/MPA_data_S22.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
      
    
}
