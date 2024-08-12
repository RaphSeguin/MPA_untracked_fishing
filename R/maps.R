maps <- function(){
  
  level_order <- c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ") 
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
  
  mpa_shp <- mpa_wdpa %>% dplyr::select(id_iucn)
  
  #Data of illegal ratio for each MPA
  SAR_stats_mpa <- SAR_stats %>%
    distinct(id_iucn,.keep_all = T) %>%
    #Join with mpa data to have spatial info
    left_join(mpa_shp, by = "id_iucn") %>%
    st_as_sf()
  
  world <- ne_countries(scale = "large", returnclass = "sf") %>% st_transform(4326)
  
  #Map of Japan MPa
  #Spatial objects
  SAR_map_1 <- SAR_stats_mpa %>%
    filter(id == "555621413") 
  
  fishing_around_japan <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_crop(c(xmin = 139, xmax = 147, ymin = 41, ymax = 46)) %>%
    #If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  
  mpa_map_1 <- SAR_map_1 %>%
    ggplot() +
    geom_sf(aes(fill =iucn_cat), pch = 21, size = 3) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    geom_sf(data = world, fill = "lightgrey") +
    # geom_text(data=world, aes(143,43.5,label = "Japan"), size=6) +
    geom_sf(data = fishing_around_japan, aes(colour = matched_category), size = 0.1,alpha = 0.5) +
    scale_colour_manual(values = c("unmatched"="#000000",
                                   "matched_fishing"="#107733"),
                        labels = c("unmatched"="Not publicly tracked",
                                   "matched_fishing"="Publicly tracked")) +
    # geom_sf(data = SAR_stats %>%
    #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
    labs(
      # title=paste(SAR_map_1$name,", Japan. IUCN Cat:",SAR_map_1$iucn_cat),
         colour = "Detection type",
         fill = "IUCN category") +
    theme(legend.position = "bottom") +
    coord_sf() +
    xlim(139,147) +
    ylim(41,46) +
    theme_map(base_size = 14,)+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    guides(color = guide_legend(override.aes = list(size=5))) +
    annotation_scale(location = "br") +
    theme(legend.position = "none") 
  
  ggsave(mpa_map_1, file = "figures/mpa_map_1.jpg", width = 597, height = 280,dpi=300,units="mm")
  
  #Map of other MPa
  #Spatial objects
  # SAR_map_2 <- SAR_stats_mpa %>%
  #   filter(id == "555705603")
  # 
  # fishing_around_map2 <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  #   st_crop(c(xmin = 116, xmax = 118, ymin = 6, ymax = 8)) %>%
  #   #If 90% sure that boat is fishing, it is unmatched and fishing
  #   mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
  #   dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  # 
  # mpa_map_2 <- SAR_map_2 %>%
  #   ggplot() +
  #   geom_sf(fill = "lightblue", pch = 21, size = 3) +
  #   geom_sf(data = world, fill = "lightgrey") +
  #   geom_text(data=world, aes(117,6.3,label = "Malaysia"), size=6) +
  #   geom_sf(data = fishing_around_map2, aes(colour = matched_category), size = 0.1,alpha = 0.5) +
  #   scale_colour_manual(values = c("unmatched"="darkblue",
  #                                  "matched_fishing"="red"),
  #                       labels = c("unmatched"="Unmatched fishing vessel",
  #                                  "matched_fishing"="Matched fishing vessel")) +
  #   # geom_sf(data = SAR_stats %>%
  #   #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
  #   labs(
  #     # title=paste(SAR_map_2$name,", Malaysia, IUCN Cat:",SAR_map_2$iucn_cat),
  #        colour = "Detection type") +
  #   theme(legend.position = "bottom") +
  #   coord_sf() +
  #   xlim(116,118) +
  #   ylim(6,8) +
  #   theme_map(base_size = 14)+
  #   theme(legend.position = "bottom") +
  #   theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  #   guides(color = guide_legend(override.aes = list(size=5)))
  # 
  # ggsave(mpa_map_2, file = "figures/mpa_map_2.jpg", width = 297, height = 210,units="mm")
  
  #Map of other MPa
  #Spatial objects
  SAR_map_3 <- SAR_stats_mpa %>%
    filter(name == "Great Barrier Reef")
  
  fishing_around_map3 <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_crop(c(xmin = 142, xmax = 154, ymin = -24, ymax = -10)) %>%
    #If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  
  mpa_map_3 <- SAR_map_3 %>%
    ggplot() +
    geom_sf(aes(fill = iucn_cat), pch = 21, size = 3,alpha = 0.8) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    geom_sf(data = world, fill = "lightgrey") +
    # geom_text(data=world, aes(145,-21,label = "Australia"), size=6) +
    geom_sf(data = fishing_around_map3, aes(colour = matched_category), size = 0.5, alpha = 1) +
    scale_colour_manual(values = c("unmatched"="#000000",
                                   "matched_fishing"="#107733"),
                        labels = c("unmatched"="Not publicly tracked",
                                   "matched_fishing"="Publicly tracked")) +
    # geom_sf(data = SAR_stats %>%
    #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
    labs(
      # title=paste(SAR_map_3$name,", Australia, IUCN Cat:",SAR_map_3$iucn_cat),
         colour = "Detection type",
         fill = "IUCN category") +
    coord_sf() +
    xlim(142,154) +
    ylim(-23,-10) +
    theme_map(base_size = 14)+
    theme(legend.position = "bottom")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    guides(color = guide_legend(override.aes = list(size=5,shape = c(16, 16))))+
    annotation_scale(location = "tl")+
    theme(legend.position = "none") 
  
  
  
  ggsave(mpa_map_3, file = "figures/mpa_map_3.jpg",  width = 597, height = 280,dpi=300,units="mm")
  
  #Map of other MPa
  #Spatial objects
  SAR_map_4 <- SAR_stats_mpa %>%
    st_crop(c(xmin = 0, xmax = 25, ymin = 32, ymax = 45)) 

  fishing_around_map4 <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_crop(c(xmin = 0, xmax = 25, ymin = 32, ymax = 47)) %>%
    #If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  
  mpa_map_4 <- SAR_map_4 %>%
    ggplot() +
    geom_sf(aes(fill = iucn_cat), pch = 21, size = 3, alpha = 0.8) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    geom_sf(data = world, fill = "lightgrey") +
    # geom_text(data=world, aes(11.2,44,label = "Italy"), size=6) +
    # geom_text(data=world, aes(9.1,42.2,label = "France"), size=4) +
    geom_sf(data = fishing_around_map4, aes(colour = matched_category),shape = ".",alpha= 0.3) +
    scale_colour_manual(values = c("unmatched"="#000000",
                                   "matched_fishing"="#107733"),
                        labels = c("unmatched"="Not publicly tracked",
                                   "matched_fishing"="Publicly tracked")) +
    # geom_sf(data = SAR_stats %>%
    #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
    labs(
      # title=paste(SAR_map_4$name,", Italy, IUCN Cat:",SAR_map_4$iucn_cat),
         colour = "Detection type",
         fill = "IUCN category") +
    theme(legend.position = "bottom") +
    coord_sf() +
    xlim(0,20) +
    ylim(38,45) +
    theme_map(base_size = 14)+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    guides(color = guide_legend(override.aes = list(size=5,shape = c(16, 16))))+
    annotation_scale(location = "tl")+
    theme(legend.position = "none") 
  
  ggsave(mpa_map_4, file = "figures/mpa_map_4.jpg", width = 297, height = 210,units="mm")
  
  #Map 5
  # SAR_map_5 <- SAR_stats_mpa %>%
  #   filter(id == "555586804")
  # 
  # fishing_around_map5 <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  #   st_crop(c(xmin = -126, xmax = -124, ymin = 47, ymax = 49)) %>%
  #   #If 90% sure that boat is fishing, it is unmatched and fishing
  #   mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
  #   dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  # 
  # mpa_map_5 <- SAR_map_5 %>%
  #   ggplot() +
  #   geom_sf(fill = "lightblue", pch = 21, size = 3) +
  #   geom_sf(data = world, fill = "lightgrey") +
  #   geom_text(data=world, aes(-124.4,48.8,label = "USA"), size=6) +
  #   geom_sf(data = fishing_around_map5, aes(colour = matched_category), size = 0.1,alpha=0.5) +
  #   scale_colour_manual(values = c("unmatched"="darkblue",
  #                                  "matched_fishing"="red"),
  #                       labels = c("unmatched"="Not publicly tracked",
  #                                  "matched_fishing"="Publicly tracked")) +
  #   # geom_sf(data = SAR_stats %>%
  #   #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
  #   labs(
  #     # title=paste(SAR_map_5$name,", USA, IUCN Cat:",SAR_map_5$iucn_cat),
  #        colour = "Detection type") +
  #   theme(legend.position = "bottom") +
  #   coord_sf() +
  #   xlim(-126,-124) +
  #   ylim(47,49) +
  #   theme_map(base_size = 14)+
  #   theme(legend.position = "bottom")+
  #   theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  #   guides(color = guide_legend(override.aes = list(size=5)))
  # 
  # ggsave(mpa_map_5, file = "figures/mpa_map_5.jpg", width = 297, height = 210,units="mm")
  
  #Map 6
  SAR_map_6 <- SAR_stats_mpa %>%
    st_crop(c(xmin = -120, xmax = -50, ymin = -5, ymax = 35))
  
  fishing_around_map6 <- SAR_data %>% st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
    st_crop(c(xmin = -120, xmax = -50, ymin = -5, ymax = 35)) %>%
    #If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = ifelse(matched_category == "unmatched" & fishing_score >= 0.8, "unmatched_fishing",matched_category)) %>%
    dplyr::filter(!category %in% c("unmatched","matched_nonfishing","matched_unknown"))
  
  mpa_map_6 <- SAR_map_6 %>%
    ggplot() +
    geom_sf(aes(fill = iucn_cat), pch = 21, size = 3) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    geom_sf(data = world, fill = "lightgrey") +
    # geom_text(data=world, aes(-100.2,19.5,label = "Mexico"), size=6) +
    geom_sf(data = fishing_around_map6, aes(colour = matched_category), shape = ".",alpha=0.3) +
    scale_colour_manual(values = c("unmatched"="#000000",
                                   "matched_fishing"="#107733"),
                        labels = c("unmatched"="Not publicly tracked",
                                   "matched_fishing"="Publicly tracked"),drop = F) +
    # geom_sf(data = SAR_stats %>%
    #           filter(id == "555539599") %>% st_as_sf(coords = c("lon","lat"),crs=4326), aes(colour = matched_category),size = 0.1) +
    labs(
      # title=paste(SAR_map_6$name,", Mexico, IUCN Cat:",SAR_map_6$iucn_cat),
         colour = "Detection type",
         fill = "IUCN category") +
    theme(legend.position = "bottom") +
    coord_sf() +
    ylim(5,30) +
    xlim(-119,-60) +
    theme_map(base_size = 14)+
    theme(legend.position = "bottom",legend.box = "vertical") +
    guides(shape = guide_legend(order = 1,nrow = 1), 
           colour = guide_legend(order = 2,nrow = 1)) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    guides(color = guide_legend(override.aes = list(size=5,shape = c(16, 16))))+
    annotation_scale(location = "tr")
  
  ggsave(mpa_map_6, file = "figures/mpa_map_6.jpg", width = 297, height = 210,units="mm")

  }
