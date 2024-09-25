plot_fishing_density <- function(mpa_model,
                                 fishing_presence_2022, fishing_presence_2023,
                                 fishing_hours_2022, fishing_hours_2023){
  
  #Prediction
  MPA_fishing <- mpa_model %>%
    #add correct size
    dplyr::select(-area_correct) %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, area_correct) %>% st_drop_geometry(), by = "id_iucn") %>%
    st_drop_geometry()%>%
    left_join(fishing_presence_2022 %>% dplyr::select(id_iucn, fishing_presence_predicted_2022), by = "id_iucn") %>%
    left_join(fishing_presence_2023 %>% dplyr::select(id_iucn, fishing_presence_predicted_2023), by = "id_iucn") %>%
    left_join(fishing_hours_2022 %>% dplyr::select(id_iucn, predicted_fishing_effort_2022), by = "id_iucn") %>%
    left_join(fishing_hours_2023 %>% dplyr::select(id_iucn, predicted_fishing_effort_2023), by = "id_iucn") %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023,
           SAR_all = sum_all_2022 + sum_all_2023,
           SAR_unmatched_all = unmatched_fishing_2022 + unmatched_fishing_2023, 
           SAR_matched_all = fishing_2022 + fishing_2023,
           presence_observed = as.factor(ifelse(fishing_presence_2022 == "Fishing" | fishing_presence_2023 == "Fishing", 
                                                "Fishing","No_fishing")),
           presence_predicted = as.factor(ifelse(fishing_presence_predicted_2022 == "Fishing" | fishing_presence_predicted_2023 == "Fishing", 
                                                 "Fishing","No_fishing")),
           predicted_fishing_all = predicted_fishing_effort_2022 + predicted_fishing_effort_2023) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I", iucn_cat)) %>%
    #Add density
    mutate(AIS_fishing_2022_density = AIS_fishing_2022/area_correct,
           AIS_fishing_2023_density = AIS_fishing_2023/area_correct,
           predicted_fishing_effort_2022_density = predicted_fishing_effort_2022/area_correct,
           predicted_fishing_effort_2023_density = predicted_fishing_effort_2023/area_correct) %>%
    #Avereage density over 2 years
    mutate(AIS_average_density = rowMeans(dplyr::select(., AIS_fishing_2022_density, AIS_fishing_2023_density), na.rm = TRUE),
           predicted_average_density = rowMeans(dplyr::select(., predicted_fishing_effort_2022_density, 
                                                              predicted_fishing_effort_2023_density), na.rm = TRUE)) %>%
    filter(predicted_average_density > 0)
  
  #Plot density by IUCN cat
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
  
  #plotting total number of  fishing vessels in MPAs against IUCN Category
  (density_by_iucn <- MPA_fishing %>%
      ggplot(aes(factor(iucn_cat,level = level_order), log(predicted_average_density), fill = iucn_cat)) + 
      scale_fill_manual(values = legend,
                        breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported"),
                        guide = "none") + 
      geom_jitter(alpha = 0.4, shape = ".") +
      geom_boxplot(alpha = 0.7) +
      labs(x = " ",
           y = "Predicted fishing density log(hours/km²)",
           fill = "IUCN Category") +
      my_custom_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  
  ggsave(density_by_iucn,
         file = "figures/density_by_iucn.jpg", 
         width = 297, height = 125, units = "mm",dpi=600)
  
  #IUCN
  MPA_fishing_IUCN <- MPA_fishing %>%
    group_by(iucn_cat) %>%
    reframe(number = n(),
            density = mean(predicted_average_density),
            density_sd =  sd(predicted_average_density)) %>%
    ungroup()
  
  #COUNTRY
  MPA_fishing_IUCN <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn, country), by = "id_iucn") %>%
    group_by(country) %>%
    reframe(number = n(),
            density = mean(predicted_average_density),
            density_sd =  sd(predicted_average_density)) %>%
    ungroup() %>%
    filter(number > 10) %>%
    arrange(desc(density)) %>%
    slice(1:20)
    
  density_by_country <- ggplot(MPA_fishing_IUCN, aes(x = reorder(country,density), y = density, fill = type)) +
    geom_bar(stat = "identity",  fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = " ", y = "Fishing effort density (hours/km²)", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom")
  
  ggsave(density_by_country, 
         file = "figures/supp/density_by_country.jpg",
         width = 297, height = 210,
         units = "mm", dpi = 300)

  #Comparison test
  EEZ_MPA_stat_test <- MPA_fishing %>%
    dplyr::filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
    dunn_test(predicted_average_density ~ iucn_cat, p.adjust.method = "bonferroni") %>%
    mutate(
      comparison = paste0(group1, " vs ", group2),
      higher_group = ifelse(statistic > 0, group1, group2)
    ) %>%
    mutate_if(is.numeric, round, digits = 2)
  
  
}