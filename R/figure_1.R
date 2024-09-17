figure_1 <- function(SAR_stats){
  
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

  #Plotting number of unmatched vessels in MPA according to time
  colors <- c("Matched fishing vessels" = "darkblue", "Unmatched fishing vessels" = "black")
  
  #plotting total number of  fishing vessels in MPAs against IUCN Category
  (total_iucn <- MPA_final_vars %>%
      bind_rows(EEZ_final_vars) %>%
      filter(SAR_density > 0) %>%
      ggplot(aes(factor(iucn_cat,level = level_order), log(SAR_density), fill = iucn_cat)) + 
      scale_fill_manual(values = legend,
                        breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"),
                        guide = "none") + 
      geom_jitter(alpha = 0.4, shape = ".") +
      geom_boxplot(alpha = 0.7) +
      labs(x = " ",
           y = "Density of vessel detections",
           fill = "IUCN Category") +
      my_custom_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  
  ggsave(total_iucn, file = "figures/total_iucn.jpg", width = 350, height = 210, units = "mm", dpi = 600)
  
  
  #plotting fishing vessel size
  (size_iucn <- SAR_stats %>%
     distinct(unique_id, .keep_all = T) %>%
    dplyr::select(iucn_cat, length_m) %>%
    bind_rows(SAR_eez_stats %>% mutate(iucn_cat = "EEZ") %>% dplyr::select(iucn_cat, length_m)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log10(length_m+1), fill = iucn_cat)) + 
    # geom_jitter(size = 0.3,alpha = 0.3) + 
    geom_violin(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size=16) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Size distribution of vessel detections",
         fill = "IUCN Category")+
    theme(legend.position = "bottom") )
  
  ggsave(size_iucn, file = "figures/size_iucn.jpg", width = 350, height = 210, units = "mm", dpi = 600)
  
  #Fraction of vessels inside MPAs
  fraction_vessels_in_EEZ_plot <- SAR_eez_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1",
                  normalized_detection = "normalized_detection_EEZ") %>%
    bind_rows(SAR_stats %>% mutate(type = "inside_mpa") %>% distinct(unique_id, .keep_all = T)) %>%
    #Group by type and parent_iso
    group_by(type, parent_iso) %>%
    reframe(sum_country = sum(normalized_detection)) %>%
    ungroup() %>%
    pivot_wider(names_from = "type",values_from="sum_country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(sum_country = inside_mpa + outside_mpa,
           fraction_country = inside_mpa/(outside_mpa+inside_mpa)) %>%
    arrange(-fraction_country) %>%
    filter(!parent_iso %in% c("FRA;ITA;MCO","NLD;DEU;DNK","SYC")) %>%
    slice(1:20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPAs", "outside_mpa"="Outside MPAs"))+
    labs(x = " ",
         y = "Number of detections inside EEZ",
         fill = "Fraction of fishing vessels detected: ") +
    my_custom_theme() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  figure_1 <- ggarrange( total_iucn , size_iucn, nrow = 2, common.legend = T, legend = "bottom",  align = "hv")
  
  # Save the fraction_vessels_in_EEZ_plot on one-third of the A4 portrait with some padding
  ggsave(fraction_vessels_in_EEZ_plot, 
         file = "figures/fraction_vessels_in_EEZ_plot.jpg",
         width = 210 * 1.5, 
         height = 99 * 1.5, 
         units = "mm", 
         dpi = 600,
         device = "jpeg",
         limitsize = FALSE,
         bg = "white")
  
  # Adjust the aspect ratio and save figure_1 on two-thirds of the A4 portrait
  ggsave(figure_1, 
         file = "figures/figure_1.jpg", 
         width = 210 * 1.5, 
         height = 198 *1.5, 
         units = "mm", 
         dpi = 300,
         device = "jpeg",
         limitsize = FALSE,
         bg = "white")
  
}
