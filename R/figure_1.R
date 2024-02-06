figure_1 <- function(SAR_stats){
  
  level_order <- c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ") 
  legend = c("I" = "#051D41",
             "II" = "#092C63",
             # "III" = "#0E58C6",
             "IV" = "#0E58C6",
             "V" = "#5090EF",
             "VI"= "#93BAF8",
             "Not Applicable" = "#F29F79",
             "Not Assigned" = "#EF8B5B",
             "Not Reported" = "#D87458",
             "EEZ" = "#9B3742")
  
  #plotting total number of  fishing vessels in MPAs against IUCN Category
  total_iucn <- SAR_stats %>%
    distinct(id, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log10(relative_sum_all), fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size=14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "A",
         x = " ",
         y = "Density of detected fishing vessels in MPAs (log scale)",
         fill = "IUCN Category")+
    theme(legend.position = "bottom")
  
  #plotting fishing vessel size
  size_iucn <- SAR_stats %>%
    distinct(id, .keep_all = T) %>%
    bind_rows(SAR_eez_final) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log10(length_m+1), fill = iucn_cat)) + 
    # geom_jitter(size = 0.3,alpha = 0.3) + 
    geom_violin(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size=14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "B",
         x = " ",
         y = "Size distribution of fishing vessels in MPAs (log scale)",
         fill = "IUCN Category")+
    theme(legend.position = "bottom") 
  
  figure_1 <- ggarrange(total_iucn, size_iucn, nrow = 2, common.legend = T)
  ggsave(figure_1, file = "figures/figure_1.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  
  }