figure_2 <- function(SAR_stats){
  
  #Data with unique info for each considered MPA
  MPA_data <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) 
  
  #Matched vs unmatched cor
  (matched_unmatched_cor <- MPA_data %>% 
    ggplot(aes(log(matched_fishing), log(unmatched_fishing),size = gis_m_area,fill = iucn_cat)) +
    geom_point(alpha = 0.5,shape=21, color="black") +
    scale_size(range = c(.5, 20), name="MPA area (km2)") + 
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size = 14) +
    theme(legend.position="bottom") +
    labs(title = "A",
         x = "Sum of matched fishing vessels",
         y = "Sum of unmatched fishing vessels",
         fill = "IUCN category") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE)) )
  
  ggsave(matched_unmatched_cor, file = "figures/matched_unmatched_cor.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Plotting density of unmatched and ratio
  (unmatched_iucn <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log(unmatched_relative), fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    labs(title = "B",
         x = " ",
         y = "Density of unmatched fishing vessels (log scale)",
         fill = "IUCN Category")+
    theme_minimal(base_size = 14) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position="bottom") )
  
  ggsave(unmatched_iucn, file = "figures/unmatched_iucn.jpg", width = 297, height = 120, units = "mm", dpi = 300)
  
  
  
  # ggsave(figure_2, file = "figures/figure_2.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  (ratio_iucn <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), unmatched_ratio, fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title ="C",
         x = " ",
         y = "Fraction of unmatched fishing vessels (log scale)",
         fill = "IUCN Category")+
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom"))
  
 
  
  ggsave(ratio_iucn, file = "figures/ratio_iucn.jpg", width = 297, height = 140, units = "mm", dpi = 300)
  
  figure_2 <- ggarrange(unmatched_iucn,ratio_iucn, nrow = 2, common.legend = T)
  ggsave(figure_2, file = "figures/figure_2.jpg", width = 297, height = 297, units = "mm", dpi = 300)
  
  
  
}
