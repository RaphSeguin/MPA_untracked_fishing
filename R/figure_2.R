figure_2 <- function(SAR_stats){
  
  #Matched vs unmatched cor
  (matched_unmatched_cor <- MPA_final_vars %>% 
    ggplot(aes(log(fishing), log(unmatched_fishing),size = area_correct,fill = iucn_cat)) +
    geom_point(alpha = 0.5,shape=21, color="black") +
    scale_size(range = c(.5, 20), name="MPA area (km2)", guide = "none") + 
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"),
                      guide = "none") + 
    theme_minimal(base_size = 14) +
    theme(legend.position="none") +
    labs(x = "Sum of matched vessel detections",
         y = "Sum of unmatched vessel detections",
         fill = "IUCN category") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE)) )
  
  ggsave(matched_unmatched_cor, file = "figures/matched_unmatched_cor.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Plotting density of unmatched and ratio
  (unmatched_iucn <- MPA_final_vars %>%
    bind_rows(EEZ_final_vars) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log(unmatched_density), fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    labs(x = " ",
         y = "Density of unmatched vessel detections",
         fill = "IUCN Category")+
    theme_minimal(base_size = 14) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position="top") )
  
  # ggsave(figure_2, file = "figures/figure_2.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  (ratio_iucn <- MPA_final_vars %>%
    bind_rows(EEZ_final_vars) %>%
    filter(iucn_cat != "III") %>%
    ggplot(aes(factor(iucn_cat,level = level_order), unmatched_ratio, fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Percentage of unmatched vessel detections",
         fill = "IUCN Category")+
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom"))
  
  figure_2 <- ggarrange(unmatched_iucn,ratio_iucn, nrow = 2, common.legend = T)
  
  # Save the first plot (matched_unmatched_cor) to fit one-third of an A4 page
  ggsave(matched_unmatched_cor, 
         file = "figures/matched_unmatched_cor.jpg", 
         width = 210 * 1.5,  # A4 width in mm
         height = 99 * 1.5,  # One-third of A4 height
         units = "mm", 
         dpi = 300)
  
  # Save the second plot (figure_2) to fit two-thirds of an A4 page
  ggsave(figure_2, 
         file = "figures/figure_2.jpg", 
         width = 210 * 1.5,  # A4 width in mm
         height = 198 * 1.5,  # Two-thirds of A4 height
         units = "mm", 
         dpi = 300)
  
}
