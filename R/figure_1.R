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
  
  (unmatched_time_plot <- SAR_stats_time %>%
      distinct(year,week, unmatched_fishing,matched_fishing,sum_all) %>%
      filter(week != "2021-12-30") %>%
      # Coalescing columns
      mutate(week = as.Date(week)) %>%
      group_by(week) %>%
      summarise(unmatched_fishing= last(na.omit(unmatched_fishing)), 
                matched_fishing = last(na.omit(matched_fishing))) %>%
      na.omit() %>%
      ggplot() + 
      #Mean number of unmatched vessels
      geom_hline(yintercept = median(SAR_stats_time$unmatched_fishing,na.rm=T),color="orange", size=.5,alpha=0.5) +
      geom_hline(yintercept = median(SAR_stats_time$matched_fishing,na.rm=T),color="darkblue", size=.5,alpha = 0.5) +
      geom_line(aes(week,unmatched_fishing,color = "Unmatched fishing vessels"),na.rm =T) + 
      geom_line(aes(week, matched_fishing, color = "Matched fishing vessels"),alpha = 0.5,size = 0.6) +
      scale_color_manual(values = colors) +
      theme_minimal(base_size = 16)+
      labs(title = "A",
           x = "Time",
           y = "Sum of fishing vessels (12 days period)",
           colour = "Vessel type") +
    theme(legend.position = "top")+
      ylim(0,3000))
  
  ggsave(unmatched_time_plot, file = "figures/unmatched_time_plot.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #plotting total number of  fishing vessels in MPAs against IUCN Category
  (total_iucn <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log10(relative_sum_all), fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size=16) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "B",
         x = " ",
         y = "Density of fishing vessels in MPAs",
         fill = "IUCN Category")+
    theme(legend.position = "bottom") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))
  
  ggsave(total_iucn, file = "figures/total_iucn.jpg", width = 350, height = 210, units = "mm", dpi = 600)
  
  #Test stat
  test_stat <-  SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat))
  
  #Test
  res.kruskal <- test_stat %>% kruskal_test(relative_sum_all ~ iucn_cat)
  res.kruskal
  
  pwc <-test_stat %>% wilcox_test(relative_sum_all ~ iucn_cat, p.adjust.method = "bonferroni")
  
  write.csv(pwc, file = "output/pwc.csv")
  
  #plotting fishing vessel size
  (size_iucn <- SAR_stats %>%
    bind_rows(SAR_eez_final) %>%
    distinct(unique_id, .keep_all = T) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log10(length_m+1), fill = iucn_cat)) + 
    # geom_jitter(size = 0.3,alpha = 0.3) + 
    geom_violin(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme_minimal(base_size=16) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "C",
         x = " ",
         y = "Size distribution of vessels in MPAs",
         fill = "IUCN Category")+
    theme(legend.position = "bottom") )
  
  ggsave(size_iucn, file = "figures/size_iucn.jpg", width = 350, height = 210, units = "mm", dpi = 600)
  
  #Test stat on size
  test_stat_size <-  SAR_stats %>%
    filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
    bind_rows(SAR_eez_final) %>% 
    st_drop_geometry() %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::select(length_m, iucn_cat)
    
  #Test
  res.kruskal_size <- test_stat_size %>% kruskal_test(length_m ~ iucn_cat)
  res.kruskal_size
  
  pwc_size <-test_stat_size %>% wilcox_test(length_m ~ iucn_cat, p.adjust.method = "bonferroni")
  pwc_size
  
  ggsave(size_iucn, file = "figures/size_iucn.jpg", width = 297, height = 140, units = "mm", dpi = 300)
  
  
  figure_1 <- ggarrange( total_iucn , size_iucn, nrow = 2, common.legend = T, legend = "bottom",  align = "hv")
  ggsave(figure_1, file = "figures/figure_1.jpg", width = 297, height = 297, units = "mm", dpi = 300)
  
}
