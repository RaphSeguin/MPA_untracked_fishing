figure_2 <- function(SAR_stats){
  
  
  #Plotting number of unmatched vessels in MPA according to time
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
     geom_hline(yintercept = median(SAR_stats_time$unmatched_fishing,na.rm=T),color="orange", size=.5) +
     geom_hline(yintercept = median(SAR_stats_time$matched_fishing,na.rm=T),color="darkblue", size=.5,alpha = 0.3) +
     geom_line(aes(week,unmatched_fishing),color = "black") +
     geom_line(aes(week, matched_fishing), color = "darkblue",alpha = 0.3) +
      theme_minimal()+
      labs(title = "A",
           x = "Time",
           y = "Sum of fishing vessels (12 days period)")) +
    ylim(0,3000)
  
  #Plotting density of unmatched and ratio
  unmatched_iucn <- SAR_stats %>%
    distinct(id, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log(unmatched_relative), fill = iucn_cat)) + 
    geom_jitter(size = 0.1,alpha = 0.2) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = "B",
         x = " ",
         y = "Density of unmatched fishing vessels (log scale)",
         fill = "IUCN Category")+
    theme(legend.position = "bottom")+
    theme_minimal()
  
  ratio_iucn <- SAR_stats %>%
    distinct(id, .keep_all = T) %>%
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
    theme(legend.position = "bottom")+
    theme_minimal()
  
  figure_2 <- ggarrange(unmatched_time_plot,unmatched_iucn, ratio_iucn, nrow = 3, common.legend = T)
  ggsave(figure_2, file = "figures/figure_2.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  
  
  
}
