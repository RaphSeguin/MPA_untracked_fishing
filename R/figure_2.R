#' Generate and Save Figure 2: Fishing Detections and Tracking Analysis
#'
#' This function creates **Figure 2**, which includes multiple visualizations related to **fishing vessel detections, tracking, and SAR coverage** inside and outside MPAs.
#'
#' @param SAR_stats A dataframe containing **SAR-based vessel detection statistics**.
#'
#' @return Saves the following outputs:
#' - `figures/total_iucn.svg`: **Boxplot of vessel detection density** by IUCN category.
#' - `figures/ratio_iucn.svg`: **Boxplot of untracked vessel detections (%)** by IUCN category.
#' - `figures/matched_unmatched_cor.svg`: **Scatter plot comparing tracked vs. untracked detections**.
#' - `figures/fig1_top.jpg`: Combined **top section of Figure 2** (detection density & untracked detections).
#' - `figures/fig1_bottom.jpg`: Combined **bottom section of Figure 2** (EEZ vs. MPA detections & tracked vs. untracked).
#'
#' @details
#' 1. **Figure 2A**: **Boxplot of vessel detection density** (`log-scale`).
#'    - Compares **SAR vessel detection density** across IUCN categories.
#'    - Uses a **custom color scheme** for IUCN categories.
#' 2. **Figure 2B**: **Bar plot of vessel detections inside vs. outside MPAs**.
#'    - Shows **top 20 EEZs** with the highest fraction of detections **inside MPAs**.
#' 3. **Figure 2C**: **Boxplot of untracked vessel detections (%)** by IUCN category.
#'    - Displays the **proportion of untracked fishing vessels** per category.
#' 4. **Figure 2D**: **Scatter plot of matched vs. unmatched detections**.
#'    - Compares **tracked vs. untracked fishing detections** using a **log-log plot**.
#'    - **Bubble size** represents MPA area.
#' 5. **Combines figures into panels**:
#'    - **Top panel**: Figure 2A (detection density) & Figure 2C (untracked %).
#'    - **Bottom panel**: Figure 2B (inside vs. outside MPA detections) & Figure 2D (tracked vs. untracked).
#' 6. **Saves all figures as `.svg` and `.jpg` formats** for publication-quality output.
#'
#' @examples
#' figure_2(SAR_stats)
#'

figure_2 <- function(SAR_stats){
  
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
  
  # Calculate spatially weighted mean for MPA_final_vars
  weighted_mean_mpa <- MPA_final_vars %>%
    group_by(iucn_cat) %>%
    summarise(weighted_mean = weighted.mean(SAR_density, w = area_correct, na.rm = TRUE))  # Use area_correct for MPA_final_vars
  
  # Calculate spatially weighted mean for EEZ_final_vars
  weighted_mean_eez <- EEZ_final_vars %>%
    group_by(iucn_cat) %>%
    summarise(weighted_mean = weighted.mean(SAR_density, w = eez_area, na.rm = TRUE))  # Use eez_area for EEZ_final_vars
  
  # Combine the two datasets for the weighted means
  weighted_mean <- bind_rows(weighted_mean_mpa, weighted_mean_eez)
  
  # Add the spatially weighted mean as a red dot to the boxplot
  (total_iucn <- MPA_final_vars %>%
      bind_rows(EEZ_final_vars) %>%
      # filter(SAR_density > 0) %>%
      ggplot(aes(factor(iucn_cat, level = level_order), log(SAR_density), fill = iucn_cat)) + 
      scale_fill_manual(values = legend,
                        breaks = c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"),
                        guide = "none") + 
      geom_jitter(alpha = 0.4, shape = ".") +
      geom_boxplot(alpha = 0.7) +
      # geom_point(data = weighted_mean, aes(x = factor(iucn_cat, level = level_order),
      #                                      y = log(weighted_mean)), color = "red", size = 3) +  # Add red dot
      labs(title = "A",
           x = " ",
           y = "Density of vessel detections (log scale)",
           fill = "IUCN Category") +
      my_custom_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0, face = "bold", size = 25)))
  
  ggsave(total_iucn, file = "figures/total_iucn_sum_proba.jpg",
         width = 18.3* 2 , 
         height = 8.6 * 2 ,
         units = "cm")
  
  ratio_iucn <- MPA_final_vars %>%
    filter(!is.na(unmatched_ratio))
  
  truc <- ratio_iucn %>% filter(iucn_cat == "I")
  
  #Fig 2C
  (ratio_iucn <- MPA_final_vars %>%
      bind_rows(EEZ_final_vars) %>%
      ggplot(aes(factor(iucn_cat,level = level_order), unmatched_ratio * 100, fill = iucn_cat)) + 
      geom_jitter(size = 0.1,alpha = 0.2) + 
      geom_boxplot(alpha = 0.7) +

      scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = "C",
           x = " ",
           y = "Percentage of untracked vessel detections (%)",
           fill = "IUCN Category")+
      my_custom_theme() +
      theme(legend.position = "bottom")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme(plot.title = element_text(hjust = 0, face = "bold", size = 25)))
  
  ggsave(ratio_iucn, file = "figures/ratio_iucn_sum_proba.jpg",
         width = 18.3 *2, 
         height = 8.6 * 2 ,
         units = "cm")
  
  #Fig 2D
  #Matched vs unmatched cor
  (matched_unmatched_cor <- MPA_final_vars %>% 
      filter(sum_all > 0) %>%
    ggplot(aes(log(fishing), log(unmatched_fishing),size = area_correct,fill = iucn_cat)) +
    geom_point(alpha = 0.5,shape=21, color="black") +
    scale_size(range = c(.5, 20), name="MPA area (km2)", guide = "none") + 
    scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"),
                      guide = "none") + 
    my_custom_theme() +
    theme(legend.position="none") +
    labs(title = "C",
         x = "Sum of tracked vessel detections (log scale)",
         y = "Sum of untracked vessel detections (log scale)",
         fill = "IUCN category") +
    guides(fill=guide_legend(nrow=4,byrow=TRUE))+
      theme(plot.title = element_text(hjust = 0, face = "bold", size = 25)))
    
  ggsave(matched_unmatched_cor, file = "figures/matched_unmatched_cor_sum_proba.jpg",
         width = 18.3 *2, 
         height = 8.6 * 2 ,
         units = "cm")
  
  #First part of fig 1
  fig1_top <- ggarrange(total_iucn, ratio_iucn, nrow = 1, common.legend = T, legend = "bottom")
  
  ggsave(fig1_top, file = "figures/fig1_top.jpg",
         width = 148.5 *4, 
         height = 105*2,
         units = "mm", dpi = 300)
  
  fig1_bottom <- ggarrange(fraction_vessels_in_EEZ_plot, matched_unmatched_cor, nrow = 1)
  
  ggsave(fig1_bottom, file = "figures/fig1_bottom.jpg",
         width = 148.5 *4, 
         height = 105*2,
         units = "mm", dpi = 300)
  
  
  
}
