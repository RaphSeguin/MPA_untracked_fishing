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
  
  #Fig 2A
  (total_iucn <- MPA_final_vars %>%
      bind_rows(EEZ_final_vars) %>%
      filter(SAR_density > 0) %>%
      ggplot(aes(factor(iucn_cat,level = level_order), log(SAR_density), fill = iucn_cat)) + 
      scale_fill_manual(values = legend,
                        breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"),
                        guide = "none") + 
      geom_jitter(alpha = 0.4, shape = ".") +
      geom_boxplot(alpha = 0.7) +
      labs(title = "A",
           x = " ",
           y = "Density of vessel detections (log scale)",
           fill = "IUCN Category") +
      my_custom_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(plot.title = element_text(hjust = 0, face = "bold", size = 25)))
  
  ggsave(total_iucn, file = "figures/total_iucn.svg",
         width = 18.3* 2 , 
         height = 8.6 * 2 ,
         units = "cm")
  
  #Fig 2B
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
    filter(sum_country > 1) %>%
    arrange(-fraction_country) %>%
    filter(!parent_iso %in% c("FRA;ITA;MCO","NLD;DEU;DNK","SYC")) %>%
    slice(1:20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPAs", "outside_mpa"="Outside MPAs"))+
    labs(title = "B",
         x = " ",
         y = "Number of detections inside EEZ",
         fill = "Fraction of fishing vessels detected: ") +
    my_custom_theme() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(plot.title = element_text(hjust = 0, face = "bold", size = 25))
  
  
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
  
  ggsave(ratio_iucn, file = "figures/ratio_iucn.svg",
         width = 18.3 *2, 
         height = 8.6 * 2 ,
         units = "cm")
  
  #Fig 2D
  #Matched vs unmatched cor
  (matched_unmatched_cor <- MPA_final_vars %>% 
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
    
  ggsave(matched_unmatched_cor, file = "figures/matched_unmatched_cor.svg",
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
