#' Plot and Analyze Fishing Effort Density in MPAs
#'
#' This function processes fishing presence and effort predictions across multiple years, 
#' computes fishing effort density, and generates visualizations of fishing density 
#' across **IUCN categories** and **countries**.
#'
#' @param mpa_model A dataframe containing MPA data with environmental and socioeconomic covariates.
#' @param fishing_presence_2022 A dataframe with predicted fishing presence for **2022**.
#' @param fishing_presence_2023 A dataframe with predicted fishing presence for **2023**.
#' @param fishing_presence_2024 A dataframe with predicted fishing presence for **2024**.
#' @param fishing_hours_2022 A dataframe with predicted fishing effort for **2022**.
#' @param fishing_hours_2023 A dataframe with predicted fishing effort for **2023**.
#' @param fishing_hours_2024 A dataframe with predicted fishing effort for **2024**.
#'
#' @return Saves the following outputs:
#' - `figures/density_by_iucn.jpg`: Boxplot of fishing density by IUCN category.
#' - `figures/density_by_country.jpg`: Bar plot of top 20 countries with highest fishing density.
#' - `figures/supp/EEZ_MPA_stat_test.csv`: Dunn’s test results for density comparisons between IUCN categories.
#'
#' @details
#' 1. **Computes fishing effort density** for AIS and predicted values (`hours/km²`).
#' 2. **Calculates average density over three years** for each MPA.
#' 3. **Visualizes density distribution by IUCN category**:
#'    - Boxplots and jittered points for variance.
#'    - Custom color scale for IUCN categories.
#' 4. **Computes country-level fishing density**:
#'    - Filters countries with at least 10 MPAs.
#'    - Extracts the top 20 countries with highest density.
#'    - Creates a **bar plot of fishing density by country**.
#' 5. **Performs statistical comparisons**:
#'    - **Dunn’s test** for fishing density differences across IUCN categories.
#'    - Applies Bonferroni correction for multiple comparisons.
#' 6. **Saves visualizations and statistical results**.
#'
#' @examples
#' plot_fishing_density(mpa_model, fishing_presence_2022, fishing_presence_2023, 
#'                      fishing_presence_2024, fishing_hours_2022, 
#'                      fishing_hours_2023, fishing_hours_2024)
#'


plot_fishing_density <- function(mpa_model,
                                 fishing_presence_2022, fishing_presence_2023, fishing_presence_2024,
                                 fishing_hours_2022, fishing_hours_2023, fishing_hours_2024){
  
  # Prediction
  MPA_fishing <- mpa_model %>%
    # Add correct size
    dplyr::select(-area_correct) %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, area_correct) %>% st_drop_geometry(), by = "id_iucn") %>%
    st_drop_geometry()%>%
    left_join(fishing_presence_2022 %>% dplyr::select(id_iucn, fishing_presence_predicted_2022), by = "id_iucn") %>%
    left_join(fishing_presence_2023 %>% dplyr::select(id_iucn, fishing_presence_predicted_2023), by = "id_iucn") %>%
    left_join(fishing_presence_2024 %>% dplyr::select(id_iucn, fishing_presence_predicted_2024), by = "id_iucn") %>%
    left_join(fishing_hours_2022 %>% dplyr::select(id_iucn, predicted_fishing_effort_2022), by = "id_iucn") %>%
    left_join(fishing_hours_2023 %>% dplyr::select(id_iucn, predicted_fishing_effort_2023), by = "id_iucn") %>%
    left_join(fishing_hours_2024 %>% dplyr::select(id_iucn, predicted_fishing_effort_2024), by = "id_iucn") %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023 + AIS_fishing_2024,
           SAR_all = sum_all_2022 + sum_all_2023 + sum_all_2024,
           SAR_unmatched_all = unmatched_fishing_2022 + unmatched_fishing_2023 + unmatched_fishing_2024, 
           SAR_matched_all = fishing_2022 + fishing_2023 + fishing_2024,
           presence_observed = as.factor(ifelse(fishing_presence_2022 == "Fishing" | fishing_presence_2023 == "Fishing" | fishing_presence_2024 == "Fishing", 
                                                "Fishing","No_fishing")),
           presence_predicted = as.factor(ifelse(fishing_presence_predicted_2022 == "Fishing" | fishing_presence_predicted_2023 == "Fishing" | fishing_presence_predicted_2024 == "Fishing", 
                                                 "Fishing","No_fishing")),
           predicted_fishing_all = predicted_fishing_effort_2022 + predicted_fishing_effort_2023 + predicted_fishing_effort_2024) %>%
    mutate(iucn_cat = as.factor(iucn_cat),
           iucn_cat = ifelse(iucn_cat %in% c("Ia", "Ib"), "I", as.character(iucn_cat))) %>%
    mutate(iucn_cat = factor(iucn_cat, levels = unique(c("I", levels(mpa_wdpa$iucn_cat))))) %>%
    # Add density
    mutate(AIS_fishing_2022_density = AIS_fishing_2022 / area_correct,
           AIS_fishing_2023_density = AIS_fishing_2023 / area_correct,
           AIS_fishing_2024_density = AIS_fishing_2024 / area_correct,
           predicted_fishing_effort_2022_density = predicted_fishing_effort_2022 / area_correct,
           predicted_fishing_effort_2023_density = predicted_fishing_effort_2023 / area_correct,
           predicted_fishing_effort_2024_density = predicted_fishing_effort_2024 / area_correct) %>%
    # Average density over 3 years
    mutate(AIS_average_density = rowMeans(dplyr::select(., AIS_fishing_2022_density, AIS_fishing_2023_density, AIS_fishing_2024_density), na.rm = TRUE),
           predicted_average_density = rowMeans(dplyr::select(., predicted_fishing_effort_2022_density, 
                                                              predicted_fishing_effort_2023_density, predicted_fishing_effort_2024_density), na.rm = TRUE)) %>%
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
  
  ggsave(density_by_iucn,
         file = "figures/density_by_iucn.svg", 
         width = 18.3*2 , height =  8.6 * 2 , units = "cm")
  
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
  
  write.csv(EEZ_MPA_stat_test, file = "figures/supp/EEZ_MPA_stat_test.csv")
  
}
