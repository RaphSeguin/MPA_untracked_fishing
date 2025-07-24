#' Generate and Save Figures for Fishing Predictions by IUCN Category
#'
#' This function processes fishing presence and effort predictions across multiple years, 
#' computes fishing effort changes for **IUCN categories**, and generates visualizations 
#' of fishing effort increases.
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
#' - `figures/supp/IUCN_MPA_increase.jpg`: Increase in MPAs with fishing, grouped by IUCN category.
#' - `figures/supp/IUCN_fishing_increase.jpg`: Predicted increase in fishing effort, grouped by IUCN category.
#'
#' @details
#' 1. **Merges fishing presence and effort data** into `MPA_fishing`.
#' 2. **Computes summary statistics**:
#'    - Total fishing effort (`AIS_fishing_all`).
#'    - Total SAR detections (`SAR_all`).
#'    - Predicted total fishing effort (`predicted_fishing_all`).
#' 3. **Calculates IUCN-level fishing effort increase**:
#'    - **MPAs with increased fishing** (`n_predicted - n_observed`).
#'    - **Percentage increase in MPAs with fishing**.
#' 4. **Creates bar plots**:
#'    - **Raw increase** in MPAs with fishing (`Top 20` IUCN categories).
#'    - **Percentage increase** in MPAs with fishing (`Top 10` IUCN categories).
#' 5. **Computes IUCN-level total fishing effort increase**:
#'    - **Difference between observed and predicted fishing effort**.
#'    - **Expresses increase as a percentage**.
#' 6. **Saves all generated plots** as `.jpg` files.
#'
#' @examples
#' figures_fishing_predictions_iucn(mpa_model, fishing_presence_2022, fishing_presence_2023, 
#'                                  fishing_presence_2024, fishing_hours_2022, 
#'                                  fishing_hours_2023, fishing_hours_2024)
#'

figures_fishing_predictions_iucn <- function(mpa_model,
                                             fishing_presence_2022, fishing_presence_2023, fishing_presence_2024,
                                             fishing_hours_2022, fishing_hours_2023, fishing_hours_2024){
  
  # Object
  MPA_fishing <- mpa_model %>%
    st_drop_geometry()%>%
    left_join(fishing_presence_2022 %>% dplyr::select(id_iucn, fishing_presence_predicted_2022), by = "id_iucn") %>%
    left_join(fishing_presence_2023 %>% dplyr::select(id_iucn, fishing_presence_predicted_2023), by = "id_iucn") %>%
    left_join(fishing_presence_2024 %>% dplyr::select(id_iucn, fishing_presence_predicted_2024), by = "id_iucn") %>%
    left_join(fishing_hours_2022 %>% dplyr::select(id_iucn, predicted_fishing_effort_2022), by = "id_iucn") %>%
    left_join(fishing_hours_2023 %>% dplyr::select(id_iucn, predicted_fishing_effort_2023), by = "id_iucn") %>%
    left_join(fishing_hours_2024 %>% dplyr::select(id_iucn, predicted_fishing_effort_2024), by = "id_iucn") %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023 + AIS_fishing_2024,
           SAR_all = sum_all_2022 + sum_all_2023 + sum_all_2024,
           SAR_matched_all = fishing_2022 + fishing_2023 + fishing_2024,
           presence_observed = as.factor(ifelse(fishing_presence_2022 == "Fishing" | fishing_presence_2023 == "Fishing" | fishing_presence_2024 == "Fishing", 
                                                "Fishing","No_fishing")),
           presence_predicted = as.factor(ifelse(fishing_presence_predicted_2022 == "Fishing" | fishing_presence_predicted_2023 == "Fishing" | fishing_presence_predicted_2024 == "Fishing", 
                                                 "Fishing","No_fishing")),
           predicted_fishing_all = predicted_fishing_effort_2022 + predicted_fishing_effort_2023 + predicted_fishing_effort_2024) %>%
    mutate(iucn_cat = as.factor(iucn_cat),
           iucn_cat = ifelse(iucn_cat %in% c("Ia", "Ib"), "I", as.character(iucn_cat))) %>%
    mutate(iucn_cat = factor(iucn_cat, levels = unique(c("I", levels(mpa_wdpa$iucn_cat))))) 
  
  #Bar plot across categories
  iucn_raw_bar_pot <- MPA_fishing %>%
    mutate(iucn_cat = as.character(iucn_cat)) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I",iucn_cat)) %>%
    filter(iucn_cat %in% c("I","II","III","IV","V","VI")) %>%
    group_by(iucn_cat) %>%
    reframe(sum_fishing = sum(predicted_fishing_all,na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(factor(iucn_cat, level_order),sum_fishing)) + 
    geom_bar(stat = "identity",fill = "#384B6A") +
    my_custom_theme() +
    labs( x = "",
          y = "AIS and untracked predicted fishing hours")
  
  ggsave(iucn_raw_bar_pot, file = "figures/supp/iucn_raw_bar_pot.jpg")
  
  # MPAs with an increase in fishing
  IUCN_presence_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    group_by(iucn_cat) %>%
    reframe(predicted_fishing_all = sum(predicted_fishing_all),
            n_observed = sum(presence_observed == "Fishing"),
            n_predicted = sum(presence_predicted == "Fishing"),
            difference = n_predicted - n_observed,
            difference_percentage = difference / n_observed * 100) %>%
    ungroup() 
  
  # Raw Increase
  raw_increase_presence <- IUCN_presence_increase %>%
    arrange(desc(difference)) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(iucn_cat, difference), difference)) + 
    geom_bar(stat = "identity", fill = "#384B6A") + 
    coord_flip() +
    labs(x = " ",
         y = "Additional number of MPAs with fishing") +
    my_custom_theme()
  
  # Percentage Increase
  percentage_increase_presence <- IUCN_presence_increase %>%
    arrange(desc(difference_percentage)) %>%
    slice(1:10) %>%
    ggplot(aes(reorder(iucn_cat, difference_percentage), difference_percentage)) + 
    geom_bar(stat = "identity", fill = "#384B6A") + 
    coord_flip() +
    labs(x = " ",
         y = "Percentage increase in the number of MPAs with fishing") +
    my_custom_theme() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 11))
  
  IUCN_MPA_increase <- ggarrange(raw_increase_presence, percentage_increase_presence, nrow = 2)
  
  ggsave(IUCN_MPA_increase, file = "figures/supp/IUCN_MPA_increase.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  
  # Create the stacked bar plot
  IUCN_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    group_by(iucn_cat) %>%
    reframe(observed_fishing = sum(AIS_fishing_all) / 1000,
            predicted_fishing = sum(predicted_fishing_all) / 1000,
            difference_fishing = predicted_fishing - observed_fishing,
            percentage_increase = difference_fishing / observed_fishing * 100) %>%
    ungroup() %>%
    na.omit() %>%
    arrange(desc(difference_fishing)) %>%
    slice(1:10)
  
  # Create the stacked bar plot
  raw_increase_fishing <- ggplot(IUCN_increase, aes(x = reorder(iucn_cat, difference_fishing), y = difference_fishing, fill = type)) +
    geom_bar(stat = "identity", fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = " ", y = "Predicted increase in fishing effort (in thousands of hours)", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom")
  
  ggsave(percentage_increase_fishing, file = "figures/supp/percentage_increase_fishing.svg", width = 297, height = 210, units = "mm", dpi = 300)
}
