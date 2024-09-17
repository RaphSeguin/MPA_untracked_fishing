figures_fishing_predictions_iucn <- function(mpa_model,
                                             fishing_presence_2022, fishing_presence_2023,
                                             fishing_hours_2022, fishing_hours_2023){
  
  #Object
  MPA_fishing <- mpa_model %>%
    st_drop_geometry()%>%
    left_join(fishing_presence_2022 %>% dplyr::select(id_iucn, fishing_presence_predicted_2022), by = "id_iucn") %>%
    left_join(fishing_presence_2023 %>% dplyr::select(id_iucn, fishing_presence_predicted_2023), by = "id_iucn") %>%
    left_join(fishing_hours_2022 %>% dplyr::select(id_iucn, predicted_fishing_effort_2022), by = "id_iucn") %>%
    left_join(fishing_hours_2023 %>% dplyr::select(id_iucn, predicted_fishing_effort_2023), by = "id_iucn") %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023,
           SAR_all = sum_all_2022 + sum_all_2023,
           SAR_matched_all = fishing_2022 + fishing_2023,
           presence_observed = as.factor(ifelse(fishing_presence_2022 == "Fishing" | fishing_presence_2023 == "Fishing", 
                                                "Fishing","No_fishing")),
           presence_predicted = as.factor(ifelse(fishing_presence_predicted_2022 == "Fishing" | fishing_presence_predicted_2023 == "Fishing", 
                                                 "Fishing","No_fishing")),
           predicted_fishing_all = predicted_fishing_effort_2022 + predicted_fishing_effort_2023)
  
  #Mpas with an increase in fishing
  IUCN_presence_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    group_by(iucn_cat) %>%
    reframe(predicted_fishing_all = sum(predicted_fishing_all),
            n_observed = sum(presence_observed == "Fishing"),
            n_predicted = sum(presence_predicted == "Fishing"),
            difference = n_predicted - n_observed,
            difference_percentage = difference/n_observed * 100) %>%
    ungroup() 
  
  #Raw
  raw_increase_presence <- IUCN_presence_increase %>%
    arrange(desc(difference)) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(iucn_cat,difference), difference)) + 
    geom_bar(stat = "identity", fill = "#384B6A") + 
    coord_flip() +
    labs(x = " ",
         y = "Additional number of MPAs with fishing") +
    my_custom_theme()
  
  #Percentage
  percentage_increase_presence <- IUCN_presence_increase %>%
    arrange(desc(difference_percentage)) %>%
    slice(1:10) %>%
    ggplot(aes(reorder(iucn_cat,difference_percentage), difference_percentage)) + 
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
    reframe(observed_fishing = sum(AIS_fishing_all)/1000,
            predicted_fishing = sum(predicted_fishing_all)/1000,
            difference_fishing = predicted_fishing - observed_fishing,
            percentage_increase = difference_fishing / observed_fishing * 100) %>%
    ungroup() %>%
    na.omit() %>%
    arrange(desc(difference_fishing)) %>%
    slice(1:10) 
  
  # Create the stacked bar plot
  raw_increase_fishing <- ggplot(IUCN_increase, aes(x = reorder(iucn_cat,difference_fishing), y = difference_fishing, fill = type)) +
    geom_bar(stat = "identity",  fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = " ", y = "Predicted increase in fishing effort (in thousands of hours)", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom")
  
  #Express as percentage of increase
  IUCN_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    group_by(iucn_cat) %>%
    reframe(observed_fishing = sum(AIS_fishing_all),
            predicted_fishing = sum(predicted_fishing_all),
            difference_fishing = predicted_fishing - observed_fishing,
            percentage_increase = difference_fishing / observed_fishing * 100) %>%
    ungroup() %>%
    filter(predicted_fishing > 1000) %>%
    na.omit() %>%
    arrange(desc(percentage_increase)) %>%
    slice(1:10) 
  
  # Create the stacked bar plot
  percentage_increase_fishing <- ggplot(IUCN_increase, aes(x = reorder(iucn_cat, percentage_increase), y = percentage_increase)) +
    geom_bar(stat = "identity", fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = "", y = "Percentage increase in fishing effort", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 11)) +  # Reduce the size of x-axis labels
    ylim(0, 100)
  
  IUCN_fishing_increase <- ggarrange(raw_increase_fishing,percentage_increase_fishing, nrow = 2 )
  
  ggsave(IUCN_fishing_increase, file = "figures/supp/IUCN_fishing_increase.jpg", width = 210, height = 297, units = "mm", dpi = 300)
  
  
  
}
