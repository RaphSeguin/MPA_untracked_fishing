#' Generate and Save Figures for Fishing Predictions
#'
#' This function processes fishing presence and effort predictions across multiple years, 
#' generates summary statistics, and creates various visualizations related to fishing activity in Marine Protected Areas (MPAs).
#'
#' @param mpa_model A dataframe containing MPA data with environmental and socioeconomic covariates.
#' @param fishing_presence_2022 A dataframe with predicted fishing presence for **2022**.
#' @param fishing_presence_2023 A dataframe with predicted fishing presence for **2023**.
#' @param fishing_presence_2024 A dataframe with predicted fishing presence for **2024**.
#' @param fishing_hours_2022 A dataframe with predicted fishing effort for **2022**.
#' @param fishing_hours_2023 A dataframe with predicted fishing effort for **2023**.
#' @param fishing_hours_2024 A dataframe with predicted fishing effort for **2024**.
#'
#' @return Saves the following figures:
#' - `figures/correlation.jpg`: Correlation between SAR detections and fishing effort.
#' - `figures/percentage_increase_full.jpg`: Increase in fishing effort across MPAs by country.
#' - `figures/predicted_fishing_map.jpg`: Global fishing effort predictions on a cartogram.
#'
#' @details
#' 1. **Merges fishing presence and fishing effort data** into `MPA_fishing`.
#' 2. **Creates summary variables**:
#'    - Total AIS fishing effort (`AIS_fishing_all`).
#'    - Total SAR detections (`SAR_all`).
#'    - Combined predicted fishing effort.
#' 3. **Generates correlation plots** comparing tracked SAR fishing and predicted fishing effort.
#' 4. **Computes country-level increases in fishing presence and effort**.
#' 5. **Creates bar plots**:
#'    - **Raw increase** in MPAs with fishing.
#'    - **Percentage increase** in MPAs with fishing.
#' 6. **Constructs a global map using a Dorling cartogram**:
#'    - Displays fishing effort increases by region.
#' 7. **Saves all generated plots** as `.jpg` and `.svg` files.
#'
#' @examples
#' figures_fishing_predictions(mpa_model, fishing_presence_2022, fishing_presence_2023, 
#'                             fishing_presence_2024, fishing_hours_2022, 
#'                             fishing_hours_2023, fishing_hours_2024)
#'

figures_fishing_predictions <- function(mpa_model,
                                        fishing_presence_2022, fishing_presence_2023, fishing_presence_2024,
                                        fishing_hours_2022, fishing_hours_2023, fishing_hours_2024){
  
  MPA_fishing <- mpa_model %>%
    st_drop_geometry() %>%
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
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I", iucn_cat)) 
  
  # Correlation plot
  correlation <- MPA_fishing %>%
    filter(fishing_presence_predicted_2022 == "Fishing" | fishing_presence_predicted_2023 == "Fishing" | fishing_presence_predicted_2024 == "Fishing") %>%
    filter(SAR_all > 0) %>%
    ggplot() +
    geom_point(aes(x = log(SAR_matched_all), y = log(AIS_fishing_all), color = "Tracked vs AIS"), 
               alpha = 0.5, size = 1) +
    geom_smooth(aes(x = log(SAR_matched_all), y = log(AIS_fishing_all), color = "Tracked vs AIS"), 
                method = "lm", se = F, size = 1.5) +
    
    geom_point(aes(x = log(SAR_all), y = log(predicted_fishing_all), color = "Tracked and untracked vs Predicted"), 
               alpha = 0.5, size = 1) +
    geom_smooth(aes(x = log(SAR_all), y = log(predicted_fishing_all), color = "Tracked and untracked vs Predicted"), 
                method = "lm", se = F, size = 1.5) +
    scale_color_manual(values = c("Tracked vs AIS" = "#E28A2A", "Tracked and untracked vs Predicted" = "#384B6A")) +
    labs(x = "Vessel detections (log-scale)", y = "Fishing effort - log(hours)", 
         color = "Type") +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 16)
    ) +
    theme(panel.grid.major = element_line(color = "gray80", size = 0.5),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "gray70"))
  
  # Save figures
  ggsave("figures/correlation_plot.jpg", plot = correlation, width = 148.5 * 1.5 , height = 105 * 1.5, units = "mm", dpi = 600)
  
  ggsave("figures/correlation.svg", plot = correlation, width = 18.3 , height =  8.6 * 2, units = "cm")
  
  #Mpas with an increase in fishing
  MPA_presence_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn, country), by = "id_iucn") %>%
    group_by(country) %>%
    reframe(predicted_fishing_all = sum(predicted_fishing_all),
            n_observed = sum(presence_observed == "Fishing"),
            n_predicted = sum(presence_predicted == "Fishing"),
            difference = n_predicted - n_observed,
            difference_percentage = difference/n_observed * 100) %>%
    ungroup() %>%
    filter(predicted_fishing_all > 500)
  
  #Raw
  raw_increase_presence <- MPA_presence_increase %>%
    arrange(desc(difference)) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(country,difference), difference)) + 
    geom_bar(stat = "identity", fill = "#384B6A") + 
    coord_flip() +
    labs(x = " ",
         y = "Additional number of MPAs with fishing") +
    my_custom_theme() 
  
  #Percentage
  percentage_increase_presence <- MPA_presence_increase %>%
    arrange(desc(difference_percentage)) %>%
    slice(1:10) %>%
    ggplot(aes(reorder(country,difference_percentage), difference_percentage)) + 
    geom_bar(stat = "identity", fill = "#384B6A") + 
    coord_flip() +
    labs(x = " ",
         y = "Percentage increase in the number of MPAs with fishing") +
    my_custom_theme() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 11))
  
  # Countries increase fishing
  country_increase <- MPA_fishing %>%
    left_join(MPA_final_vars %>% dplyr::select(id_iucn, country), by = "id_iucn") %>%
    group_by(country) %>%
    reframe(observed_fishing = sum(AIS_fishing_all)/1000,
            predicted_fishing = sum(predicted_fishing_all)/1000,
            difference_fishing = predicted_fishing - observed_fishing,
            percentage_increase = difference_fishing / observed_fishing * 100) %>%
    filter(observed_fishing > 50) %>%
    ungroup() %>%
    na.omit() 
  
  # Create the stacked bar plot
  raw_increase_fishing <- country_increase %>%
    arrange(desc(difference_fishing)) %>%
    slice(1:10) %>%
    ggplot(aes(x = reorder(country,difference_fishing), y = difference_fishing, fill = type)) +
    geom_bar(stat = "identity",  fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = " ", y = "Predicted increase in fishing effort (in thousands of hours)", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom")
  
  # Create the stacked bar plot
  percentage_increase_fishing <- country_increase %>%
    arrange(desc(percentage_increase)) %>%
    slice(1:10)  %>%
    ggplot(aes(x = reorder(country, percentage_increase), y = percentage_increase)) +
    geom_bar(stat = "identity", fill = "#384B6A", size = 0.2, position = "stack") +
    coord_flip() +  # Flip coordinates for horizontal bars
    labs(x = "", y = "Percentage increase in fishing effort", 
         fill = "Type") +
    my_custom_theme() +
    theme(legend.position = "bottom")
  
  #FULL FIRST PART
  percentage_increase_full <- ggarrange(raw_increase_fishing, percentage_increase_fishing, nrow = 2, ncol = 1,
                                        align = "v")
  
  ggsave(percentage_increase_full,
         file = "figures/percentage_increase_full.jpg",
         width = 210,  # A4 width in mm
         height = 297,  # Half of A4 height in mm
         units = "mm", 
         dpi = 300)
  
  ggsave(percentage_increase_full,
         file = "figures/percentage_increase_full.svg",  width = 18.3 , height =  8.6 * 2, units = "cm")

  #GLOBAL MAP OF PERCENTAGE INCREASE
  MPA_LME <- MPA_fishing %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    st_as_sf() %>%
    st_centroid() %>%
    #Join with LME
    st_join(LME %>% dplyr::select(LME_NAME), join = st_nearest_feature) %>%
    group_by(LME_NAME) %>%
    reframe(observed_fishing = sum(AIS_fishing_all),
            predicted_fishing = sum(predicted_fishing_all),
            difference_fishing = predicted_fishing - observed_fishing,
            percentage_increase = difference_fishing / observed_fishing * 100,
            predicted_fishing_log = sqrt(predicted_fishing)) %>%
    ungroup() %>%
    filter(predicted_fishing > 500) %>%
    left_join(LME %>% dplyr::select(LME_NAME), by = "LME_NAME") %>%
    st_as_sf() %>%
    sf::st_transform(world_ne, crs="ESRI:54030") %>%
    filter(!is.nan(percentage_increase))

  #First plot increase by LME
  LME_raw_increase <- MPA_LME %>%
    arrange(-difference_fishing) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(LME_NAME,difference_fishing), difference_fishing/1000) )+
    geom_bar(stat = "identity", fill = "#384B6A") +
    coord_flip() +
    my_custom_theme() +
    labs(x = " ",
         y = "Increase in fishing effort (in thousands of hours)")

  LME_percentage_increase <- MPA_LME %>%
    filter(predicted_fishing > 1000) %>%
    arrange(-percentage_increase) %>%
    slice(1:20) %>%
    ggplot(aes(reorder(LME_NAME,percentage_increase), percentage_increase)) +
    geom_bar(stat = "identity", fill = "#384B6A") +
    coord_flip() +
    my_custom_theme() +
    labs(x = " ",
         y = "Percentage increase in fishing effort")

  LME_increase <- ggarrange(LME_raw_increase, LME_percentage_increase, nrow = 2)

  ggsave(LME_increase,
         file = "figures/supp/LME_increase.jpg",
         width = 297 * 1.2 ,
         height = 297 * 1.2,
         units = "mm",
         dpi = 300)

  # Making Dorling cartogram based on total agricultural land
  dorl<-cartogram::cartogram_dorling(
    MPA_LME, weight='percentage_increase', k = 1.2,
    m_weight = 1, itermax = 1000
  )

  # World map excluding Antarctica
  world <- ne_countries(scale = 110, type = "countries", returnclass = "sf") %>%
    # Filter out Antarctica
    filter(!name == "Antarctica") %>%
    # Convert WGS84 to projected crs (here Robinson)
    sf::st_transform(crs = "ESRI:54030")

  # Set theme
  theme_custom <- theme_void()+
    theme(plot.background = element_rect(fill="white",color=NA),
          legend.position = )

  # Compute area and radius for each circus of the cartogram
  dorl<- dorl%>%
    mutate(
      # Compute area
      ar=as.numeric(st_area(dorl)),
      # Compute radius based on area
      rad=as.numeric(sqrt(ar/pi))
    )

  # Extract centroids for each circle
  centr <- dorl%>%
    st_centroid()%>%
    st_coordinates()
  
  # Modify the data preparation for pie charts
  dorl2 <- dorl %>%
    mutate(X = centr[,1], Y = centr[,2]) %>%
    mutate(
      total_fishing_effort = observed_fishing + difference_fishing,
      tracked_proportion = observed_fishing / total_fishing_effort,
      untracked_proportion = difference_fishing / total_fishing_effort
    )
  
  # Generate pie chart data using ggforce geom_arc_bar
  dorl2_pies <- dorl2 %>%
    pivot_longer(cols = c(tracked_proportion, untracked_proportion),
                 names_to = "fishing_type", values_to = "proportion") %>%
    mutate(start_angle = cumsum(lag(proportion, default = 0)) * 2 * pi,
           end_angle = (cumsum(proportion) * 2 * pi))
  
  # Plot the map with pie charts
  predicted_fishing_map <- ggplot() +
    # World basemap
    geom_sf(
      data = world, aes(geometry = geometry),
      fill = "#1A1A1A", color = alpha("dimgrey", 0.25)
    ) +
    # Reduced outline thickness of the circles
    ggforce::geom_circle(
      data = dorl2, aes(x0 = X, y0 = Y, r = rad),
      fill = alpha("dimgrey", 0.75), color = alpha("white", 0.1)  # Reduced outline opacity
    ) + 
  
  # Reduced outline thickness of the pie charts
  ggforce::geom_arc_bar(
    data = dorl2_pies,
    aes(x0 = X, y0 = Y, r0 = 0, r = rad, start = start_angle, end = end_angle, fill = fishing_type),
    color = alpha("black", 0.3), size = 0.2  # Reduced outline thickness
  ) + 
    scale_fill_manual(values = c("tracked_proportion" = "#E28A2A", "untracked_proportion" = "#384B6A")) +
    theme_custom
  
  # Save the updated map
  ggsave(predicted_fishing_map, file = "figures/predicted_fishing_map_proba.jpg",
         width = 210,  # A4 width in mm
         height = 148.5,  # Half of A4 height in mm
         units = "mm",
         dpi = 300)
  
  ggsave(predicted_fishing_map, file = "figures/predicted_fishing_map.svg",  width = 18.3*2 , height =  8.6 * 2 , units = "cm")
  
  # 
  # # Combine data
  # dorl2 <- tibble(dorl,X=centr[,1],Y=centr[,2])%>%
  #   arrange(-predicted_fishing) %>%
  #   mutate(ratio_fishing = observed_fishing/predicted_fishing,
  #          ratio_unseen_fishing = difference_fishing/predicted_fishing) %>%
  #   mutate(rad_fishing=sqrt(rad*rad*ratio_fishing),
  #          rad_unseen = sqrt(rad*rad*ratio_unseen_fishing))
  # 
  # col_fish <- "#E28A2A"
  # col_unseen <- "#384B6A"
  # 
  # circleFun <- function(
  #   center=c(0,0),   # center of the circle 
  #   diameter=1,      # diameter 
  #   npoints=100,     # number of points to draw the circle
  #   start=0, end=2   # start point/end point
  # ){
  #   tt <- seq(start*pi, end*pi, length.out=npoints)
  #   tb <- tibble(
  #     x = center[1] + diameter / 2 * cos(tt), 
  #     y = center[2] + diameter / 2 * sin(tt)
  #   )
  #   return(tb)
  # }
  # 
  # # Half circle for crops
  # half_fish <- bind_cols(
  #   LME_NAME = rep(dorl2$LME_NAME[1],100),
  #   circleFun(
  #     c(dorl2$X[1],dorl2$Y[1]),dorl2$rad_fishing[1]*2, start=1.5, end=2.5
  #   ))
  # 
  # # Half circle for grass
  # half_unseen <- bind_cols(
  #   LME_NAME = rep(dorl2$LME_NAME[1],100),
  #   circleFun(
  #     c(dorl2$X[1],dorl2$Y[1]),dorl2$rad_unseen[1]*2, start=0.5, end=1.5
  #   ))
  # 
  # # Make loop for all countries
  # for (i in 2:dim(dorl2)[1]){
  #   
  #   # Draw for crops
  #   temp_fish <- bind_cols(
  #     LME_NAME = rep(dorl2$LME_NAME[i],100),
  #     circleFun(
  #       c(dorl2$X[i],dorl2$Y[i]),dorl2$rad_fishing[i]*2, start=1.5, end=2.5
  #     ))
  #   # Draw for grass
  #   temp_unseen <- bind_cols(
  #     LME_NAME = rep(dorl2$LME_NAME[i],100),
  #     circleFun(
  #       c(dorl2$X[i],dorl2$Y[i]),dorl2$rad_unseen[i]*2, start=0.5, end=1.5
  #     ))
  #   
  #   half_fish<-half_fish%>%
  #     bind_rows(temp_fish)
  #   
  #   half_unseen<-half_unseen%>%
  #     bind_rows(temp_unseen)
  # }
  # 
  # predicted_fishing_map <- ggplot()+
  #   # World basemap
  #   geom_sf(
  #     world,mapping=aes(geometry=geometry),
  #     fill="#1A1A1A",color=alpha("dimgrey",0.25)
  #   )+
  #   # Draw Dorling cartogram with geom_circle()
  #   ggforce::geom_circle(
  #     data = dorl2, aes(x0 = X, y0 = Y, r = rad),
  #     fill=alpha("dimgrey",0.75),color=alpha("white",0.2)
  #   )+
  #   # Draw half circle for crop with geom_polygon
  #   geom_polygon(
  #     half_fish,
  #     mapping=aes(x,y,group=LME_NAME),
  #     fill=col_fish,color=NA
  #   )+ 
  #   # Draw half circle for grass with geom_polygon
  #   geom_polygon(
  #     half_unseen,
  #     mapping=aes(x,y,group=LME_NAME),
  #     fill=col_unseen,color=NA
  #   )+ 
  #   theme_custom
  # 
  # ggsave(predicted_fishing_map, file = "figures/predicted_fishing_map.jpg",
  #        width = 210,  # A4 width in mm
  #        height = 148.5,  # Half of A4 height in mm
  #        units = "mm", 
  #        dpi = 300)
  # 
  # ggsave(predicted_fishing_map, file = "figures/predicted_fishing_map.svg",  width = 18.3*2 , height =  8.6 * 2 , units = "cm")

  #For Figure 5
  ggsave(predicted_fishing_map,
         file = "figures/predicted_fishing_map.jpg", 
         width = 297*1.5, height = 105*1.5, units = "mm",dpi=600)
  
  ggsave("figures/correlation.jpg", plot = correlation, width = 148.5 * 1.5 , height = 105 * 1.5, units = "mm", dpi = 600)
  
  ggsave("figures/percentage_increase_full.jpg", plot = percentage_increase_full, 
         width = 180 *2, height = 105*2, units = "mm")
  
  #----- 
  

}
