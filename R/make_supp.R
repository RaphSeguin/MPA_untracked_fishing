make_supp <- function(MPA_final_vars){
  
  nrow(MPA_covariates)/nrow(mpas_all)
  
  #Figures on proportion of MPAs included
  MPA_union <- MPA_covariates %>%
    st_drop_geometry() %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn), by = "id_iucn") %>%
    st_as_sf() %>%
    group_by(parent_iso) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf() %>%
    mutate(area = set_units(st_area(.),"km^2"))
  
  MPA_union_all <- world_mpas_clean %>%
    group_by(parent_iso) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf() %>%
    mutate(area = set_units(st_area(.),"km^2"))
  
  sum(MPA_union$area)/sum(MPA_union_all$area) * 100

  #World for maps
  world <- rnaturalearth::ne_countries(scale = "large")

    #Number of MPAs by IUCN category using a bar plot 
  n_mpas_iucn_cat <- ggplot(MPA_final_vars, aes(x = fct_rev(fct_infreq(factor(iucn_cat, levels = level_order))), fill = iucn_cat)) +
    geom_bar(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = legend) +
    labs(title = "Number of MPAs by IUCN Category",
         x = " ",
         y = "Number of MPAs",
         fill = "IUCN Category") +
    my_custom_theme() 
  
  ggsave(n_mpas_iucn_cat, file = "figures/supp/n_mpas_iucn_cat.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Coverage of SAR images
  SAR_footprints_sf_union <- st_read("data/study_area_clean.shp")
  
  SAR_footprints_map <- ggplot() + 
    geom_sf(data = SAR_footprints_sf_union, fill = "lightblue") +
    geom_sf(data = eez, fill = NA, color = "black", lwd = 0.06) + 
    geom_sf(data = world %>% st_transform(crs = 4326), fill = "lightgrey") + 
    theme_map()
  
  ggsave(SAR_footprints_map, file = "figures/supp/SAR_footprints_map.jpg", width = 1920*2, height = 1080*2, units = "px", dpi = 300)
  
  ggsave(SAR_footprints_map, file = "figures/supp/SAR_footprints_map.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  #Coverage of MPAs on SAR images
  MPAs_included <- ggplot() + 
    geom_sf(data = world, fill = "lightgrey") + 
    geom_sf(data = mpas_all, fill = "darkred") +
    geom_sf(data = mpa_wdpa, fill = "forestgreen") +
    theme_map()
  
  ggsave(MPAs_included, file = "figures/supp/MPAs_included.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  #Number of MPAs by IUCN category, separated by fishing presence or absence,
  mpa_percentages <- MPA_final_vars %>%
    group_by(iucn_cat, SAR_presence) %>%
    summarise(count = n()) %>%
    group_by(iucn_cat) %>%
    mutate(percentage = count / sum(count) * 100)  # Calculate percentage
  
  # Create the bar plot with percentages on top of the bars
  (n_mpas_vessels_iucn_cat <- ggplot(MPA_final_vars, aes(x = factor(iucn_cat, levels = level_order), fill = SAR_presence)) +
    geom_bar(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("SAR" = "#051D41", "No_SAR" = "#8B0000"), labels = c("No vessels","Vessels")) + 
    labs(title = "Number of MPAs by IUCN category and vessel presence",
         x = "IUCN Category",
         y = "Number of MPAs",
         fill = "Vessel presence/absence") +
    my_custom_theme() +
    theme(legend.position = "bottom") +
    geom_text(data = mpa_percentages, aes(x = factor(iucn_cat, levels = level_order), y = count, label = paste0(round(percentage, 0), "%")), 
              position = position_dodge(width = 0.8), vjust = -0.5, size = 4))
  
  ggsave(n_mpas_vessels_iucn_cat, file = "figures/supp/n_mpas_vessels_iucn_cat_sum_probabilities.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Time series
  SAR_stats <- SAR_stats %>%
    mutate(timestamp = as.Date(timestamp))
  
  # Filter data for the time range January 2022 to December 2024
  SAR_stats_filtered <- SAR_stats %>%
    filter(timestamp >= as.Date("2022-01-01") & timestamp <= as.Date("2024-12-31"))
  
  # Summarize data by month
  SAR_stats_monthly <- SAR_stats_filtered %>%
    mutate(month = floor_date(timestamp, "month")) %>% # Group data by month
    group_by(month) %>%
    summarise(
      untracked_vessels = sum(unmatched_fishing, na.rm = TRUE),
      tracked_vessels = sum(fishing, na.rm = TRUE)
    )
  
  # Plot the time series
  SAR_time_series <- ggplot(SAR_stats_monthly, aes(x = month)) +
    geom_line(aes(y = untracked_vessels, color = "Untracked Vessels"), size = 1) +
    geom_line(aes(y = tracked_vessels, color = "Tracked Vessels"), size = 1) +
    scale_color_manual(
      values = c("Untracked Vessels" = "black", "Tracked Vessels" = "#E69F00"),
      name = "Vessel Type"
    ) +
    scale_x_date(
      date_labels = "%b %Y", 
      date_breaks = "3 months"
    ) +
    labs(
      title = "Monthly fishing vessel detections (2022-2024)",
      x = "Month",
      y = "Number of vessel detections"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom"
    ) +
    ylim(0, 150000)
  
  ggsave(SAR_time_series, file = "figures/supp/SAR_time_series.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #N images
  # Create a monthly column and count unique scene_id
  SAR_images_monthly <- SAR_footprints %>%
    st_drop_geometry() %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(n_images = n_distinct(scene_id)) %>%
    ungroup()
  
  SAR_image_series <- ggplot(SAR_images_monthly, aes(x = month, y = n_images)) +
    geom_line(color = "#56B4E9", size = 1) +
    labs(
      title = "Monthly number of unique SAR scenes (2022–2024)",
      x = "Month",
      y = "Number of SAR images"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) +
    ylim(0, NA)  # Allow y-axis to scale naturally
  
  combined_plot <- SAR_time_series / SAR_image_series + 
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
  
  
  # Save combined plot
  ggsave(combined_plot, file = "figures/supp/SAR_combined_time_series.jpg",
         width = 219, height = 297, units = "mm", dpi = 300)
  
  
  #Number of SAR vessels by management plan
  n_vessels_management <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR"), aes(x = management_plan, y = log(sum_all))) +
    geom_boxplot() +
    scale_fill_manual(values = legend) +
    labs(x = "",
         y = "Number of vessel detections (log-scale)") +
    my_custom_theme() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(n_vessels_management, file = "figures/supp/n_vessels_management.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of SAR vessels by year of creation
  n_vessels_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR" & status_yr != 0), aes(x = factor(status_yr), y = log(sum_all))) +
    geom_boxplot() +
    scale_x_discrete(breaks = seq(min(MPA_final_vars$status_yr), max(MPA_final_vars$status_yr), by = 10)) +
    labs(x = "MPA Creation Year",
         y = "Number of fishing vessel detections (log-scale)") +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_vessels_year, file = "figures/supp/n_vessels_year.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of unmatched SAR vessels by management plan
  n_unmatched_vessels_management <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR"), aes(x = management_plan, y = log(unmatched_fishing))) +
    geom_boxplot() +
    scale_fill_manual(values = legend) +
    labs(x = "",
         y = "Number of untracked vessel detections (log-scale)") +
    my_custom_theme() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(n_unmatched_vessels_management, file = "figures/supp/n_unmatched_vessels_management.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of unmatched SAR vessels by year of creation
  n_unmatched_vessels_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR" & status_yr != 0), aes(x = factor(status_yr), y = log(unmatched_fishing))) +
    geom_boxplot() +
    scale_x_discrete(breaks = seq(min(MPA_final_vars$status_yr), max(MPA_final_vars$status_yr), by = 10)) +
    labs(x = "MPA Creation Year",
         y = "Number of untracked fishing vessel detections (log-scale)") +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_unmatched_vessels_year, file = "figures/supp/n_unmatched_vessels_year.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Coverage of EEZ on SAR images
  EEZ_footprints_map <- ggplot() + 
    geom_sf(data = world %>% st_transform(crs = 4326), fill = "lightgrey") + 
    geom_sf(data = SAR_footprints_sf_union, fill = "lightblue") +
    geom_sf(data = eez_no_mpa, fill = "forestgreen") +
    theme_map()
  
  ggsave(EEZ_footprints_map, file = "figures/supp/EEZ_footprints_map.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  
  # Distribution of MPA sizes (area_correct) by IUCN category
  size_by_iucn <- ggplot(MPA_final_vars, aes(x = factor(iucn_cat, levels = level_order), y = area_correct, fill = iucn_cat)) +
    geom_boxplot() +
    scale_y_log10() +
    scale_fill_manual(values = legend) +
    labs(title = "Distribution of MPA sizes by IUCN Category",
         x = "IUCN Category",
         y = "MPA size (log-scale)",
         fill = "IUCN Category") +
    my_custom_theme() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(size_by_iucn, file = "figures/supp/size_by_iucn.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  
  #Fraction of unmatched SAR vessels by management plan
  n_fraction_management <- ggplot(MPA_final_vars, aes(x = management_plan, y = unmatched_ratio * 100)) +
    geom_boxplot() +
    scale_fill_manual(values = legend) +
    labs( x = "",
         y = "Percentage of untracked vessel detections") +
    my_custom_theme() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(n_fraction_management, file = "figures/supp/n_fraction_management.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)

  
  #Fraction of unmatched SAR vessels by year of creation
  n_unmatched_fraction_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR" & status_yr != 0), aes(x = factor(status_yr), y = unmatched_ratio*100)) +
    geom_boxplot() +
    scale_y_continuous() +
    scale_x_discrete(breaks = seq(min(MPA_final_vars$status_yr), max(MPA_final_vars$status_yr), by = 10)) +
    labs(x = "MPA Creation Year",
         y = "Percentage of untracked fishing vessel detections") +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_unmatched_fraction_year, file = "figures/supp/n_unmatched_fraction_year.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Surface of MPAs by country
  country_summary <- MPA_final_vars %>%
    group_by(parent_iso) %>%
    summarize(total_area = sum(area_correct, na.rm = TRUE) / 1000) %>%
    arrange(desc(total_area)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(parent_iso, "iso3c", "country.name")) %>%
    filter(!is.na(country_name))
  
  # Plotting
  surface_mpa_country <- ggplot(country_summary, aes(x = reorder(country_name, total_area), y = total_area)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(title = "Surface covered by MPAs by country",
         x = "",
         y = "Surface covered by MPAs (in thousands of km²)") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(surface_mpa_country, file = "figures/supp/surface_mpa_country.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of SAR vessels in EEZs
  n_vessels_EEZ <-  EEZ_final_vars %>%
    arrange(desc(sum_all)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(ISO_SOV1, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, sum_all), y = sum_all)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs( x = "",
          y = "Number of fishing vessel detections") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_vessels_EEZ, file = "figures/supp/n_vessels_EEZ.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of unmatched SAR vessels in EEZs
  n_unmatched_vessels_EEZ <- EEZ_final_vars %>%
    arrange(desc(unmatched_fishing)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(ISO_SOV1, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, unmatched_fishing), y = unmatched_fishing)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(x = "",
         y = "Number of untracked fishing vessel detections") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_unmatched_vessels_EEZ, file = "figures/supp/n_unmatched_vessels_EEZ.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of SAR vessels by country
  n_vessels_mpa <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    group_by(parent_iso) %>%
    summarize(total_vessels = sum(normalized_detection, na.rm = TRUE)) %>%
    arrange(desc(total_vessels)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(parent_iso, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, total_vessels), y = total_vessels)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(x = "",
         y = "Number of fishing vessel detections") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_vessels_mpa, file = "figures/supp/n_vessels_mpa.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Number of unmatched SAR vessels by country
  n_vessels_unmatched_mpa <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    filter(matched_category == "unmatched") %>%
    group_by(parent_iso) %>%
    summarize(total_vessels = sum(normalized_detection, na.rm = TRUE)) %>%
    arrange(desc(total_vessels)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(parent_iso, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, total_vessels), y = total_vessels)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(x = "",
         y = "Number of untracked fishing vessel detections") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(n_vessels_unmatched_mpa, file = "figures/supp/n_vessels_unmatched_mpa.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Average of SAR vessels densities by country
  density_mpas <- MPA_final_vars %>%
    group_by(parent_iso) %>%
    summarize(average_density = mean(relative_sum_all)) %>%
    arrange(desc(average_density)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(parent_iso, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, average_density), y = average_density)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(x = "",
         y = "Average density of fishing vessel detections in MPAs") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(density_mpas, file = "figures/supp/density_mpas.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  #Average of unmached SAR vessels densities by country
  unmatched_density_mpas <- MPA_final_vars %>%
    group_by(parent_iso) %>%
    summarize(average_density = mean(unmatched_relative)) %>%
    arrange(desc(average_density)) %>%
    slice(1:20) %>%
    mutate(country_name = countrycode(parent_iso, "iso3c", "country.name")) %>%
    filter(!is.na(country_name)) %>%
    ggplot(aes(x = reorder(country_name, average_density), y = average_density)) +
    geom_bar(stat = "identity", fill = "gray") +
    coord_flip() +
    labs(x = "",
         y = "Average density of untracked fishing vessel detections in MPAs") +
    scale_y_continuous(labels = scales::comma) +
    my_custom_theme() +
    theme(
      legend.position = "none"
    )
  
  ggsave(unmatched_density_mpas, file = "figures/supp/unmatched_density_mpas.jpg", 
         width = 297, height = 210, units = "mm", dpi = 300)
  
  
  #Fraction of SAR vessel in MPAs per EEZ
  truc <- SAR_eez_stats %>%
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
    arrange(-fraction_country) %>%
    filter(!parent_iso %in% c("FRA;ITA;MCO","NLD;DEU;DNK","SYC")) %>%
    slice(1:10) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPAs", "outside_mpa"="Outside MPAs"))+
    labs(x = " ",
         y = "Number of fishing vessels detected inside EEZ (in thousands)",
         fill = "Fraction of fishing vessels detected: ") +
    my_custom_theme() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  #Fraction of unmatched SAR vessels in MPAs per EEZ
  fraction_unmatched_mpa_vs_eez <- SAR_eez_stats %>%
    filter(matched_category == "unmatched") %>%
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
    arrange(-fraction_country) %>%
    filter(!parent_iso %in% c("FRA;ITA;MCO","NLD;DEU;DNK","SYC")) %>%
    filter(sum_country > 1) %>%
    slice(1:20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value, fill = name),stat="identity") +
    scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPAs", "outside_mpa"="Outside MPAs"))+
    labs(x = " ",
         y = "Number of untracked fishing vessel detections",
         fill = "Fraction of untracked fishing vessel detections: ") +
    my_custom_theme() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  ggsave(fraction_unmatched_mpa_vs_eez, file = "figures/supp/fraction_unmatched_mpa_vs_eez.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Length of vessels outside MPAs
  level_order_size <- c("0-15m","15-25m","25-35m","35-45","45-55",">55") 
  
  size_EEZ <- SAR_eez_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                      ifelse(length_m < 25, "15-25m",
                                             ifelse(length_m < 35, "25-35m",
                                                    ifelse(length_m < 45,"35-45",
                                                           ifelse(length_m < 55, "45-55",">55")))))) %>%
    group_by(size_class) %>%
    summarize(n_size_class = n()) %>%
    ungroup() %>%
    ggplot(aes(factor(size_class, level = level_order_size), n_size_class)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Size class",
         y = "Number of detection outside MPAs") +
    my_custom_theme()
  
  ggsave(size_EEZ, file = "figures/supp/size_EEZ.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Length of vessels inside MPAs
  size_mpas <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                      ifelse(length_m < 25, "15-25m",
                                             ifelse(length_m < 35, "25-35m",
                                                    ifelse(length_m < 45,"35-45",
                                                           ifelse(length_m < 55, "45-55",">55")))))) %>%
    group_by(size_class) %>%
    summarize(n_size_class = n()) %>%
    ungroup() %>%
    ggplot(aes(factor(size_class,level_order_size), n_size_class)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Size class",
         y = "Number of detection inside MPAs") +
    my_custom_theme()
  
  ggsave(size_mpas, file = "figures/supp/size_mpas.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Size distribution outside of MPAs
  size_distribution_EEZ <- SAR_eez_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                      ifelse(length_m < 25, "15-25m",
                                             ifelse(length_m < 35, "25-35m",
                                                    ifelse(length_m < 45,"35-45",
                                                           ifelse(length_m < 55, "45-55",">55")))))) %>%
    ggplot(aes(factor(size_class,level_order_size), length_m)) + 
    geom_violin() + 
    labs(x = "Size class",
         y = "Vessel length outside MPAs (m)") +
    my_custom_theme()
  
  ggsave(size_distribution_EEZ, file = "figures/supp/size_distribution_EEZ.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #Size distribution inside of MPAs
  size_distribution_MPA <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                      ifelse(length_m < 25, "15-25m",
                                             ifelse(length_m < 35, "25-35m",
                                                    ifelse(length_m < 45,"35-45",
                                                           ifelse(length_m < 55, "45-55",">55")))))) %>%
    ggplot(aes(factor(size_class,level_order_size), length_m)) + 
    geom_violin() + 
    labs(x = "Size class",
         y = "Vessel length outside MPAs (m)") +
    my_custom_theme()
  
  ggsave(size_distribution_MPA, file = "figures/supp/size_distribution_MPA.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #List of the MPAs with only matched SAR vessels
  MPA_SAR_no_unmatched <- MPA_final_vars %>% filter(SAR_presence == "SAR" & unmatched_fishing == 0)
  
  MPA_SAR_no_unmatched_plot <- MPA_SAR_no_unmatched %>%
    group_by(parent_iso) %>%
    reframe(count = n()) %>%
    ungroup() %>%
    slice(1:20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    ggplot(aes(reorder(country, - count), count)) + 
    geom_bar(stat = "identity") + 
    my_custom_theme() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(y = "Number of MPAs", x = " ")
  
  ggsave(MPA_SAR_no_unmatched_plot, file = "figures/supp/MPA_SAR_no_unmatched_plot.jpg", width = 297, height = 210,  units = "mm", dpi = 300)

  MPA_SAR_no_unmatched_table <- MPA_SAR_no_unmatched %>%
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, name) %>% st_drop_geometry(), by = "id_iucn") %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name"))  %>%
    dplyr::select(name, iucn_cat, management_plan,area_correct, country, fishing) %>%
    mutate(fishing = round(fishing,4),
           area_correct = round(area_correct, 0)) %>%
    arrange(desc(fishing)) %>%
    distinct(name, .keep_all = T) %>%
    distinct(area_correct, .keep_all = T)
  
  write.csv(MPA_SAR_no_unmatched_table, "figures/supp/MPA_SAR_no_unmatched_table.csv")
  
  #Correlation with LMEs
  MPA_corr <- MPA_covariates %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023 + AIS_fishing_2024,
           SAR_all = fishing_2022 + fishing_2023 + fishing_2024) %>%
    filter(AIS_fishing_all > 0) %>%
    filter(SAR_all > 0) %>%
    st_join(LME %>% dplyr::select(LME_NAME), join = st_nearest_feature) %>%
    group_by(LME_NAME) %>%
    filter(n() > 15) %>%
    ungroup()
    
  corr_across_LME <- ggplot(MPA_corr, aes(log(SAR_all), log(AIS_fishing_all))) + 
    geom_point() + 
    my_custom_theme() +
    labs(x = "Number of tracked detections (log scale)",
         y = "Fishing effort (log scale)") +
    facet_wrap(~ LME_NAME)
  
  ggsave(corr_across_LME, file = "figures/Supp/corr_across_LME.jpg",
         width = 297,
         height = 210,
         units = "mm",
         dpi = 300)
  
  #Correlation with IUCN
  MPA_corr_iucn <- MPA_covariates %>%
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023 + AIS_fishing_2024,
           SAR_all = fishing_2022 + fishing_2023 + fishing_2024) %>%
    filter(AIS_fishing_all > 0) %>%
    filter(SAR_all > 0) %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"), "I", as.character(iucn_cat))) %>%
    group_by(iucn_cat) %>%
    filter(n() > 15) %>%
    ungroup() %>%
    mutate(iucn_cat = factor(iucn_cat, levels = unique(iucn_cat))) 
  
  corr_across_iucn <- ggplot(MPA_corr_iucn, aes(log(SAR_all), log(AIS_fishing_all))) + 
    geom_point() + 
    my_custom_theme() +
    labs(x = "Number of tracked detections (log scale)",
         y = "Fishing effort (log scale)") +
    facet_wrap(~ iucn_cat)
  
  ggsave(corr_across_iucn, file = "figures/Supp/corr_across_iucn.jpg",
         width = 297,
         height = 210,
         units = "mm",
         dpi = 300)
  
  #Vizualisase covariates
  numeric_covariates <- c("area_correct", "mean_chl", "sd_chl", "mean_sst", "sd_sst",
                          "gdp", "HDI", "hf", "MarineEcosystemDependency",
                          "depth", "dist_to_shore", "travel_time")
  
  # Define numeric covariates used in the model and their explicit labels
  covariate_labels <- list(
    "area_correct" = "MPA size (sq km) (log+1-scale)",
    "mean_chl" = "Primary productivity (average)",
    "sd_chl" = "Primary productivity (SD)",
    "mean_sst" = "Sea surface temperature (mean)",
    "sd_sst" = "Sea surface temperature (SD)",
    "gdp" = "Gross domestic product (GDP) (log+1-scale)",
    "HDI" = "Human development index (HDI)",
    "hf" = "Human footprint index",
    "MarineEcosystemDependency" = "Marine ecosystem dependency",
    "depth" = "Average MPA depth (m) (log+1-scale)",
    "dist_to_shore" = "Distance to shore (km) (log+1-scale)",
    "travel_time" = "Travel time to markets (hours) (log+1-scale)"
  )
  
  # Covariates that require log(x+1) transformation
  log_transform_vars <- c("area_correct", "gdp", "depth", "dist_to_shore", "travel_time")
  
  # Define color scale
  # Define color scale
  color_scale <- c("#ffc6c4", "#f4a3a8", "#e38191", "#cc607d", "#ad466c", "#8b3058", "#672044")
  
  # Function to create individual plots with explicit labels, applying log transformation where necessary
  plot_covariate <- function(covariate) {
    data_filtered <- MPA_covariates %>% filter(!is.na(.data[[covariate]])) # Drop NAs
    
    # Apply log+1 transformation if needed
    if (covariate %in% log_transform_vars) {
      data_filtered <- data_filtered %>% mutate(!!covariate := log(abs(.data[[covariate]])+1))
    }
    
    ggplot() +
      geom_sf(data = world, fill = "gray90", color = "white") +
      geom_sf(data = data_filtered, aes_string(color = covariate), size = 1) +
      scale_color_gradientn(colors = color_scale) +
      labs(title = covariate_labels[[covariate]], color = covariate_labels[[covariate]]) +
      theme_map() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))   # Increased size and boldened title
  }
  
  # Generate plots for each numeric covariate
  plots <- lapply(names(covariate_labels), plot_covariate)
  
  # Arrange all plots for a vertical A4 layout (fewer columns, more rows)
  map_covariates <- ggarrange(plotlist = plots, ncol = 3, nrow = 4, common.legend = F, legend = "bottom")
  
  # Save the final plot with adjusted dimensions for vertical A4 (portrait mode)
  ggsave(map_covariates, file = "figures/supp/map_all_covariates.jpg", width = 210 * 1.5, height = 297 * 1.4, units = "mm", dpi = 300)
  
  #Histogram of covaraites
  # Define the numeric covariates and their labels
  covariate_labels <- list(
    "area_correct" = "MPA size (sq km)",
    "mean_chl" = "Primary productivity (average)",
    "sd_chl" = "Primary productivity (SD)",
    "mean_sst" = "Sea surface temperature (mean)",
    "sd_sst" = "Sea surface temperature (SD)",
    "gdp" = "Gross domestic product (GDP)",
    "HDI" = "Human development index (HDI)",
    "hf" = "Human footprint index",
    "MarineEcosystemDependency" = "Marine ecosystem dependency",
    "depth" = "Average MPA depth (m)",
    "dist_to_shore" = "Distance to shore (km)",
    "travel_time" = "Travel time to markets (hours)"
  )
  
  # Create an empty list to store histogram plots
  histograms <- list()
  
  # Loop through each covariate and create a histogram
  for (covariate in names(covariate_labels)) {
    data_filtered <- MPA_covariates %>% filter(!is.na(.data[[covariate]])) # Drop NAs
    
    # Create histogram
    hist_plot <- ggplot(data_filtered, aes_string(x = covariate)) +
      geom_histogram(fill = "#cc607d", color = "black", bins = 30) +
      labs(title = covariate_labels[[covariate]], x = covariate_labels[[covariate]], y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    
    # Store in the list
    histograms[[covariate]] <- hist_plot
  }
  
  # Arrange all histograms in a grid (portrait layout for A4)
  covariates_histograms <- ggarrange(plotlist = histograms, ncol = 3, nrow = 4)
  
  # Save the histogram plots in A4 portrait format
  ggsave(covariates_histograms, file = "figures/supp/covariates_histograms.jpg", width = 210 * 1.4, height = 297 * 1.4, units = "mm", dpi = 300)
  
  #Correlation table
  numeric_covariates <- c(
    "area_correct", "mean_chl", "sd_chl", "mean_sst", "sd_sst",
    "gdp", "HDI", "hf", "MarineEcosystemDependency",
    "depth", "dist_to_shore", "travel_time"
  )
  
  covariate_labels <- c(
    "area_correct" = "MPA size",
    "mean_chl" = "Primary productivity (average)",
    "sd_chl" = "Primary productivity (SD)",
    "mean_sst" = "Sea surface temperature (mean)",
    "sd_sst" = "Sea surface temperature (SD)",
    "gdp" = "Gross domestic product",
    "HDI" = "Human development index",
    "hf" = "Human footprint index",
    "MarineEcosystemDependency" = "Marine ecosystem dependency",
    "depth" = "Average MPA depth",
    "dist_to_shore" = "Distance to shore",
    "travel_time" = "Travel time to markets"
  )
  
  # Ensure all columns in numeric_covariates are numeric
  MPA_correlation <- MPA_covariates %>%
    st_drop_geometry() %>%
    dplyr::select(all_of(numeric_covariates)) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
    
  correlation_matrix <- cor(MPA_correlation[, numeric_covariates], use = "complete.obs")
  
  # Rename the correlation matrix with explicit names
  colnames(correlation_matrix) <- rownames(correlation_matrix) <- covariate_labels
  
  # Melt the correlation matrix for visualization
  correlation_melted <- reshape2::melt(correlation_matrix)
  
  # Plot the correlation heatmap
  correlation_heatmap <- ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#112090", mid = "white", high = "#DD0000", midpoint = 0, limit = c(-1, 1), space = "Lab") +
    labs(title = "Correlation heatmap of covariates", x = "", y = "", fill = "Correlation") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) + 
    theme(legend.position = "bottom") 
  
  # Save the heatmap as an image
  ggsave("figures/supp/correlation_heatmap.jpg", plot = correlation_heatmap, width = 297, height = 210, units = "mm", dpi = 300)
  
  #Table
  
  covariate_labels <- list(
    "area_correct" = "MPA size (sq km)",
    "mean_chl" = "Primary productivity (average, mg/m³)",
    "sd_chl" = "Primary productivity (SD, mg/m³)",
    "mean_sst" = "Sea surface temperature (mean, °C)",
    "sd_sst" = "Sea surface temperature (SD, °C)",
    "gdp" = "Gross domestic product (GDP, USD)",
    "HDI" = "Human development index (HDI)",
    "hf" = "Human footprint index",
    "MarineEcosystemDependency" = "Marine ecosystem dependency",
    "depth" = "Average MPA depth (m)",
    "dist_to_shore" = "Distance to shore (km)",
    "travel_time" = "Travel time to markets (hours)"
  )
  
  # Function to compute summary statistics for each covariate
  summarize_covariate <- function(data, covariate, label) {
    data_filtered <- data %>% filter(!is.na(.data[[covariate]])) # Remove NAs
    
    summary_stats <- data_filtered %>%
      st_drop_geometry() %>%
      summarise(
        Mean = round(mean(.data[[covariate]], na.rm = TRUE), 2),
        Median = round(median(.data[[covariate]], na.rm = TRUE), 2),
        Min = round(min(.data[[covariate]], na.rm = TRUE), 2),
        Max = round(max(.data[[covariate]], na.rm = TRUE), 2),
        SD = round(sd(.data[[covariate]], na.rm = TRUE), 2),
        IQR = round(IQR(.data[[covariate]], na.rm = TRUE), 2)
      ) %>%
      mutate(Covariate = label) %>%
      dplyr::select(Covariate, everything()) # Reorder columns
    
    return(summary_stats)
  }
  
  # Compute summary statistics for all covariates
  covariate_summary <- bind_rows(lapply(names(covariate_labels), function(cov) {
    summarize_covariate(MPA_covariates, cov, covariate_labels[[cov]])
  }))
  
  # Print the summary table
  print(covariate_summary)
  
  # Save as CSV file
  write.csv(covariate_summary, "figures/supp/covariate_summary.csv", row.names = FALSE)
  
}
