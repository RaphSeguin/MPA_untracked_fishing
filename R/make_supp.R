make_supp <- function(MPA_final_vars){
  
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
    geom_sf(data = world, fill = "lightgrey") + 
    geom_sf(data = SAR_footprints_sf_union, fill = "lightblue") +
    theme_map()
  
  ggsave(SAR_footprints_map, file = "figures/supp/SAR_footprints_map.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  #Coverage of MPAs on SAR images
  MPAs_footprints_map <- ggplot() + 
    geom_sf(data = world, fill = "lightgrey") + 
    geom_sf(data = SAR_footprints_sf_union, fill = "lightblue") +
    geom_sf(data = mpa_wdpa, fill = "forestgreen") +
    theme_map()
  
  ggsave(MPAs_footprints_map, file = "figures/supp/MPAs_footprints_map.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  #Coverage of EEZ on SAR images
  EEZ_footprints_map <- ggplot() + 
    geom_sf(data = world, fill = "lightgrey") + 
    geom_sf(data = SAR_footprints_sf_union, fill = "lightblue") +
    geom_sf(data = eez_no_mpa, fill = "forestgreen") +
    theme_map()
  
  ggsave(EEZ_footprints_map, file = "figures/supp/EEZ_footprints_map.jpg", width = 297, height = 105, units = "mm", dpi = 300)
  
  #Number of MPAs by IUCN category, separated by fishing presence or absence,
  n_mpas_vessels_iucn_cat <- ggplot(MPA_final_vars, aes(x = factor(iucn_cat, levels = level_order), fill = SAR_presence)) +
    geom_bar(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("SAR" = "#051D41", "No_SAR" = "#8B0000"), labels = c("No vessels","Vessels")) + 
    labs(title = "Number of MPAs by IUCN category and vessel presence",
         x = "IUCN Category",
         y = "Number of MPAs",
         fill = "Vessel presence/absence") +
    my_custom_theme() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(n_mpas_vessels_iucn_cat, file = "figures/supp/n_mpas_vessels_iucn_cat.jpg",
         width = 297, height = 210, units = "mm", dpi = 300)
  
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
  
  #Number of SAR vessels by year of creation
  n_vessels_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR"), aes(x = factor(status_yr), y = log(sum_all))) +
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
  
  #Number of unmatched SAR vessels by year of creation
  n_unmatched_vessels_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR"), aes(x = factor(status_yr), y = log(unmatched_fishing))) +
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
  
  #Fraction of unmatched SAR vessels by year of creation
  n_unmatched_fraction_year <- ggplot(MPA_final_vars %>% filter(SAR_presence == "SAR"), aes(x = factor(status_yr), y = unmatched_ratio*100)) +
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
  SAR_eez_stats %>%
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
    slice(1:20) %>%
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
    mutate(AIS_fishing_all = AIS_fishing_2022 + AIS_fishing_2023,
           SAR_all = fishing_2022 + fishing_2023) %>%
    filter(AIS_fishing_all > 0) %>%
    filter(SAR_all > 0) %>%
    st_join(LME %>% dplyr::select(LME_NAME), join = st_nearest_feature) %>%
    group_by(LME_NAME) %>%
    filter(n() > 15) %>%
    ungroup()
    
  corr_across_LME <- ggplot(MPA_corr, aes(log(SAR_all), log(AIS_fishing_all))) + 
    geom_point() + 
    my_custom_theme() +
    labs(x = "Number of tracked detections",
         y = "Fishing effort") +
    facet_wrap(~ LME_NAME)
  
  ggsave(corr_across_LME, file = "figures/Supp/corr_across_LME.jpg",
         width = 297,
         height = 210,
         units = "mm",
         dpi = 300)
  
}
