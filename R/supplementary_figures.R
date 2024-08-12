supplementary_figures <- function(){
  
  #Data with unique info for each considered MPA
  MPA_data <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) 
  
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
  
   world <- ne_countries(scale = "large", returnclass = "sf")

  # Fig. S 1
  # Number of marine protected areas considered per IUCN category in the study. 
  # We removed MPAs of category IUCN III given the small number of MPAs of this category in our dataset. 
  # 
  (MPA_data_S1 <- mpa_wdpa_no_sf %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat)) %>%
      group_by(iucn_cat) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_bar(aes(reorder(iucn_cat,sum),sum,fill=iucn_cat),stat='identity') +
      scale_fill_manual(values = legend)+
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Number of MPAs",
           fill = "IUCN Category")+
      theme(legend.position="bottom")) 
  
  ggsave(MPA_data_S1, file = "Figures/Supp/MPA_data_S1.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  #
  # Fig. S 2
  # Area covered by SAR satellite images. 
  # The shapefile for this map was downloaded from Paolo et al. 2024. 
  # 
  
  #Now keeping only MPAs which fall withing range of study area
  #downloaded from nature paper
  Fig_S2_data <- read.csv("data/study_area.csv") %>%
    dplyr::select(study_area) %>%
    # pivot_longer(cols = c(study_area,study_area_02,study_area_05)) %>%
    dplyr::rename(geometry = "study_area") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    head(18713) %>%
    st_union() 
  
  Fig_S2 <- ggplot() +
    geom_sf(data = Fig_S2_data, fill = "lightblue") +
    geom_sf(data = world, fill = "lightgrey") +
    theme_map()
  
  ggsave(Fig_S2, file = "Figures/Supp/Fig_S2.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig S3 
  # 
  # In green, final selection of MPAs after removing the 1km buffer from the shoreline and MPAs 
  # which are not covered by satellite images. 
  # In blue, all MPAs created before 2017 without the final selection steps. 
  # Given the coverage of satellite images, our study removes most remote MPAs.
  mpa_wdpa_all <- bind_rows(st_read(dsn = "maps/WDPA/WDPA_Feb2024_Public_shp_0",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Feb2024_Public_shp_1",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE),
                        st_read(dsn =
                                  "maps/WDPA/WDPA_Feb2024_Public_shp_2",
                                layer = "WDPA_Feb2024_Public_shp-polygons",
                                quiet = TRUE)) %>%
    clean_names() %>%
    #Keep only marine and partially marines MPAs
    dplyr::filter(marine %in% c(1,2),
                  !status_yr == 0) %>%
    #Only selecting MPAs created BEFORE 2017 
    filter(status_yr < 2017) %>%
    filter(!status %in% c("Proposed","Established","Not Reported")) %>%
    #No MPAs which are outside EEZs
    filter(iso3 != "ABNJ") %>%
    #Only with management plan
    #Selection 
    dplyr::select(id=wdpaid, name, desig_eng, iucn_cat, status_yr,status, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine) 
  
  Fig_S3 <- ggplot() +
    geom_sf(data = world, fill = "lightgrey") +
    geom_sf(data = mpa_wdpa_all, fill = "lightblue", lwd = 0) +
    geom_sf(data = mpa_wdpa, fill = "forestgreen", lwd= 0) +
    theme_map()
  
  ggsave(Fig_S3, file = "Figures/Supp/Fig_S3.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 4
  # Number of MPAs with and without fishing vessels inside according to IUCN category. 
  # 
  
  (Fig_S4 <- mpa_wdpa_no_sf %>%
      mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat)) %>%
      #sCheck if fhsing in MPA
      mutate(fishing_presence = as.factor(ifelse(id_iucn %in% MPA_data$id_iucn,"Fishing","No_fishing"))) %>%
      ggplot(aes(x = factor(iucn_cat,level = level_order), group = fishing_presence, fill = fishing_presence)) +
      geom_bar(position = position_dodge(width = 0.7),stat = "count") +
      scale_fill_manual(values = c("Fishing" = "#051D41", 
                                   "No_fishing" = "#9B3742"), labels = c("Fishing" = "Fishing","No_fishing"= "No fishing")) +
      theme_minimal(base_size = 16) +
      labs(x = " ",
           y = "Number of MPAs",
           fill = "Fishing presence/absence") +
      theme(legend.position = "bottom"))
  
  ggsave(Fig_S4, file = "figures/Supp/Fig_S4.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  # Fig. S 5
  # MPA size (log-scale) according to IUCN category. 
  # 
  
  (Fig_S5 <- mpa_wdpa_no_sf %>%
      mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia","Ib"),"I",iucn_cat)) %>%
    ggplot(aes(factor(iucn_cat,level = level_order), log(area_correct),fill = iucn_cat)) +
    geom_boxplot(alpha = 0.8)+
      scale_fill_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))+
      theme_minimal(base_size = 16)+
      labs(x = " ",
           y = "MPA size (log-scale)",
           fill = "IUCN category"))
  
  ggsave(Fig_S5, file = "figures/Supp/Fig_S5.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  # Table S 3
  # Results for the binomial model on fishing vessel presence. 
  # IUCN category I serves as the reference category for the factor variable on IUCN categories. 
  Table_S3 <- summary(mod_spamm_binomial,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "area_correct","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",Variable)
                                                                                                   ))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(mod_spamm_binomial_output, "Figures/Supp/Table_S3.csv")
  
  # Fig. S 5
  # Relationship between MPA size, MPA type (fully marine or marine and terrestrial) 
  # and travel time to the nearest city on the presence/absence of fishing vessels inside MPAs. 
  # All three significant covariates (according to t-value < - 1.96 or > 1.96) for the binomial model
  # of presence/absence of fishing vessels inside MPAs (n = 5,211) are shown here. Effects were plotted 
  # using the visreg function from the visreg package. 
  # 
  
  partial_effects_area <- visreg(mod_spamm_binomial,"area_correct",type="conditional",scale="response")$fit %>% mutate(var = "MPA area") %>% dplyr::rename(pred_var = "area_correct")
  partial_effects_marine<- visreg(mod_spamm_binomial,"marine",type="conditional",scale = "response")$fit %>% dplyr::select(marine, visregFit) %>% mutate(var = "Marine or Terrestrial MPA") %>% dplyr::rename(pred_var = "marine")
  partial_effects_travel_time <- visreg(mod_spamm_binomial,"travel_time",type="conditional",scale="response")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel time to the nearest city") %>% dplyr::rename(pred_var = "travel_time")

  plot_effects_area <- ggplot(partial_effects_area) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "MPA area (log-scale)",
         y = "Probability of containing fishing vessels") 
  
  plot_effects_marine <- ggplot(partial_effects_marine) + 
    geom_boxplot(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Fully marine or marine and terrestrial MPA",
         y = "Probability of containing fishing vessels") 
  
  plot_effects_traveltime <- ggplot(partial_effects_travel_time) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time to the nearest city (log-scale)",
         y = "Probability of containing fishing vessels") 
  
  
  Fig_S6 <- ggarrange(plot_effects_area, 
            plot_effects_marine + rremove("ylab"),
            plot_effects_traveltime + rremove("ylab"), nrow = 1)
  
  ggsave(Fig_S6, file = "Figures/Supp/Fig_S6.jpg",width = 297, height = 210, units = "mm", dpi = 300)
  
  
  
  # Fig. S 7
  # Exclusive Economic Zones (EEZs) considered in this study after cropping them to the extent of the area 
  # covered by satellite images (green). Worldwide EEZs before cropping to study area (blue). 
  
  #EEZ after removing study area
  Fig_S7 <- ggplot() +
    geom_sf(data = world, fill = "lightgrey") + 
    geom_sf(data = eez, fill = "lightblue") + 
    geom_sf(data = eez_no_mpa, fill = "forestgreen") +
    theme_map()
  
  ggsave(Fig_S7, file = "Figures/Supp/Fig_S7.jpg")
  
  #Table S 4
  # Results of the Wilcoxon test to compare the density of fishing vessels density 
  # inside and outside MPAs with a declared IUCN category. 
  #Test stat
  Table_S4_test_stats <-  SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat))
  
  #Test
  res.kruskal <- Table_S4_test_stats %>% kruskal_test(relative_sum_all ~ iucn_cat)
  res.kruskal
  
  Table_S4 <- Table_S4_test_stats %>% wilcox_test(relative_sum_all ~ iucn_cat, p.adjust.method = "bonferroni")
  
  write.csv(Table_S4, file = "Figures/Supp/Table_S4.csv")
  
  #Fig. S 8
  
  # Density of fishing vessels (publicly and not publicly tracked) inside MPAs (log-scaled) with (n = 1,214) 
  # and without (n = 1,912) a management plan according to the WDPA database. 
  # The sum of fishing vessels inside MPAs was divided by the mean number of SAR acquisitions inside the MPA, 
  # resulting in an average number of fishing vessel detections per image per squared kilometer. 
  
  
  #Management plan against relative
  (Fig_S8 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id_iucn, management_plan) %>%
      inner_join(MPA_data, by = "id_iucn") %>%
      group_by(management_plan) %>%
      # na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,log(relative_sum_all))) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Density of fishing vessels"))
  
  stat_test <- mpa_wdpa_no_sf %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(id_iucn, .keep_all = T) %>%
    mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
    dplyr::select(id_iucn, management_plan) %>%
    inner_join(MPA_data, by = "id_iucn") 
  
  t.test(relative_sum_all ~ management_plan, data = stat_test)
  
  ggsave(Fig_S8, file = "Figures/Supp/Fig_S8.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  # 
  
  # Fig. S 9
  # Density of fishing vessels inside MPAs (log-scaled) by MPA creation year according to the WDPA database. 
  # The sum of fishing vessels inside MPAs was divided by the mean number of SAR acquisitions inside the MPA,
  # resulting in an average number of fishing vessel detections per image per squared kilometer. 
  #
  
  #Across time
  (Fig_S9 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr)) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,log(relative_sum_all),group = status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Density of fishing vessels inside MPAs"))
  
  ggsave(Fig_S9, file = "Figures/Supp/Fig_S9.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Table S 5
  # Results for the model on fishing vessel density (n = 1,182). 
  # IUCN category I serves as the reference category for the factor variable on IUCN categories. 
  # 
  
  Table_S5 <- summary(mod_spamm,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catI", "IUCN category I", 
                                                                                                                                      ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                             ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                             ))))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(Table_S5, file = "Figures/Supp/Table_S5.csv")
  
  
  # Fig. S 10
  # Relationship between the three main model predictors and fishing vessel density. 
  # Top three significant covariates (according to t-value < - 1.96 or > 1.96) for 
  # the model of fishing model density in MPAs with a declared IUCN category are shown here, 
  # except MPA size and the fully/partially marine factor, which both were the strongest predictors in all models. 
  # Effects were plotted using the visreg function from the visreg package.
  #

  partial_effects_Bathymetry <- visreg(mod_spamm,"Bathymetry",type="conditional")$fit %>% dplyr::select(Bathymetry, visregFit) %>% mutate(var = "Depth") %>% dplyr::rename(pred_var = "Bathymetry")
  partial_effects_traveltime <- visreg(mod_spamm,"travel_time",type="conditional")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel Time") %>% dplyr::rename(pred_var = "travel_time")
  partial_effects_Chla <- visreg(mod_spamm,"Chla",type="conditional")$fit %>% dplyr::select(Chla, visregFit) %>% mutate(var = "Chla") %>% dplyr::rename(pred_var = "Chla")

  plot_Bathymetry<- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Depth (log-scale)",
         y = "Fitted coefficients")
  
  plot_traveltime<- ggplot(partial_effects_traveltime) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time (log-scale)",
         y = "Fitted coefficients")
  
  plot_Chla <- ggplot(partial_effects_Chla) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Primary productivity (log-scale)",
         y = "Fitted coefficients")
  
  (Fig_S10 <- ggarrange(plot_Bathymetry,
                       plot_traveltime + rremove("ylab"), 
                       plot_Chla + rremove("ylab"), nrow = 1))
  
  ggsave(Fig_S10, file = "Figures/Supp/Fig_S10.jpg")
  
  
  # Fig. S 11
  # Top 20 countries with the highest surface covered by marine protected areas (expressed in thousands of km²). 
  #
  
  MPA_union <- mpa_wdpa %>%
    group_by(parent_iso) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup()
  
  (Fig_S11 <-  MPA_union %>%
      mutate(mpa_area = as.numeric(set_units(st_area(MPA_union %>% st_as_sf()),km^2))) %>%
      st_drop_geometry() %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      drop_na(country) %>%
      dplyr::distinct(country, mpa_area) %>%
      arrange(-mpa_area) %>%
      head(20) %>%
      na.omit() %>%
      ggplot() +
      geom_bar(aes(reorder(country,mpa_area),mpa_area/1000),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Surface covered by MPAs (in thousands of km²)"))
    
    
  ggsave(Fig_S11, file = "Figures/Supp/Fig_S11.jpg")
  
  # Fig. S 12
  # Top 20 countries with the highest number of fishing vessels (expressed in thousands of vessels) 
  # detected inside their EEZ (inside and outside MPAs). 
  # 
  
  #Fraction of fleet that is inside MPA
  SAR_inside_mpas <- SAR_stats %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "inside_mpa") %>%
    dplyr::select(parent_iso,matched_category, type)
  
  (Fig_S12 <- SAR_eez_final %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(type = "outside_mpa") %>%
      dplyr::rename(parent_iso = "ISO_TER1") %>%
      dplyr::select(parent_iso, matched_category, type) %>%
      # filter(matched_category=="unmatched") %>%
      bind_rows(SAR_inside_mpas) %>%
      #Group by type and parent_iso
      group_by(parent_iso) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      arrange(-sum_country) %>%
      head(20) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum_country), sum_country/1000),stat="identity") +
      theme_minimal(base_size = 14) +
      coord_flip() +
      labs(x = " ",
           y = "Number of vessels detected in EEZ (in thousands)"))
  
  ggsave(Fig_S12, file = "Figures/Supp/Fig_S12.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  # Fig. S 13
  # Top 20 countries with the highest number of not publicly tracked fishing vessels (in thousands) 
  # detected inside EEZ (inside and outside MPAs) 
  
  #All vessels unmatched
  (Fig_S13 <- SAR_eez_final %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(type = "outside_mpa") %>%
      dplyr::rename(parent_iso = "ISO_TER1") %>%
      dplyr::select(parent_iso, matched_category, type) %>%
      filter(matched_category=="unmatched") %>%
      bind_rows(SAR_inside_mpas) %>%
      #Group by type and parent_iso
      group_by(parent_iso) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      arrange(-sum_country) %>%
      head(20) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum_country), sum_country/1000),stat="identity") +
      theme_minimal(base_size = 14) +
      coord_flip() +
      labs(x = " ",
           y = "Number of unmatched vessels detected in EEZ"))
  
  ggsave(Fig_S13, file = "Figures/Supp/Fig_S13.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  # Fig. S 14
  # Top 20 countries with the highest number of not publicly tracked fishing vessels detected inside MPAs.  
  # 
  
  #Inside MPAs
  (Fig_S14 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(type = "inside_mpa") %>%
      group_by(parent_iso) %>%
      reframe(sum_country = sum(detection_count)) %>%
      ungroup() %>%
      arrange(-sum_country) %>%
      #Deleteing joint regime to avoid double counting in same EEZ
      filter(parent_iso != "FRA;ITA;MCO") %>%
      head(20) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum_country), sum_country),stat="identity") +
      theme_minimal(base_size = 14) +
      coord_flip() +
      labs(x = " ",
           y = "Number of fishing vessels detected inside MPAs"))
  
  ggsave(Fig_S14, file = "Figures/Supp/Fig_S14.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  # Fig. S 15
  # Top 20 countries with the highest number of not publicly tracked fishing vessels detected inside MPAs.  
  # 
  
  #Inside MPAs unmatched
  (Fig_S15 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(type = "inside_mpa") %>%
      dplyr::select(parent_iso,matched_category, type) %>%
      filter(matched_category=="unmatched") %>%
      group_by(parent_iso) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      arrange(-sum_country) %>%
      #Deleteing joint regime to avoid double counting in same EEZ
      filter(parent_iso != "FRA;ITA;MCO") %>%
      head(20) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      ggplot() +
      geom_bar(aes(reorder(country,sum_country), sum_country/1000),stat="identity") +
      theme_minimal(base_size = 14) +
      coord_flip() +
      labs(x = " ",
           y = "Number of not publicly tracked vessels detected in MPAs (in thousands)"))
  
  ggsave(Fig_S15, file = "Figures/Supp/Fig_S15.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  # Fig. S 16
  # Top 20 countries with the highest average density of fishing vessels inside MPAs. 
  # Fishing vessel detections inside MPAs were divided by the number of SAR acquisitions taken at the place of 
  # the detection between 2017 and 2021, resulting in an average number of fishing vessel detections per image 
  # per squared kilometer. 
  
  #Country vs total
  (Fig_S16 <- MPA_data %>%
      filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      group_by(country) %>%
      reframe(mean = mean(relative_sum_all)) %>%
      ungroup() %>%
      arrange(-mean) %>%
      head(20) %>%
      ggplot() +
      geom_bar(aes(reorder(country,mean),mean),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Average density of fishing vessels inside MPAs (number of detections/image/km²)"))
  
  ggsave(Fig_S16, file = "Figures/Supp/Fig_S16.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # #Fig. S 17
  # Top 20 countries with the highest fraction of fishing vessels (in thousands) detected inside MPAs
  # (compared to the total number of fishing vessels detected inside the EEZ). 
  # Countries are ordered by total number of fishing vessels detected inside the EEZ on this graph and 
  # filled by fishing vessels detected inside MPAs and outside MPAs. 
  # Germany was the country with the highest fraction of fishing vessels detected inside MPAs, 
  # with 63% of all fishing vessels detected inside their EEZs.  
  #
  
  (Fig_S17 <- SAR_eez_final %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(type = "outside_mpa") %>%
      dplyr::rename(parent_iso = "ISO_TER1") %>%
      dplyr::select(parent_iso, matched_category, type) %>%
      bind_rows(SAR_inside_mpas) %>%
      #Group by type and parent_iso
      group_by(type, parent_iso) %>%
      reframe(sum_country = n()) %>%
      ungroup() %>%
      #Widen data to calculate some stuff
      pivot_wider(names_from = "type",values_from="sum_country") %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      mutate(sum_country = inside_mpa + outside_mpa,
             fraction_country = inside_mpa/(outside_mpa+inside_mpa)) %>%
      filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
      arrange(-fraction_country) %>%
      head(20) %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      #Pivot longer for plots
      pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
      ggplot() +
      geom_bar(aes(reorder(country,-value), value/1000, fill = name),stat="identity") +
      scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPA", "outside_mpa"="Outside MPA"))+
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(x = " ",
           y = "Number of fishing vessels detected inside EEZ (in thousands)",
           fill = "Fraction of fishing vessels detected: ") +
      theme(legend.position = "top"))
  
  ggsave(Fig_S17, file = "Figures/Supp/Fig_S17.jpg", width = 297, height = 210, units = "mm", dpi = 300 )
  
  
  # Fig. S 18
  # Number of detections per size class of vessel length outside MPAs. 
  # 
  
  level_order_size <- c("0-15m","15-25m","25-35m","35-45","45-55",">55") 
  
  (Fig_S18 <- SAR_eez_final %>%
      
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      group_by(size_class) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_bar(aes(factor(size_class,level=level_order_size),sum,group=size_class),stat='identity')+
      theme_minimal() +
      labs(x = "Size class",
           y = "Number of detections outisde MPAs"))
  
  ggsave(Fig_S18, file = "Figures/Supp/Fig_S18.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 19
  # Number of detections per size class of vessel length inside MPAs. 
  # 
  
  (Fig_S19 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      group_by(size_class) %>%
      reframe(sum = n()) %>%
      ungroup() %>%
      ggplot() +
      geom_bar(aes(factor(size_class,level=level_order_size),sum,group=size_class),stat='identity')+
      theme_minimal() +
      labs(x = "Size class",
           y = "Number of detections inside MPAs"))
  
  ggsave(Fig_S19, file = "Figures/Supp/Fig_S19.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 20
  # Size distribution per size class of vessel length outside MPAs. 
  # 
  
  (Fig_S20 <- SAR_eez_final %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      ggplot() +
      geom_violin(aes(factor(size_class,level=level_order_size),length_m,group=size_class))+
      theme_minimal() +
      labs(x = "Size class",
           y = "Vessel length outside MPAs"))
  
  ggsave(Fig_S20, file = "Figures/Supp/Fig_S20.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 21
  # Size distribution per size class of vessel length inside MPAs. 
  # 
  (Fig_S21 <- SAR_stats %>%
      distinct(unique_id, .keep_all = T) %>%
      dplyr::mutate(size_class = ifelse(length_m < 15, "0-15m",
                                        ifelse(length_m < 25, "15-25m",
                                               ifelse(length_m < 35, "25-35m",
                                                      ifelse(length_m < 45,"35-45",
                                                             ifelse(length_m < 55, "45-55",">55")))))) %>%
      ggplot() +
      geom_violin(aes(factor(size_class,level=level_order_size),length_m,group=size_class))+
      theme_minimal() +
      labs(x = "Size class",
           y = "Vessel length inside MPAs"))
  
  ggsave(Fig_S21, file = "Figures/Supp/Fig_S21.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Table S 6
  # Model results for the average vessel size in MPAs. 
  # IUCN category I serves as the reference category for the factor variable on IUCN categories. 
  # 
  
  Table_S6 <- summary(mod_spamm_size,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(Table_S6,"Figures/supp/Table_S6.csv")
  
  # Table S 7
  # Results of the Wilcoxon test on the density of unmatched fishing vessels inside and outside MPAs 
  # with a declared IUCN category. 
  # 
  
  #Test stat
  Table_S7_unmatched <-  SAR_stats %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    distinct(id_iucn, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat))
  
  #Test
  res.kruskal_unmatched <- Table_S7_unmatched %>% kruskal_test(unmatched_relative ~ iucn_cat)
  
  Table_S7 <- Table_S7_unmatched %>% wilcox_test(unmatched_relative ~ iucn_cat, p.adjust.method = "bonferroni")
  
  write.csv(Table_S7, "Figures/supp/Table_S7.csv")
  
  # Fig. S 22
  # Density of not publicly tracked fishing vessels inside MPAs (log-scaled) with
  # and without  a management plan according to the WDPA database.
  # The sum of fishing vessels inside MPAs was divided by the mean number of SAR acquisitions inside the MPA, 
  # resulting in an average number of fishing vessel detections per image per squared kilometer. 
  # 
  
  (Fig_S22 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id_iucn, management_plan) %>%
      inner_join(MPA_data, by = "id_iucn") %>%
      group_by(management_plan) %>%
      # na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,log(unmatched_relative))) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Density of not publicly tracked fishing vessels"))
  
  ggsave(Fig_S22, file = "Figures/Supp/Fig_S22.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 23
  # Density of not publicly tracked fishing vessels inside MPAs (log-scaled) 
  # by MPA creation year according to the WDPA database. 
  # The sum of fishing vessels inside MPAs was divided by the mean number of SAR acquisitions inside the MPA, 
  # resulting in an average number of fishing vessel detections per image per squared kilometer. 
  
  (Fig_S23 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr)) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,log(unmatched_relative),group = status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Density of not publicly tracked fishing vessels inside MPAs"))
  
  ggsave(Fig_S23, file = "Figures/Supp/Fig_S23.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  
  # Table S 8
  # Model results for density of unmatched fishing vessels. 
  
  Table_S8 <- summary(mod_spamm_unmatched,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(Table_S8,"Figures/Supp/Table_S8.csv")
  
  # Fig. S 24
  # Top 20 countries with the highest average density of not publicly tracked fishing vessels inside MPAs. 
  # The sum of fishing vessels inside MPAs was divided by the mean number of SAR acquisitions inside the MPA, 
  # resulting in an average number of fishing vessel detections per image per squared kilometer. 
  #
  #Unmatched
  (Fig_S24 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      group_by(country) %>%
      reframe(mean = mean(unmatched_relative)) %>%
      ungroup() %>%
      arrange(-mean) %>%
      head(20) %>%
      ggplot() +
      geom_bar(aes(reorder(country,mean),mean),stat='identity') +
      coord_flip() +
      theme_minimal() +
      labs(x = "",
           y = "Average density of not publicly tracked fishing vessels inside MPAs (number of detections/image/km²)"))
  
  ggsave(Fig_S24, file = "Figures/Supp/Fig_S24.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Table S 9
  # Results of the Wilcoxon test on the fraction of unmatched fishing vessels
  # inside MPAs with a declared IUCN category and outside. 
  
  #Test stat ratio
  Table_S9_stat <-  SAR_stats %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    distinct(id_iucn, .keep_all = T) %>%
    bind_rows(SAR_eez_final %>% distinct(MRGID_SOV1, sum_all,relative_sum_all, unmatched_ratio,unmatched_relative,iucn_cat))
  
  #Test
  res.kruskal_ratio <- Table_S9_stat %>% kruskal_test(unmatched_ratio ~ iucn_cat)
  res.kruskal_ratio
  
  Table_S9 <- Table_S9_stat %>% wilcox_test(unmatched_ratio ~ iucn_cat, p.adjust.method = "bonferroni")
  write.csv(Table_S9, "Figures/supp/Table_S9.csv") 
  
  # Fig. S 25
  # Fraction of not publicly tracked fishing vessels inside MPAs with and without a management plan 
  # according to the WDPA database.
  
  (Fig_S25 <- mpa_wdpa_no_sf %>%
      mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
      distinct(id_iucn, .keep_all = T) %>%
      mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
      dplyr::select(id, management_plan) %>%
      inner_join(MPA_data, by = "id") %>%
      group_by(management_plan) %>%
      na.omit() %>%
      ggplot() +
      geom_boxplot(aes(management_plan,unmatched_ratio)) +
      # coord_flip() +
      theme_minimal() +
      labs(x = " ",
           y = "Fraction of not publicly tracked fishing vessels"))
  
  stat_test_3 <-  mpa_wdpa_no_sf %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(id_iucn, .keep_all = T) %>%
    mutate(management_plan = ifelse(mang_plan %in% c(" Management plan is not implemented and not available","No","Management plan is not implented and not available","Not Reported"),"No management plan","Management plan")) %>%
    dplyr::select(id, management_plan) %>%
    inner_join(MPA_data, by = "id")
  
  t.test(unmatched_ratio ~ management_plan, data = stat_test_2)

  ggsave(Fig_S25, file = "Figures/Supp/Fig_S25.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  
  # Fig. S 26
  # Fraction of not publicly tracked fishing vessels inside MPAs (log-scaled)
  # by MPA creation year according to the WDPA database. 
  
  (Fig_S26 <- MPA_data %>%
      mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
      # mutate(status_yr = as.factor(status_yr),group =) %>%
      ggplot() +
      geom_boxplot(aes(status_yr,unmatched_ratio,group =status_yr)) +
      theme_minimal() +
      labs(x = "MPA creation year",
           y = "Fraction of not publicly tracked fishing vessels inside MPAs"))

  ggsave(Fig_S26, file = "Figures/Supp/Fig_S26.jpg", width = 297, height = 210, dpi = 300, units = "mm") 
  

  # Fig. S 27
  # Top 20 countries with the highest fraction of not publicly tracked fishing vessels (in thousands) 
  # detected inside MPAs (as compared to the total number of not publicly tracked fishing vessels detected inside the EEZ). 
  # Countries are ordered by total number of not publicly tracked fishing vessels detected inside the EEZ on this graph 
  # and filled by fishing vessels detected inside MPAs and outside MPAs. Germany was the country with the highest
  # fraction of fishing vessels detected inside MPAs, with 80% of all fishing vessels detected inside EEZs.  
  
  (Fig_S27 <- SAR_eez_final %>%
    distinct(unique_id, .keep_all = T) %>%
    dplyr::mutate(type = "outside_mpa") %>%
    dplyr::rename(parent_iso = "ISO_TER1") %>%
    dplyr::select(parent_iso, matched_category, type) %>%
    filter(matched_category=="unmatched") %>%
    bind_rows(SAR_inside_mpas) %>%
    #Group by type and parent_iso
    group_by(type, parent_iso) %>%
    reframe(sum_country = n()) %>%
    ungroup() %>%
    #Widen data to calculate some stuff
    pivot_wider(names_from = "type",values_from="sum_country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(sum_country = inside_mpa + outside_mpa,
           fraction_country = inside_mpa/(outside_mpa+inside_mpa)) %>%
    filter(!parent_iso %in% c("ABNJ","FRA;ITA;MCO","NLD;DEU;DNK","PLW")) %>%
    arrange(-fraction_country) %>%
    head(20) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    #Pivot longer for plots
    pivot_longer(cols = c("outside_mpa","inside_mpa")) %>%
    ggplot() +
    geom_bar(aes(reorder(country,-value), value/1000, fill = name),stat="identity") +
      scale_fill_hp_d(option="Ravenclaw",labels = c("inside_mpa" = "Inside MPA", "outside_mpa"="Outside MPA"))+
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = " ",
         y = "Number of not publicly tracked fishing vessels detected inside EEZ (in thousands)",
         fill = "Fraction of not publicly tracked fishing vessels detected: ") +
    theme(legend.position = "top"))
  
  ggsave(Fig_S27, file = "Figures/Supp/Fig_S27.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
 

  country_vessel_size <- SAR_stats %>%
    st_drop_geometry() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) %>%
    distinct(unique_id, .keep_all = T) %>%
    group_by(iso3) %>%
    reframe(number_of_detections = n(),
            mean_vessel_size = mean(length_m),
            sd_vessel_siez = sd(length_m)) %>%
    ungroup()
  
  
    
}

