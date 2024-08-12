calc_covariates_MPA <- function(mpa_wdpa){
  
  #Calculate CHL
  CHL_values <- calculate_CHL(mpa_wdpa)
  
  #Calculate SST
  SST_values <- calculate_SST(mpa_wdpa)
  
  #Calculate SST Anomalies
  anom_values <- calculate_anom(mpa_wdpa) 
  
  #Add SST anomaly
  mpa_covariates <- mpa_wdpa %>%
    bind_cols(SST_values, CHL_values, anom_values)
  
  #Add distance to nearest seamount
  seamounts <- st_read("data/covariates/seamounts-yesson-2019/YessonEtAl2019-Seamounts-V2.shp")
  
  mpa_centroids <- st_centroid(mpa_covariates)
  
  # Find the nearest seamount for each MPA centroid
  nearest_seamount_indices <- st_nearest_feature(mpa_centroids, seamounts)
  
  # Calculate the distance to the nearest seamount
  seamount_distance <- st_distance(mpa_centroids, seamounts[nearest_seamount_indices, ], by_element = TRUE)
  
  mpa_covariates$seamount_distance <- as.numeric(set_units(seamount_distance,km))
  
  #Calculate bathymetry
  bathymetry_rast <- terra::rast("data/covariates/bathymetry.tif")
  bathymetry_values <-  exact_extract(bathymetry_rast, mpa_covariates, 'mean', progress = TRUE)
  
  mpa_covariates$depth <- bathymetry_values
  
  #Distance from port
  dist_from_port_rast <- raster("data/covariates/distance-from-port-v20201104.tiff")
  dist_from_port <- exact_extract(dist_from_port_rast, mpa_covariates, 'mean', progress = TRUE)
  
  mpa_covariates$dist_to_port <- dist_from_port
  
  #Distance from shore
  dist_from_shore_rast <- raster("data/covariates/distance-from-shore.tif")
  dist_from_shore <- exact_extract(dist_from_shore_rast, mpa_covariates, 'mean', progress = TRUE)
  
  mpa_covariates$dist_to_shore <- dist_from_shore

  #Water area that lies within the shelf, slope, and abyssal zones
  abyssal <- raster("data/covariates/Abyssal_res_05_annual_years_2019_Clim_scen_historical_global.asc")
  shelf <- raster("data/covariates/Shelf_res_05_annual_years_2019_Clim_scen_historical_global.asc")
  slope <- raster("data/covariates/Slope_res_05_annual_years_2019_Clim_scen_historical_global.asc")
  
  abyssal_mpa <- exact_extract(abyssal, mpa_covariates, 'mean', progress = TRUE)
  shelf_mpa <- exact_extract(shelf, mpa_covariates, 'mean', progress = TRUE)
  slope_mpa <- exact_extract(slope, mpa_covariates, 'mean', progress = TRUE)
  
  mpa_covariates$abyssal <- abyssal_mpa
  mpa_covariates$shelf <- shelf_mpa
  mpa_covariates$slope <- slope_mpa
  
  #Sediment
  sediment <- raster("data/covariates/Sediment_thickness_res_05_annual_years_1997_Clim_scen_historical_global.asc")
  sediment_mpa <-exact_extract(sediment, mpa_covariates, 'mean', progress = TRUE)
  
  mpa_covariates$sediment <- sediment_mpa
  
  #Add GDP
  #Load gdp
  gdp <- raster("data/covariates/GDP_per_capita_PPP_1990_2015_v2.nc") 
  gdp <- projectRaster(gdp, crs = crs(mpa_covariates))
  
  # Convert GDP raster to points, keeping only non-NA values
  gdp_points <- rasterToPoints(gdp, spatial = TRUE)
  
  # Convert to sf object for easier handling
  gdp_points_sf <- st_as_sf(gdp_points)
  
  # Find the index of the nearest GDP point for each MPA
  nearest_gdp_indices <- st_nearest_feature(mpa_covariates, gdp_points_sf)
  
  # Extract the nearest GDP values using the indices
  mpa_covariates$gdp <- gdp_points_sf$Gross.Domestic.Production..GDP..per.capita..PPP.[nearest_gdp_indices]
  
  #Add LME and MEOW
  lme <- st_join(st_centroid(mpa_covariates), LME %>% dplyr::select(LME_NAME), join = st_nearest_feature) 
  mpa_covariates$lme <- lme$LME_NAME
  
  ecoregion <- st_join(st_centroid(mpa_covariates), MEOW %>% dplyr::select(ECOREGION), join = st_nearest_feature) 
  mpa_covariates$ecoregion <- ecoregion$ECOREGION
  
  #Add wind
  grid <- read.csv("data/global_grid.csv")
  wind <- read.csv("data/covariates/remss_wind.csv") %>%
    left_join(grid, by = "pixel_id") %>%
    st_as_sf(wkt = "geometry_wkt", crs = 4326) %>%
    dplyr::select(wind_speed_ms_mean, wind_speed_ms_sd)
  
  mpa_wind <- mpa_covariates %>%
    st_join(wind, join = st_nearest_feature)
  
  mpa_covariates$mean_wind_speed <-mpa_wind$wind_speed_ms_mean
  mpa_covariates$sd_wind_speed <-mpa_wind$wind_speed_ms_sd
  
  #Load previous covariates
  load("data/IUCN.I.VI.AMP.Rdata")
  
  old_covariates <- IUCN.I.VI.AMP %>%
    st_as_sf(coords = c("long","lat"), crs = 4326) %>%
    dplyr::select(c( HDI, MarineEcosystemDependency, travel_time)) 
  
  mpa_old_covariates <- st_join(st_centroid(mpa_covariates), old_covariates, join = st_nearest_feature)

  #Add number of boats as covariates
  MPA_SAR_data <- SAR_stats %>%
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, fishing, unmatched_fishing, sum_all)
  
  MPA_covariates <- mpa_old_covariates %>%
    left_join(MPA_SAR_data, by = "id_iucn") %>%
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    #Add binomial variable for SAR presence
    mutate(SAR_presence = as.factor(ifelse(is.na(sum_all), "No_SAR","SAR")))
  
  #Average length of fishing vessels
  average_length_all <- SAR_stats %>%
    group_by(id_iucn) %>%
    reframe(length_all = mean(length_m, na.rm=T)) %>%
    ungroup() 
  
  average_length_matched <- SAR_stats %>%
    filter(matched_category == "fishing") %>%
    group_by(id_iucn) %>%
    reframe(length_matched = mean(length_m, na.rm=T)) %>%
    ungroup() 
  
  average_length_unmatched <- SAR_stats %>%
    filter(matched_category == "unmatched") %>%
    group_by(id_iucn) %>%
    reframe(length_unmatched = mean(length_m, na.rm=T)) %>%
    ungroup() 
  
  MPA_covariates <- MPA_covariates %>%
    left_join(average_length_all, by = "id_iucn") %>%
    left_join(average_length_matched, by = "id_iucn") %>%
    left_join(average_length_unmatched, by = "id_iucn")
  
  #ADD AIS RECEPTION
  ais_reception <- read.csv("data/covariates/gfw_reception_quality.csv") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) 
  
  mpa_reception <- st_join(mpa_wdpa %>% st_centroid(), ais_reception, join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(id_iucn,ais_reception_positions_per_day_class_A,ais_reception_positions_per_day_class_B )
  
  #Add number of non fishing vessels
  # MPA_covariates <- MPA_covariates %>%
  #   left_join(mpa_SAR_non_fishing, by = "id_iucn") %>%
  #   mutate(n_non_fishing = ifelse(is.na(n_non_fishing), 0, n_non_fishing)) %>%
  #   left_join(mpa_reception, by = "id_iucn")
  
  #Add fishing effort from 2022 as a covariate
  path = (here::here("data/GFW_fishing_effort/"))
  setwd(path)
  
  # Function to load and ensure consistent data types
  load_and_cast <- function(file) {
    data <- get(load(file))
    
    # Ensure consistent data types across all relevant columns
    data <- data %>%
      mutate(
        Lat = as.numeric(Lat),  # Ensure 'Lat' is numeric
        Lon = as.numeric(Lon),  # Ensure 'Lon' is numeric (if needed)
        `Time Range` = as.character(`Time Range`),  # Ensure 'Time Range' is character
        `Vessel IDs` = as.character(`Vessel IDs`),  # Ensure 'Vessel IDs' is character
        `Apparent Fishing Hours` = as.numeric(`Apparent Fishing Hours`)  # Ensure 'Apparent Fishing Hours' is numeric
      )
    
    return(data)
  }
  
  # Load all .RData files and combine them into one dataframe
  fishing_effort_GFW_2022 <- list.files() %>%
    map_df(~ load_and_cast(.x))
  
  fishing_effort_GFW_2022_clean <- fishing_effort_GFW_2022 %>%
    clean_names() %>%
    filter(time_range == 2022) %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  #Join GFW fishing effort with MPAs
  MPA_wdpa_fishing_2022 <- mpa_wdpa %>%
    #Keep only MPAs from SAR dataset
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    dplyr::select(id_iucn) %>%
    #Join with AIS fishing effort
    st_join(fishing_effort_GFW_2022_clean) %>%
    group_by(id_iucn) %>%
    reframe(fishing_2022 = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    mutate(fishing_2022 = ifelse(is.na(fishing_2022), 0, fishing_2022)) 
  
  MPA_covariates <- MPA_covariates %>%
    left_join(MPA_wdpa_fishing_2022, by = "id_iucn") %>%
    left_join(mpa_reception, by = "id_iucn")
  
  setwd(here())

  save(MPA_covariates, file = "output/MPA_covariates.Rdata")
  
}
