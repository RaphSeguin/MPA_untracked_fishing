#' Calculate MPA Covariates
#'
#' This function computes various environmental, geographic, and human activity 
#' covariates for Marine Protected Areas (MPAs).
#'
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return Saves an enriched MPA dataset (`MPA_covariates.Rdata`) with the following covariates:
#' - **Environmental variables**: Chlorophyll-a (CHL), Sea Surface Temperature (SST), seamount distance, bathymetry.
#' - **Geographic variables**: Distance to port and shore.
#' - **Economic variable**: GDP per capita.
#' - **Human activity metrics**: AIS reception quality, fishing effort (2021-2024).
#' - **SAR fishing statistics**: Fishing activity for 2022-2024.
#'
#' @details
#' 1. Computes CHL and SST for MPAs.
#' 2. Measures the distance to the nearest seamount.
#' 3. Extracts bathymetry, distance from ports and shore.
#' 4. Matches GDP data from a raster dataset.
#' 5. Joins precomputed covariates (HDI, ecosystem dependency, travel time, human footprint).
#' 6. Integrates SAR-based fishing data (2022-2024).
#' 7. Incorporates AIS reception data for MPAs.
#' 8. Adds GFW-derived fishing effort (2021-2024).
#'
#' @examples
#' calc_covariates_MPA(mpa_wdpa)
#'

calc_covariates_MPA <- function(mpa_wdpa){
  
  #Calculate CHL
  CHL_values <- calculate_CHL(mpa_wdpa)
  save(CHL_values, file = "output/CHL_values.Rdata")
  
  #Calculate SST
  SST_values <- calculate_SST(mpa_wdpa)
  save(SST_values, file = "output/SST_values.Rdata")
  
  #Calculate SST Anomalies
  # anom_values <- calculate_anom(mpa_wdpa) 
  
  #Add SST anomaly
  mpa_covariates <- mpa_wdpa %>%
    bind_cols(SST_values, CHL_values)
  
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

  #Add GDP
  #Load gdp
  gdp <- raster("data/covariates/GDP_per_capita_PPP_1990_2015_v2.nc")
  gdp <- projectRaster(gdp, crs = crs(mpa_covariates))
  #
  # Convert GDP raster to points, keeping only non-NA values
  gdp_points <- rasterToPoints(gdp, spatial = TRUE)

  # Convert to sf object for easier handling
  gdp_points_sf <- st_as_sf(gdp_points)

  # Find the index of the nearest GDP point for each MPA
  nearest_gdp_indices <- st_nearest_feature(mpa_covariates, gdp_points_sf)

  # Extract the nearest GDP values using the indices
  mpa_covariates$gdp <- gdp_points_sf$Gross.Domestic.Production..GDP..per.capita..PPP.[nearest_gdp_indices]
  # 
  # Load previous covariates
  load("data/IUCN.I.VI.AMP.Rdata")

  old_covariates <- IUCN.I.VI.AMP %>%
    st_as_sf(coords = c("long","lat"), crs = 4326) %>%
    dplyr::select(c( HDI, MarineEcosystemDependency, travel_time, hf))

  MPA_covariates <- st_join(st_centroid(mpa_covariates), old_covariates, join = st_nearest_feature)

  #Add number of boats as covariates
  MPA_SAR_data_2022 <- SAR_stats_2022 %>%
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, 
                  fishing_2022 = fishing, 
                  unmatched_fishing_2022 = unmatched_fishing, 
                  sum_all_2022 = sum_all)
  
  MPA_SAR_data_2023 <- SAR_stats_2023 %>%
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, 
                  fishing_2023 = fishing, 
                  unmatched_fishing_2023 = unmatched_fishing, 
                  sum_all_2023 = sum_all)
  
  MPA_SAR_data_2024 <- SAR_stats_2024 %>%
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, 
                  fishing_2024 = fishing, 
                  unmatched_fishing_2024 = unmatched_fishing, 
                  sum_all_2024 = sum_all)
  
  MPA_covariates <- MPA_covariates %>%
    left_join(MPA_SAR_data_2022, by = "id_iucn") %>%
    left_join(MPA_SAR_data_2023, by = "id_iucn") %>%
    left_join(MPA_SAR_data_2024, by = "id_iucn") %>%
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) 
  
  #ADD AIS RECEPTION
  ais_reception <- read.csv("data/covariates/gfw_reception_quality.csv") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326) 
  
  mpa_reception <- st_join(mpa_wdpa %>% st_centroid(), ais_reception, join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    dplyr::select(id_iucn,ais_reception_positions_per_day_class_A,ais_reception_positions_per_day_class_B )
  
  #Add fishing effort from 2021 and 2022 as a covariate
  MPA_wdpa_fishing_2021 <- process_fishing_effort(mpa_wdpa, 2021)
  MPA_wdpa_fishing_2022 <- process_fishing_effort(mpa_wdpa, 2022)
  MPA_wdpa_fishing_2023 <- process_fishing_effort(mpa_wdpa, 2023)
  MPA_wdpa_fishing_2024 <- process_fishing_effort(mpa_wdpa, 2024)
  
  MPA_covariates <- MPA_covariates %>%
    left_join(MPA_wdpa_fishing_2021, by = "id_iucn") %>%
    left_join(MPA_wdpa_fishing_2022, by = "id_iucn") %>%
    left_join(MPA_wdpa_fishing_2023, by = "id_iucn") %>%
    left_join(MPA_wdpa_fishing_2024, by = "id_iucn") %>%
    left_join(mpa_reception, by = "id_iucn")

  save(MPA_covariates, file = "output/MPA_covariates.Rdata")
  
}
