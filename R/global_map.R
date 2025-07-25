#' Generate and Save Global and Regional Maps of SAR Fishing Detections
#'
#' This function creates a **global fishing detection map** and detailed **regional maps** 
#' for key areas with significant fishing activity based on SAR data.
#'
#' @return Saves the following outputs:
#' - `figures/global_map.png`: Global map showing MPAs, EEZs, and SAR detections.
#' - `figures/north_west_mediterranean_map.jpg`: Fishing detections in the **North-West Mediterranean**.
#' - `figures/hokkaido_map.jpg`: Fishing detections in **Hokkaidō, Japan**.
#' - `figures/great_barrier_reef_map.jpg`: Fishing detections near the **Great Barrier Reef**.
#' - `figures/central_america_map.jpg`: Fishing detections around **Central America**.
#' - `figures/north_sea_map.jpg`: Fishing detections in the **North Sea**.
#'
#' @details
#' 1. **Processes SAR detection data** (`full_SAR_data`):
#'    - Joins matched detections to create a comprehensive dataset.
#'    - Classifies detections by continent and refines North and South America.
#'    - Computes **percentage of tracked fishing activity** at a **continental level**.
#' 2. **Generates a global map**:
#'    - Displays **coastlines, EEZs, MPAs**, and **SAR detections**.
#'    - Uses a custom color palette to differentiate tracked and untracked fishing activity.
#'    - Saves as a **high-resolution PNG**.
#' 3. **Defines bounding boxes for key regions**:
#'    - **North-West Mediterranean** (Europe).
#'    - **Hokkaidō, Japan**.
#'    - **Great Barrier Reef, Australia**.
#'    - **Central America**.
#'    - **North Sea**.
#' 4. **Creates regional maps**:
#'    - Crops SAR, MPA, and world coastline data to bounding boxes.
#'    - Highlights **fishing activity** using color-coded detections.
#'    - Saves each **regional map as a high-resolution JPEG**.

global_map <- function(){
  
  #full SAR data
  full_SAR_data <- SAR_stats %>%
    bind_rows(SAR_eez_stats %>% dplyr::rename(parent_iso = "ISO_SOV1")) %>%
    distinct(unique_id, .keep_all = T) %>%
    left_join(SAR_data_sf %>% dplyr::select(unique_id), by = "unique_id") %>%
    st_as_sf() %>%
    mutate(continent = countrycode(parent_iso, origin = "iso3c", destination = "continent")) %>%
    filter(!is.na(continent))
  
  #Divide south and north america
  north_america <- c("USA", "CAN", "MEX", "BLZ", "GTM", "SLV", "HND", "NIC", "CRI", "PAN",
                     "BHS", "CUB", "DOM", "HTI", "JAM", "TTO", "BRB", "GRD", "LCA", "VCT",
                     "DMA", "ATG", "KNA")
  
  south_america <- c("BRA", "ARG", "CHL", "COL", "PER", "VEN", "ECU", "BOL", "PRY", "URY", 
                     "GUY", "SUR")
  
  
  full_SAR_data$continent <- ifelse(full_SAR_data$parent_iso %in% north_america, "North America", 
                         ifelse(full_SAR_data$parent_iso %in% south_america, "South America", full_SAR_data$continent))
  
  #Region wide analysis
  region_SAR <- full_SAR_data %>%
    st_drop_geometry() %>%
    group_by(continent, matched_category) %>%
    summarize(continent_count = sum(normalized_detection,na.rm=T)) %>%
    ungroup() %>%
    pivot_wider(names_from = "matched_category", values_from = "continent_count") %>%
    mutate(percentage_tracked = fishing/(fishing+unmatched) * 100)
  
  
  #Prep data for map
  colors <- c("#ffc6c4", "#f4a3a8", "#e38191", "#cc607d", "#ad466c", "#8b3058", "#672044")
  
  coastline <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") %>%
    sf::st_transform(crs = "ESRI:54030")  # Convert to projected CRS
  
  study_area <- st_read("data/study_area_clean.shp")
  
  #Map
  global_map <- ggplot() +
    geom_sf(data = coastline, fill = "#F5F5F5", lwd = 0.01) +
    geom_sf(data = eez, fill = NA, color = "black", lwd = 0.05) + 
    geom_sf(data = mpa_wdpa %>% sf::st_transform(crs = "ESRI:54030"),
            fill = "#43a9d1", color = "black", lwd = 0.05) + 
    theme_void() +
    theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) 
  
  #Coverage of MPAs on SAR images
  MPAs_included <- ggplot() + 
    geom_sf(data = coastline, fill = "#F5F5F5", lwd = 0.01) +
    geom_sf(data = eez, fill = NA, color = "black", lwd = 0.05) + 
    geom_sf(data = mpas_all %>% sf::st_transform(crs = "ESRI:54030"), fill = "darkred", lwd = 0.05) +
    geom_sf(data = mpa_wdpa %>% sf::st_transform(crs = "ESRI:54030"), fill = "forestgreen", lwd = 0.05) +
    theme_map()
  
  #Coverage of MPAs on SAR images
  MPAs_included <- ggplot() + 
    geom_sf(data = coastline %>% st_transform(crs = 4326), fill = "#F5F5F5", lwd = 0.01) +
    geom_sf(data = eez%>% st_transform(crs = 4326), fill = NA, color = "black", lwd = 0.05) + 
    geom_sf(data = mpas_all, fill = "darkred", lwd = 0.05) +
    geom_sf(data = mpa_wdpa, fill = "forestgreen", lwd = 0.05) +
    theme_map()
  
  ggsave(MPAs_included, file = "figures/supp/MPAs_included.jpg", width = 1920 * 4, height = 1080 * 4, units = "px", dpi = 300)
  
  ggsave(global_map, 
         file = "figures/global_map.png",
         width = 297*1.5,
         height = 105*1.5,
         dpi = 600,
         units = "mm")
  
  ggsave(global_map, 
         file = "figures/global_map.png",
         width = 1920 * 2,
         height = 1080 * 2,
         dpi = 600,
         units = "px")
  

  #Now create inset maps
  world_4326 <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") 
  
  # Function to crop data and create a ggplot map for a specific region
  plot_region_map <- function(full_SAR_data, mpa_wdpa, world_4326, bbox, region_name) {
  
    # Crop the data for the specified region
    cropped_data <- list(
      SAR = st_crop(full_SAR_data, bbox),
      MPA = st_crop(mpa_wdpa, bbox),
      World = st_crop(world_4326, bbox)
    )
    
    #Matched category color
    matched_category_colors <- c("fishing" = "#E69F00", "unmatched" = "black") 
    
    # Create the ggplot
    region_map <- ggplot() +
      geom_sf(data = cropped_data$MPA, fill = "#43a9d1", color = "black") + 
      # geom_sf(data = cropped_data$area, fill = "lightgrey", color = "darkred", lwd = 0.5,alpha = 0.2) +
      geom_sf(data = cropped_data$World, fill = "#F5F5F5", lwd = 0.1) +
      # scale_color_manual(values = legend,
      #                   breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ")) +
      # ggnewscale::new_scale_color() + 
      geom_sf(data = cropped_data$SAR, aes(color = matched_category), size = 0.3, alpha = 0.5) +
      scale_color_manual(values = matched_category_colors, na.value = "gray") +
      # coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +  # Set the limits using bbox
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
      # Add the black border using geom_rect based on bbox
      annotate("rect", 
               xmin = bbox["xmin"], xmax = bbox["xmax"], 
               ymin = bbox["ymin"], ymax = bbox["ymax"], 
               color = "black", fill = NA, linewidth = 0.5)
    
    # Save the plot using the region name
    ggsave(region_map, file = paste0("figures/", region_name, "_map.jpg"), 
           width = 297,
           height = 210,
           units = "mm",
           dpi = 600)
      
  }
  
  # Define the bounding box for the North-West Mediterranean region (in EPSG:4326)
  bbox_nw_mediterranean <- st_bbox(c(xmin = 0, ymin = 38, xmax = 21, ymax = 46), crs = st_crs(full_SAR_data))
  # Define the bounding box for Hokkaidō (Japan)
  bbox_hokkaido <- st_bbox(c(xmin = 138, ymin = 41, xmax = 147, ymax = 46), crs = st_crs(full_SAR_data))
  # Great Barrier Reef (Australia)
  bbox_great_barrier_reef <- st_bbox(c(xmin = 141, ymin = -26, xmax = 160, ymax = -10), crs = st_crs(full_SAR_data))
  # Central America
  bbox_central_america <- st_bbox(c(xmin = -120, ymin = 4, xmax = -54, ymax = 29), crs = st_crs(full_SAR_data))
  # North Sea
  bbox_north_sea<- st_bbox(c(xmin = -15, ymin = 42, xmax = 8, ymax = 64), crs = st_crs(full_SAR_data))
  
  # Plot and save the maps
  nw_mediterranean_map <- plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_nw_mediterranean, "north_west_mediterranean")
  hokkaido_map <- plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_hokkaido, "hokkaido")
  great_barrier_reef_map <- plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_great_barrier_reef, "great_barrier_reef")
  central_america_map <- plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_central_america, "central_america")
  north_sea_map <- plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_north_sea, "north_sea")
  
}
