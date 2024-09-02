plot_mpa_cases <- function(){
  
  #555643633_Not Reported
  #Talus du golfe de gascogne
  
  #Mpa polygon
  MPA_polygon <- mpa_wdpa %>%
    filter(id_iucn == "555643633_Not Reported")
  
  #SAR points
  SAR_MPA_points <- SAR_stats %>%
    filter(id_iucn == "555643633_Not Reported") %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326)
  
  #France
  france <- ne_countries(scale = "large",type = "countries",country = "France", returnclass = "sf")
  
  #Create grid
  grid <- st_make_grid(MPA_polygon, cellsize = 0.01)
  grid_within_polygon <- st_intersection(grid, MPA_polygon)
  grid_sf <- st_sf(geometry = grid_within_polygon) %>%
    mutate(ID = row_number()) 
  
  #Get fishing effort from GFW
  fishing_effort_mpa_2022 <- get_raster(spatial_resolution = 'HIGH',
               temporal_resolution = 'YEARLY',
               group_by = 'FLAG',
               start_date = "2022-01-01",
               end_date = "2022-12-31",
               region = 555643633,
               region_source = 'MPA',
               key = key)
  
  fishing_effort_mpa_2023 <- get_raster(spatial_resolution = 'HIGH',
                                        temporal_resolution = 'YEARLY',
                                        group_by = 'FLAG',
                                        start_date = "2023-01-01",
                                        end_date = "2023-12-31",
                                        region = 555643633,
                                        region_source = 'MPA',
                                        key = key)
  
  fishing_effort_mpa_full <- bind_rows(fishing_effort_mpa_2022, fishing_effort_mpa_2023) %>%
    st_as_sf(coords = c("Lon","Lat"), crs = 4326) 

  #Join fishing effort with grid
  fishing_effort_grid <- grid_sf %>%
    st_join(fishing_effort_mpa_full) %%>
    
  
  #Matched only
  ggplot() + 
    geom_sf(data= france, fill = "darkgrey") +
    geom_sf(data = MPA_polygon, fill = "lightblue") +
    geom_sf(data = SAR_MPA_points %>% filter(matched_category == "fishing"),shape = ".", color = "#107733") +
    xlim(-10, 0) +
    ylim(44,50) + 
    theme_void()
  
  #ALL sar detections
  ggplot() + 
    geom_sf(data= france, fill = "darkgrey") +
    geom_sf(data = MPA_polygon, fill = "lightblue") +
    geom_sf(data = SAR_MPA_points, aes(color = matched_category),shape = ".") + 
    scale_colour_manual(values = c("unmatched"="#000000",
                                   "fishing"="#107733"),
                        labels = c("unmatched"="Not publicly tracked",
                                   "fishing"="Publicly tracked")) +
    xlim(-10, 0) +
    ylim(44,50) + 
    theme_void()
  

  
  
}