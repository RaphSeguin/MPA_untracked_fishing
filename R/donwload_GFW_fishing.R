donwload_GFW_fishing <- function(){

  #Key
  key <- gfw_auth()
  
  #grid
  grid <- read.csv("data/emlab-ucsb/emlab-ucsb-mpa-fishing-effort-redistribution-2c931c4/data/model_features/global_grid.csv") %>%
    st_as_sf(wkt = "geometry_wkt", crs = 4326)
  
  study_area_clean <- st_read("data/study_area_clean.shp")
  
  #Grid clean with SAR footprint
  grid_clean <- grid %>%
    st_intersection(study_area_clean) %>%
    st_make_valid()
  
  #Join polygons for simpler geometry
  grid_final <- grid_clean %>%
    mutate(group_id = as.factor(floor((row_number() - 1) / (n() / 100)))) %>%
    group_by(group_id) %>%
    summarise(geometry = st_union(geometry_wkt)) %>%
    st_as_sf() %>%  # Ensure the result is an sf object
    ungroup() %>%
    st_make_valid()
  
  grid_final_geometry <- grid_final$geometry
  
  failed_iterations <- c()

  # Download GFW fishing effort in two steps (first and second half of the year)
  fishing_effort_2024 <- lapply(44:length(grid_final_geometry), function(i) {
    
    Sys.sleep(30)
    
    print(paste0("Actual I: ", i))
    
    temp_geometry <- grid_final_geometry[i] %>% st_as_sf()
    
    print("Downloading first half...")
    
    # Download for the first half of the year
    fishing_effort_mpa_first_half <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'MONTHLY',
                 group_by = 'FLAG',
                 start_date = "2022-01-01",
                 end_date = "2022-06-30",
                 region = temp_geometry,
                 region_source = 'USER_SHAPEFILE',
                 key = key)
    }, error = function(e) {
      # Append the index to failed_iterations if an error occurs
      failed_iterations <<- c(failed_iterations, paste0(i, "_first_half"))
      # Return NULL to skip the save step
      return(NULL)
    })
    
    # Save the first half data if available
    if (!is.null(fishing_effort_mpa_first_half)) {
      save(fishing_effort_mpa_first_half, file = paste0("data/GFW_fishing_effort_grid/2022_fishing_effort_first_half_", i, ".Rdata"))
    }

    Sys.sleep(30)
    print("Downloading second half...")
    
    # Download for the second half of the year
    fishing_effort_mpa_second_half <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'MONTHLY',
                 group_by = 'FLAG',
                 start_date = "2024-07-01",
                 end_date = "2024-12-31",
                 region = temp_geometry,
                 region_source = 'USER_SHAPEFILE',
                 key = key)
    }, error = function(e) {
      # Append the index to failed_iterations if an error occurs
      failed_iterations <<- c(failed_iterations, paste0(i, "_second_half"))
      # Return NULL to skip the save step
      return(NULL)
    })
    
    # Save the second half data if available
    if (!is.null(fishing_effort_mpa_second_half)) {
      save(fishing_effort_mpa_second_half, file = paste0("data/GFW_fishing_effort_grid/2024_fishing_effort_second_half_", i, ".Rdata"))
    }
    })
  
  
}