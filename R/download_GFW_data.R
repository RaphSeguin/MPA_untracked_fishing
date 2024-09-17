download_GFW_data <- function(mpa_wdpa){
  
  key <- gfw_auth()
  
  #Group MPA geoemtries by country for faster download
  mpa_wdpa_union_country <- mpa_wdpa %>%
    group_by(iso3) %>%
    summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_make_valid()
  
  mpa_geometry <- mpa_wdpa_union_country$geometry
  
  #2021
  # Initialize a vector to store the indices of failed iterations
  #missing
  failed_iterations <- c()
  
  # Download GFW fishing effort
  fishing_effort_2021 <- lapply(63:length(mpa_geometry), function(i){
    
    Sys.sleep(15)
    
    print(paste0("Actual I: ", i))
    
    temp_geometry <- mpa_geometry[i] %>%  st_as_sf()
    
    # Fishing effort
    fishing_effort_mpa <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'YEARLY',
                 group_by = 'FLAG',
                 start_date = "2021-01-01",
                 end_date = "2021-12-31",
                 region = 8453,
                 region_source = 'EEZ',
                 key = key)
    }, error = function(e) {
      # If an error occurs, append the index to failed_iterations
      failed_iterations <<- c(failed_iterations, i)
      # Return NULL to skip the save step
      return(NULL)
    })
    
    # If fishing_effort_mpa is not NULL, save the object
    if (!is.null(fishing_effort_mpa)) {
      save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/2021_fishing_effort_mpa",i,".Rdata"))
    }
  })
  
  #2022
  # Initialize a vector to store the indices of failed iterations
  failed_iterations <- c()
  
  # Download GFW fishing effort
  fishing_effort_2023 <- lapply(1:length(mpa_geometry), function(i){
    
    Sys.sleep(15)
    
    print(paste0("Actual I: ", i))
    
    temp_geometry <- mpa_geometry[i] %>%  st_as_sf()
    
    # #Temp for solving
    # fishing_effort_mpa <-  get_raster(spatial_resolution = 'HIGH',
    #                                   temporal_resolution = 'YEARLY',
    #                                   group_by = 'FLAG',
    #                                   start_date = "2022-01-01",
    #                                   end_date = "2022-12-31",
    #                                   region = 8463,
    #                                   region_source = 'EEZ',
    #                                   key = key)
    # 
    # save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/2022_fishing_effort_mpa",i,".Rdata"))
    
    # Fishing effort
    fishing_effort_mpa <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'YEARLY',
                 group_by = 'FLAG',
                 start_date = "2022-01-01",
                 end_date = "2022-12-31",
                 region = temp_geometry,
                 region_source = 'USER_SHAPEFILE',
                 key = key)
    }, error = function(e) {
      # If an error occurs, append the index to failed_iterations
      failed_iterations <<- c(failed_iterations, i)
      # Return NULL to skip the save step
      return(NULL)
    })
    
    # If fishing_effort_mpa is not NULL, save the object
    if (!is.null(fishing_effort_mpa)) {
      save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/2022_fishing_effort_mpa",i,".Rdata"))
    }
  })
  
  #2023
  # Initialize a vector to store the indices of failed iterations
  failed_iterations <- c()
  
  # Download GFW fishing effort
  fishing_effort_2023 <- lapply(1:length(mpa_geometry), function(i){
    
    Sys.sleep(15)
    
    print(paste0("Actual I: ", i))
    
    temp_geometry <- mpa_geometry[i] %>%  st_as_sf()
    # 
    # plot_thing <- ggplot() + geom_sf(data = temp_geometry, fill = "lightblue")
    # ggsave(plot_thing, file = "plot_thing.jpg")
    # # 
    # # #Temp for solving
    fishing_effort_mpa <-  get_raster(spatial_resolution = 'HIGH',
                                      temporal_resolution = 'YEARLY',
                                      group_by = 'FLAG',
                                      start_date = "2023-01-01",
                                      end_date = "2023-12-31",
                                      region = temp_geometry,
                                      region_source = 'USER_SHAPEFILE',
                                      key = key)
  
    # # 
    # save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/2023_fishing_effort_mpa",i,".Rdata"))
    
    # Fishing effort
    fishing_effort_mpa <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'YEARLY',
                 group_by = 'FLAG',
                 start_date = "2023-01-01",
                 end_date = "2023-12-31",
                 region = temp_geometry,
                 region_source = 'USER_SHAPEFILE',
                 key = key)
    }, error = function(e) {
      # If an error occurs, append the index to failed_iterations
      failed_iterations <<- c(failed_iterations, i)
      # Return NULL to skip the save step
      return(NULL)
    })
    
    # If fishing_effort_mpa is not NULL, save the object
    if (!is.null(fishing_effort_mpa)) {
      save(fishing_effort_mpa, file = paste0("data/GFW_fishing_effort/2023_fishing_effort_mpa",i,".Rdata"))
    }
  })

  

  
 }