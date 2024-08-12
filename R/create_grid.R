create_grid <- function(){
  
  #Load study area 
  study_area <- read.csv("data/study_area.csv") %>%
    dplyr::select(study_area) %>%
    # pivot_longer(cols = c(study_area,study_area_02,study_area_05)) %>%
    dplyr::rename(geometry = "study_area") %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    head(18713) %>%
    st_union()
  
  # find which grid points intersect `polygons` (countries) 
  # and create an index to subset from
  grd <- st_make_grid(study_area, cellsize = c(0.25,0.25), what = "polygons")
  
  index <- which(lengths(st_intersects(grd, study_area)) > 0)
  
  # subset the grid to make a fishnet
  grid <- grd[index]
  
  #Final grid
  SAR_grid <- grid %>%
    st_as_sf() %>%
    st_transform(crs = 4326) %>%
    mutate(grid_id = seq_along(1:length(grid))) %>%
    mutate(grid_area = as.numeric(set_units(st_area(.),km^2)))
  
  #Intersect with MPA data
  grid_mpa <- st_intersection(SAR_grid, mpa_wdpa)
  
  # Calculate area of each intersection in km^2
  grid_mpas_intersect <- grid_mpa %>%
    mutate(intersect_area_km2 = as.numeric(set_units(st_area(.),km^2))) %>%
    st_drop_geometry()
  
  #determine protection status in each cell
  grid_protection <- SAR_grid %>%
    left_join(
      grid_mpas_intersect %>%
        group_by(grid_id) %>%
        summarise(total_intersect_area_km2 = sum(intersect_area_km2)),
      by = "grid_id"
    ) %>%
    #Only protected if MPA overlaps with more than half the cell area
    mutate(protected = if_else(total_intersect_area_km2 > grid_area/2, TRUE, FALSE, missing = FALSE))
  
  #Assign the highest IUCN category for overlapping MPAs:
  grid_mpas_iucn <- grid_mpas_intersect %>%
    group_by(grid_id) %>%
    slice(which.min(match(iucn_cat, c("Ia", "Ib", "II", "III", "IV", "V", "VI","Not Assigned","Not Reported","Not Applicable")))) %>%
    ungroup()
  
  #final grid
  final_grid <- grid_protection %>%
    left_join(
      grid_mpas_iucn %>%
        dplyr::select(grid_id, id, iucn_cat,status_yr),
      by = "grid_id"
    ) %>%
    mutate(iucn_cat = as.factor(as.character(ifelse(protected == F, "Unprotected",
                                                    ifelse(iucn_cat %in% c("Not Assigned","Not Reported","Not Applicable") & protected == T,"No_IUCN_cat",
                                                    ifelse(iucn_cat %in% c("Ia","Ib") & protected == T,"I",iucn_cat))))))
  
  
  #Grid with country
  grid_with_country <- st_join(final_grid, eez %>% dplyr::select(SOVEREIGN1), join = st_nearest_feature) 
  
  grid_with_LME <- st_join(grid_with_country, LME %>% dplyr::select(LME_NAME), join = st_nearest_feature)
    
  grid_with_MEOW <- st_join(grid_with_LME, MEOW %>% dplyr::select(provname), join = st_nearest_feature)
  
  #Final grid
  mpa_grid <- grid_with_MEOW %>%
    distinct(grid_id, .keep_all = T)
  
}