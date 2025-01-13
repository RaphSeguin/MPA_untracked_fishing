prep_mpa_data <- function() {

  #Download worldwide MPAs from WDPA
  world_mpas <- wdpa_fetch("global", wait = TRUE,
                           download_dir = rappdirs::user_data_dir("wdpar"))
  
  #Moll equal area projection for estimates
  moll_proj <- "+proj=moll +datum=WGS84 +lon_0=0 +x_0=0 +y_0=0 +units=m +no_defs"
  
  # Unionize all EEZ
  # Simplify geometries and make union valid
  eez_union <- eez %>%
    st_transform(crs = moll_proj) %>%
    st_simplify(dTolerance = 1000) %>%
    st_make_valid() %>%
    st_union()
  
  # Cleaning and preprocessing the MPAs database
  world_mpas_clean <- world_mpas %>%
    # Filter for marine MPAs only and those created before 2022
    dplyr::filter(MARINE %in% c(1, 2), STATUS_YR < 2022, ISO3 != "ABNJ") %>%
    clean_names() %>%
    # Filter relevant statuses and remove UNESCO biosphere reserves
    dplyr::filter(status %in% c("Designated", "Inscribed", "Established"), desig_eng != "UNESCO-MAB Biosphere Reserve") %>%
    # Rename and add geometry type, keeping only polygons
    dplyr::rename(parent_iso = parent_iso3) %>%
    dplyr::mutate(geometry_type = st_geometry_type(.) %in% c("POINT", "MULTIPOINT"), geometry_type = ifelse(geometry_type, "POINT", "POLYGON")) %>%
    # The previous geometry type detection was simplified using `st_geometry_type(.)`
    dplyr::filter(geometry_type == "POLYGON", !sf::st_is_empty(.)) %>%
    # Set factor levels for IUCN categories and trim management plan field
    dplyr::mutate(
      iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned")),
      mang_plan = trimws(mang_plan)
    )

  # # Remove identical polygons and intersect with EEZs
  # world_mpas_clean_final <- world_mpas_clean %>%
  #   # Reproject for intersections
  #   st_transform(crs = moll_proj) %>%
  #   # Remove duplicate geometries by selecting the strictest IUCN category
  #   group_by(geometry) %>% arrange(iucn_cat) %>% slice(1) %>% ungroup() %>%
  #   st_make_valid() %>%
  #   # Intersect with EEZ
  #   st_intersection(eez_union) %>%
  #   # Recalculate area and filter out polygons smaller than 1 km^2
  #   dplyr::mutate(area_clean = as.numeric(set_units(st_area(.), "km2"))) %>%
  #   dplyr::filter(area_clean > 1)
  # 
  # # Keep MPAs with duplicate names and covering similar areas
  # duplicate_names <- world_mpas_clean_final %>%
  #   dplyr::mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
  #   dplyr::group_by(name) %>%
  #   dplyr::filter(n() > 1, all(abs(area_clean - first(area_clean)) <= 10)) %>%
  #   dplyr::ungroup() %>%
  #   st_drop_geometry()
  # 
  # # In these MPAs, keep either strictest IUCN category or largest MPA
  # iucn_rank <- c("Ia" = 1, "Ib" = 2, "II" = 3, "III" = 4, "IV" = 5, "V" = 6, "VI" = 7, "Not Reported" = 99, "Not Applicable" = 99, "Not Assigned" = 99)
  # 
  # filtered_data <- duplicate_names %>%
  #   dplyr::mutate(iucn_rank = iucn_rank[as.character(iucn_cat)]) %>%
  #   dplyr::group_by(name) %>%
  #   dplyr::arrange(iucn_rank, desc(area_clean)) %>%  # Sort by IUCN rank and area
  #   dplyr::slice(1) %>%  # Keep the first row (strictest or largest)
  #   dplyr::ungroup()
  # 
  # # Full dataset without duplicates
  # final_mpas <- world_mpas_clean_final %>%
  #   dplyr::mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
  #   anti_join(duplicate_names, by = "name") %>%
  #   dplyr::bind_rows(filtered_data) %>%
  #   st_make_valid() %>%
  #   st_transform(crs = 4326) %>%
  #   st_make_valid()
  
  #--------
  
  mpa_wdpa <- world_mpas_clean %>% st_transform(crs = 4326)
  
  #Create buffer 1km from shoreline
  ROI = ne_countries(returnclass = 'sf',scale="large") %>%
    st_combine()
  
  buffer_in_km <- 1
  buffer_as_arc_degrees<- buffer_in_km/40075*360
  
  coastalWaters = ROI %>%
    st_buffer(buffer_as_arc_degrees)  %>% st_wrap_dateline() %>% st_make_valid()
  
  #Removing parts of MPA 1km within shoreline
  mpa_wdpa_nocoastline <- st_difference(mpa_wdpa,coastalWaters)
  
  save(mpa_wdpa_nocoastline, file = "data/mpa_wdpa_nocoastline.Rdata")
  
  #Now keeping only MPAs which fall withing range of study area
  # #downloaded from GFW
  # load("data/SAR_footprints.Rdata")
  # 
  # # Step 1: Convert to sf and union the geometries
  # study_area <- SAR_footprints %>%
  #   st_as_sf(wkt = "footprint_wkt", crs = 4326) 
  # 
  # st_write(study_area, dsn = "data/study_area.shp")
  # 
  # study_area <- st_read("data/study_area.shp")
  # 
  # study_area_clean <- study_area %>%
  #   st_simplify(dTolerance = 0.1) %>%
  #   st_buffer(0.1) %>%
  #   st_union() 
  # 
  # st_write(study_area_clean, dsn = "data/study_area_clean.shp")
  
  study_area_clean <- st_read("data/study_area_clean.shp")

  mpa_wdpa_nocoastline_clean <- mpa_wdpa_nocoastline %>%
    #remove duplicated values of id
    group_by(wdpaid) %>%
    filter(n() == 1) %>%
    ungroup()
  
  mpa_wdpa_study_area <- st_intersection(mpa_wdpa_nocoastline_clean, study_area_clean) %>%
    st_make_valid() 
  
  mpa_wdpa_online <- mpa_wdpa_study_area %>%
    #Only keeping MPAs with more than 1km^2 surface
    #Calculating new area
    mutate(area_correct = set_units(st_area(.),km^2),
           area_correct = as.numeric(area_correct)) %>%
    filter(area_correct > 1) %>%
    st_make_valid()  %>%
    #remove duplicated values of id
    group_by(wdpaid) %>%
    filter(n() == 1) %>%
    ungroup()
  
  #Clean database
  mpa_wdpa_clean <- mpa_wdpa_online %>%
    # Remove duplicate geometries by selecting the strictest IUCN category
    group_by(geometry) %>% arrange(iucn_cat) %>% slice(1) %>% ungroup() %>%
    st_make_valid() 
  
  # Keep MPAs with duplicate names and covering similar areas
  duplicate_names <- mpa_wdpa_clean %>%
    dplyr::mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
    dplyr::group_by(name) %>%
    dplyr::filter(n() > 1, all(abs(area_correct - first(area_correct)) <= 5)) %>%
    dplyr::ungroup() %>%
    st_drop_geometry()
  
  # In these MPAs, keep either strictest IUCN category or largest MPA
  iucn_rank <- c("Ia" = 1, "Ib" = 2, "II" = 3, "III" = 4, "IV" = 5, "V" = 6, "VI" = 7, "Not Reported" = 99, "Not Applicable" = 99, "Not Assigned" = 99)
  
  filtered_data <- duplicate_names %>%
    dplyr::mutate(iucn_rank = iucn_rank[as.character(iucn_cat)]) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(iucn_rank, desc(area_correct)) %>%  # Sort by IUCN rank and area
    dplyr::slice(1) %>%  # Keep the first row (strictest or largest)
    dplyr::ungroup()
  
  # Full dataset without duplicates
  final_mpas <- mpa_wdpa_clean %>%
    dplyr::mutate(name = stri_trans_general(name, "Latin-ASCII")) %>%
    anti_join(duplicate_names, by = "name") %>%
    dplyr::bind_rows(filtered_data) %>%
    st_make_valid() %>%
    st_transform(crs = 4326) %>%
    st_make_valid()
  
  mpa_wdpa <- final_mpas
  
  #remove duplicate geometries (MPAs that have exactly the same geometry but maybe have diffrenet management regimes)
  # mpa_centroids <- st_centroid(mpa_wdpa)
  # 
  # mpa_wdpa_no_duplicates <- mpa_wdpa[!duplicated(mpa_centroids$geometry), ]
  
  save(mpa_wdpa, file = "data/mpa_wdpa.Rdata")
  
  #MPA object not as sf to save space
  mpa_wdpa_no_sf <- mpa_wdpa %>% st_drop_geometry()
  save(mpa_wdpa_no_sf, file = "data/mpa_wdpa_no_sf.Rdata")

}