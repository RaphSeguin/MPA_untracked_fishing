trawlers_in_MPAs <- function(){ 
    
  #grid
  grid <- read.csv("data/emlab-ucsb/emlab-ucsb-mpa-fishing-effort-redistribution-2c931c4/data/model_features/global_grid.csv") %>%
    st_as_sf(wkt = "geometry_wkt", crs = 4326)
  
  #Grid clean with SAR footprint
  grid_clean <- grid %>%
    st_make_valid()
  
  #Join polygons for simpler geometry
  grid_final <- grid_clean %>%
    mutate(group_id = as.factor(floor((row_number() - 1) / (n() / 200)))) %>%
    group_by(group_id) %>%
    summarise(geometry = st_union(geometry_wkt)) %>%
    st_as_sf() %>%  # Ensure the result is an sf object
    ungroup() %>%
    st_make_valid()
  
  grid_final_geometry <- grid_final$geometry
  
  # Download GFW fishing effort in two steps (first and second half of the year)
  fishing_effort_2024 <- lapply(93:length(grid_final_geometry), function(i) {
    
    Sys.sleep(30)
    
    print(paste0("Actual I: ", i))
    
    temp_geometry <- grid_final_geometry[i] %>% st_as_sf()
    
    print("Downloading first half...")
    
    # Download for the first half of the year
    fishing_effort_mpa_first_half <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'MONTHLY',
                 group_by = 'VESSEL_ID',
                 start_date = "2024-01-01",
                 end_date = "2024-06-30",
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
      save(fishing_effort_mpa_first_half, file = paste0("data/trawlers_fishing/2024_fishing_effort_first_half_", i, ".Rdata"))
    }
    
    Sys.sleep(30)
    print("Downloading second half...")
    
    # Download for the second half of the year
    fishing_effort_mpa_second_half <- tryCatch({
      get_raster(spatial_resolution = 'HIGH',
                 temporal_resolution = 'MONTHLY',
                 group_by = 'VESSEL_ID',
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
      save(fishing_effort_mpa_second_half, file = paste0("data/trawlers_fishing/2024_fishing_effort_second_half_", i, ".Rdata"))
    }
  
    })
  
  
  load_and_cast <- function(file) {
    data <- get(load(file))
    
    # Ensure consistent data types across all columns
    data <- data %>%
      mutate(
        # Location columns
        Lat = as.numeric(Lat),
        Lon = as.numeric(Lon),
        
        # String columns
        `Time Range` = as.character(`Time Range`),
        `Vessel ID` = as.character(`Vessel ID`),
        Flag = as.character(Flag),
        `Vessel Name` = as.character(`Vessel Name`),
        `Gear Type` = as.character(`Gear Type`),
        `Vessel Type` = as.character(`Vessel Type`),
        CallSign = as.character(CallSign),
        
        # Numeric columns
        MMSI = as.numeric(MMSI),
        IMO = as.numeric(IMO),
        `Apparent Fishing Hours` = as.numeric(`Apparent Fishing Hours`),
        
        # DateTime columns - convert to character first to ensure consistency
        `Entry Timestamp` = as.POSIXct(as.character(`Entry Timestamp`), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
        `Exit Timestamp` = as.POSIXct(as.character(`Exit Timestamp`), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
        `First Transmission Date` = as.POSIXct(as.character(`First Transmission Date`), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
        `Last Transmission Date` = as.POSIXct(as.character(`Last Transmission Date`), format="%Y-%m-%d %H:%M:%S", tz="UTC")
      )
    
    return(data)
  }
  
  # Load all .RData files and combine them into one dataframe
  path <- here::here("data/trawlers_fishing/")
  setwd(path)
  
  # Load all .RData files and combine them into one dataframe
  fishing_effort_2024 <- list.files() %>%
    map_df(~ load_and_cast(.x))
  
  setwd(here())
  
  #Select only trawelrs in EU registry
  eu_fleet_registry <- read.csv("data/blacklist_bloom_fleet_register.csv", sep = ";") %>%
    clean_names()
  
  #Trawlers
  trawlers <- eu_fleet_registry %>%
    distinct(mmsi, .keep_all = T) %>%
    clean_names() %>%
    filter(en_description %in% c(
      
      # Bottom Trawls
      "Single boat bottom otter trawls",
      "Bottom trawls nephrops trawls",
      "Bottom pair trawls", 
      "Bottom trawls (nei)",
      "Bottom trawls shrimp trawls",
      "Twin bottom otter trawls",
      
      # Midwater Trawls
      "Single boat midwater otter trawls",
      "Midwater pair trawls",
      "Midwater shrimp trawls", 
      "Midwater trawls (nei)",
      
      #Seines
      "- Danish seines",
      "- Scottish seines"
    ))
    
  #Load GFW registry
  GFW_registry <- read.csv("data/fishing-vessels-v3.csv") %>%
    filter(vessel_class_gfw %in% c("trawlers")) %>%
    mutate(mmsi = as.factor(mmsi)) %>%
    #Keep unique MMSI wit the most recent year
    group_by(mmsi) %>%
    slice_max(order_by = year, n = 1) %>%
    distinct(mmsi, .keep_all = TRUE) %>%
    ungroup() %>%
    #Select columns of interests
    dplyr::select(mmsi, year, flag_gfw, vessel_class_gfw, length_m_gfw, engine_power_kw_gfw, tonnage_gt_gfw)
  
  #Eu countries
  eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", 
                    "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", 
                    "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", 
                    "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
  
  # Clean and filter data for the selected year
  fishing_effort_clean <- fishing_effort_2024 %>%
    clean_names() %>%
    mutate(year = str_extract(time_range, "^\\d{4}")) %>%
    filter(year == 2024) %>%
    # mutate(mmsi = as.factor(mmsi)) %>%
    #Join with EU fleet registry
    left_join(trawlers, by = "mmsi") %>%
    #Join with GFW registry
    left_join(GFW_registry, by = "mmsi") %>%
    dplyr::filter(!is.na(en_description) | vessel_class_gfw %in% c("trawlers")) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  save(fishing_effort_clean, file = "output/fishing_effort_clean_pauline.Rdata")
  
  #Download worldwide MPAs from WDPA
  world_mpas <- wdpa_fetch("global", wait = TRUE,
                           download_dir = rappdirs::user_data_dir("wdpar"))
  
  # Cleaning and preprocessing the MPAs database
  world_mpas_clean <- world_mpas %>%
    # Filter for marine MPAs only and those created before 2022
    dplyr::filter(MARINE %in% c(1, 2), STATUS_YR < 2024) %>%
    clean_names() %>%
    # Filter relevant statuses and remove UNESCO biosphere reserves
    dplyr::filter(status %in% c("Designated", "Inscribed", "Established"), desig_eng != "UNESCO-MAB Biosphere Reserve") %>%
    # Rename and add geometry type, keeping only polygons
    dplyr::rename(parent_iso = parent_iso3) %>%
    # Set factor levels for IUCN categories and trim management plan field
    dplyr::mutate(
      iucn_cat = factor(iucn_cat, levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Applicable", "Not Assigned")),
      mang_plan = trimws(mang_plan)
    )
  
  #Join with mpa data
  fishing_effort_mpa <- fishing_effort_clean %>%
    st_join(world_mpas_clean, left = F) 
  
  save(fishing_effort_mpa, file = "output/fishing_effort_mpa_pauline.Rdata")
  
  load("output/fishing_effort_mpa_pauline.Rdata")
  
  # Function to get the first non-NA value from multiple columns
  first_non_na <- function(...) {
    vals <- c(...)
    return(vals[!is.na(vals)][1])
  }
  
  # Function to determine the highest IUCN category
  highest_iucn <- function(iucn_categories) {
    iucn_order <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not Assigned", "Not Applicable")
    valid_categories <- iucn_categories[iucn_categories %in% iucn_order]
    if (length(valid_categories) > 0) {
      return(valid_categories[which.min(match(valid_categories, iucn_order))])
    } else {
      return(NA)
    }
  }
  
  # Try a more robust approach to create the union
  MPA_union <- world_mpas_clean %>%
    group_by(parent_iso) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup()
  
  fishing_effort_union <- fishing_effort_clean %>%
    st_join(MPA_union %>% st_as_sf(), left = F)
  
  save(fishing_effort_union, file = "output/fishing_effort_union_pauline.Rdata")
  
  load("output/fishing_effort_union_pauline.Rdata")
  
  #Total fishing hours in union mpas
  fishing_union_summary <- fishing_effort_union %>%
    #Remove fishing events smaller than 20 minutes
    filter(apparent_fishing_hours > 0.2) %>%
    st_drop_geometry() %>%
    group_by(mmsi) %>%
    reframe(total_fishing_hours_union = sum(apparent_fishing_hours, na.rm = TRUE)) %>%
    ungroup() 
  
  #Calculate trawled surfaces
  load("output/gear_widths.Rdata")
  
  gear_widths_clean <- gear_widths %>% 
    mutate(gear_group = as.factor(case_when(
      benthisMet %in% c("OT_SPF", "OT_DMF", "OT_MIX_DMF_BEN", "OT_MIX", "OT_MIX_DMF_PEL", "OT_MIX_CRU_DMF", "OT_MIX_CRU", "OT_CRU") ~ "OT", # Otter trawls
      benthisMet %in% c("SDN_DMF", "SSC_DMF") ~ "SDN", # Danish and Scottish seines as OT
      benthisMet %in% c("TBB_CRU", "TBB_DMF", "TBB_MOL") ~ "BT", # Beam trawls
      benthisMet %in% c("DRB_MOL") ~ "TD" # Towed dredges
    ))) %>%
    #Average fishing speed for each gear group
    group_by(gear_group) %>%
    reframe(avFspeed = mean(as.numeric(avFspeed))) %>%
    ungroup() %>%
    dplyr::select(gear_group, avFspeed) 
  
  #Surface footprint
  latlon <- st_coordinates(fishing_effort_mpa)
  
  fishing_unique_cells <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    cbind(latlon) %>%
    # Select only the columns we need
    dplyr::select(mmsi, X, Y) %>%
    # Remove duplicates to get unique combinations of mmsi, X, Y
    distinct() %>%
    # Group by mmsi and count unique cells
    group_by(mmsi) %>%
    summarize(fishing_surface = n()) %>%
    ungroup()
  
  #Most fished MPA
  # top_10_mpa <- fishing_effort_mpa %>%
  #   group_by(name, parent_iso) %>%
  #   reframe(total_fishing_hours_union = sum(apparent_fishing_hours, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   arrange(-total_fishing_hours_union) %>%
  #   head(10)
  # 
  #Final list
  fishing_summary <- fishing_effort_mpa %>%
    st_drop_geometry() %>%
    dplyr::select(mmsi, vessel_id, vessel_name, en_description, imo, apparent_fishing_hours, name, iucn_cat,
                  flag_gfw, length_m_gfw) %>%
    mutate(
      en_description = ifelse(en_description == "- Danish seines", "Danish seines", en_description)  # Fix gear name
    ) %>%
    group_by(mmsi) %>%
    summarise(
      vessel_id = first(vessel_id),
      imo = first(imo),
      flag_gfw = first(flag_gfw),
      length_m_gfw = first(length_m_gfw),
      en_description = first(en_description),
      vessel_name = first(vessel_name),
      mpa_names = paste(unique(name), collapse = ", "), # List of MPAs where vessel fished
      highest_iucn = highest_iucn(unique(iucn_cat)) # Determine the highest IUCN category
    ) %>%
    ungroup() %>%
    left_join(fishing_union_summary, by = "mmsi") %>%
    left_join(fishing_unique_cells, by = "mmsi") %>%
    dplyr::select(mmsi, imo, vessel_name, flag_gfw, length_m_gfw, en_description, 
                  total_fishing_hours_union, fishing_surface, mpa_names, highest_iucn) %>%
    # 🔽 Exclude FRA vessels <= 12m using specific dredge gears
    filter(!(flag_gfw == "FRA" & length_m_gfw <= 12 & en_description %in% c("Towed dredges", "Mechanized dredges"))) %>%
    filter(
      total_fishing_hours_union > 24, 
      vessel_name != 1,
      !is.na(vessel_name),
      !is.na(flag_gfw),
      !(length_m_gfw > 80 & is.na(imo)),
      !grepl("UNKNOWN", flag_gfw),
      !(flag_gfw %in% c("ITA", "FRA", "ESP", "DEU", "NLD", "BEL", "DNK", "GRC", "PRT", "SWE", "FIN", "IRL", "POL") &
          en_description == "Trawler - GFW")
    ) %>%
    mutate(
      en_description = ifelse(is.na(en_description), "Trawler - GFW", en_description),
      fishing_surface = ifelse(length_m_gfw > 80 & en_description == "Trawler - GFW", NA, fishing_surface),
      Uncertainty = case_when(
        !is.na(mmsi) & !is.na(imo) & en_description != "Trawler - GFW" ~ "low",
        en_description != "Trawler - GFW" & is.na(imo) ~ "medium",
        en_description == "Trawler - GFW" ~ "high"
      )
    )
  
  # Apply the validation function to the mmsi column
  fishing_summary$validation_result <- sapply(fishing_summary$mmsi, validate_mmsi)
  
  #Blabla
  fishing_summary <- fishing_summary %>%
    filter(validation_result == "Valid MMSI") %>%
    dplyr::select(-validation_result) 
  
  # Create the English version with more explicit column names and rounded fishing hours
  fishing_summary_english <- fishing_summary %>%
    rename(
      `Vessel name` = vessel_name, 
      `MMSI`= mmsi,
      `IMO` = imo,
      `Flag` = flag_gfw,
      `Vessel length (m)` = length_m_gfw,
      `Fishing gear` = en_description,
      Uncertainty = Uncertainty,
      `Total apparent fishing hours` = total_fishing_hours_union,
      `Fishing surface footprint (km^2)` = fishing_surface,
      `Fished MPAs` = mpa_names,
      `Highest IUCN category` = highest_iucn
    ) %>%
    mutate(
      # Convert flag codes to full country names
      Flag = countrycode(Flag, "iso3c", "country.name", nomatch = NA),
      Flag = ifelse(is.na(Flag),"Unknown",Flag), 
      `Vessel length (m)` = round(`Vessel length (m)`,0), 
      # Round fishing hours to 0 decimal places
      `Total apparent fishing hours` = round(`Total apparent fishing hours`, 0),
      `IMO` = ifelse(is.na(`IMO`), "", `IMO`)
    ) %>%
    arrange(-`Total apparent fishing hours`)
  
  library(writexl)
  write_xlsx(fishing_summary_english, "fishing_summary_english.xlsx")
  
  #ACCENT FRANCAIS
  
  # Create a gear type translation dictionary
  # Create a gear type translation dictionary
  gear_translation <- c(
    "Twin bottom otter trawls" = "Chaluts de fond jumeaux à panneaux",
    "Single boat bottom otter trawls" = "Chaluts de fond à panneaux simples",
    "Midwater pair trawls" = "Chaluts pélagiques en paire",
    "Towed dredges" = "Dragues remorquées",
    "Trawler - GFW" = "Chalutier - GFW",
    "Single boat midwater otter trawls" = "Chaluts pélagiques à panneaux simples",
    "Danish seines" = "Sennes danoises",
    "Bottom trawls nephrops trawls" = "Chaluts de fond à langoustines",
    "Midwater trawls (nei)" = "Chaluts pélagiques (n.c.a.)",
    "Bottom trawls (nei)" = "Chaluts de fond (n.c.a.)",
    "Midwater shrimp trawls" = "Chaluts pélagiques à crevettes",
    "Bottom pair trawls" = "Chaluts de fond en paire"
  )
  
  # Create the French version with translated column names and gear types
  fishing_summary_french <- fishing_summary_english %>%
    rename(
      `Nom du navire` = `Vessel name`,
      `MMSI` = `MMSI`,  #
      `IMO` = `IMO`,    
      `Pavillon` = `Flag`,
      `Longueur du navire (m)` = `Vessel length (m)`,
      `Engin de pêche` = `Fishing gear`,
      `Incertitude` = Uncertainty,
      `Heures totales de pêche apparente` = `Total apparent fishing hours`,
      `Empreinte spatiale de pêche (km^2)`= `Fishing surface footprint (km^2)`,
      `AMP pêchées` = `Fished MPAs`,
      `Catégorie UICN la plus élevée` = `Highest IUCN category`
    ) %>%
    mutate(
      # Translate gear types to French
      `Engin de pêche` = dplyr::recode(`Engin de pêche`, !!!gear_translation),
      
      # Translate uncertainty levels
      `Incertitude` = dplyr::recode(`Incertitude`,
                                    "low" = "faible",
                                    "medium" = "moyenne",
                                    "high" = "élevée"),
      
      # Convert country names to French
      `Pavillon` = countrycode(`Pavillon`, "country.name", "country.name.fr", nomatch = "Inconnu")
    )
  
  write_xlsx(fishing_summary_french, "fishing_summary_french.xlsx")


}
