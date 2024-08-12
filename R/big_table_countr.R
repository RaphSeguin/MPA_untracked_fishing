big_table_country <- function(){

  
  #Number of mpas per country
  countries_mpas <- mpa_wdpa_no_sf %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    drop_na(country) %>%
    distinct(id_iucn, country) %>%
    group_by(country) %>%
    reframe(number_of_mpas = n()) %>%
    ungroup()
  
  #Surface of mpas per country
  surface_mpas <- MPA_union %>%
    mutate(mpa_area = round(as.numeric(set_units(st_area(MPA_union %>% st_as_sf()),km^2)),2)) %>%
    st_drop_geometry() %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    drop_na(country) %>%
    dplyr::distinct(country, mpa_area) 
  
  #percentage of eez protected
  percentage_protected <- eez %>%
    st_drop_geometry() %>%
    #REnaming cook
    mutate(SOVEREIGN1 = ifelse(TERRITORY1 == "Cook Islands","Cook Islands",SOVEREIGN1)) %>%
    group_by(SOVEREIGN1) %>%
    reframe(AREA_KM2 = sum(AREA_KM2)) %>%
    ungroup() %>%
    dplyr::rename(country = "SOVEREIGN1") %>%
    # mutate(country = countrycode(ISO_TER1,origin="iso3c",destination="country.name")) %>%
    #Have to manually rename some stuff
    dplyr::mutate(country = ifelse(country == "Antigua and Barbuda", "Antigua & Barbuda",
                                   ifelse(country == "Democratic Republic of the Congo","Congo - Kinshasa",
                                          ifelse(country == "Republic of the Congo","Congo - Brazzaville",
                                                        ifelse(country == "Comores","Comoros",
                                                               ifelse(country == "Saint Kitts and Nevis", "St. Kitts & Nevis",
                                                                      ifelse(country == "Saint Lucia","St. Lucia",
                                                                             ifelse(country == "Myanmar","Myanmar (Burma)",
                                                                                    ifelse(country == "Republic of Mauritius","Mauritius",
                                                                                           ifelse(country == "East Timor","Timor-Leste",
                                                                                                  ifelse(country == "Saint Vincent and the Grenadines","St. Vincent & Grenadines",country))))))))))) %>%
    dplyr::select(country, AREA_KM2) %>%
    right_join(surface_mpas, by = "country") %>%
    mutate(percentage_protected = round((mpa_area/AREA_KM2)*100,2)) %>%
    dplyr::select(country, percentage_protected)
    
  #Number of mpas with fishing vessels inside
  countries_mpas_fishing <- SAR_stats %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    drop_na(country)  %>%
    distinct(id_iucn, country) %>%
    group_by(country) %>%
    reframe(number_of_mpas_fishing = n()) %>%
    ungroup()
  
  #Surface of mpas with fishing vessels inside
  surface_mpas_fishing <- SAR_stats %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    drop_na(country)  %>%
    left_join(mpa_wdpa, by = "id_iucn") %>%
    distinct(id_iucn,country, .keep_all = T) %>%
    group_by(country) %>%
    reframe(geometry = st_union(geometry)) %>%
    ungroup() %>%
    st_as_sf()
  
  surface_mpas_fishing <- surface_mpas_fishing %>%
    mutate(fished_mpa_area = round(as.numeric(set_units(st_area(surface_mpas_fishing),km^2)),2)) %>%
    st_drop_geometry() 
  
  #Percentage of mpas with fishing
  percentge_mpas_fishing <- countries_mpas_fishing %>%
    full_join(countries_mpas, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_fishing = round((number_of_mpas_fishing/(number_of_mpas + number_of_mpas_fishing))*100,2)) %>%
    dplyr::select(country, percentage_fishing)
  
  #Percentage of surface with fishing
  percentge_surface_mpas_fishing <- surface_mpas_fishing %>%
    full_join(surface_mpas, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_fishing_surface = round((fished_mpa_area/mpa_area)*100,2)) %>%
    dplyr::select(country, percentage_fishing_surface)
  
  #Number of fishing vessels detected inside EEZ
  SAR_inside_mpa <- SAR_stats %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(country, unique_id)
  
  fishing_vessels_eez <- SAR_eez_final %>%
    st_drop_geometry() %>%
    mutate(SOVEREIGN1 = ifelse(TERRITORY1 == "Cook Islands","Cook Islands",SOVEREIGN1)) %>%
    dplyr::rename(country = "SOVEREIGN1") %>%
    # mutate(country = countrycode(ISO_TER1,origin="iso3c",destination="country.name")) %>%
    #Have to manually rename some stuff
    dplyr::mutate(country = ifelse(country == "Antigua and Barbuda", "Antigua & Barbuda",
                                   ifelse(country == "Democratic Republic of the Congo","Congo - Kinshasa",
                                          ifelse(country == "Republic of the Congo","Congo - Brazzaville",
                                                 ifelse(country == "Comores","Comoros",
                                                        ifelse(country == "Saint Kitts and Nevis", "St. Kitts & Nevis",
                                                               ifelse(country == "Saint Lucia","St. Lucia",
                                                                      ifelse(country == "Myanmar","Myanmar (Burma)",
                                                                             ifelse(country == "Republic of Mauritius","Mauritius",
                                                                                    ifelse(country == "East Timor","Timor-Leste",
                                                                                           ifelse(country == "Saint Vincent and the Grenadines","St. Vincent & Grenadines",country))))))))))) %>%
    distinct(country, unique_id) %>%
    bind_rows(SAR_inside_mpa) %>%
    group_by(country) %>%
    reframe(number_of_fishing_boats = n()) %>%
    # mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    ungroup()
  
  #Number of fishing vessels detected inside MPAs
  fishing_vessels_mpa <- SAR_stats %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(country, unique_id) %>%
    drop_na(country) %>%
    group_by(country) %>%
    reframe(number_of_fishing_boats_mpa = n()) %>%
    # mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    ungroup()
  
  #Percentage inside MPAs
  percentage_inside_mpas <- fishing_vessels_eez %>%
    left_join(fishing_vessels_mpa, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_inside_mpa = round((number_of_fishing_boats_mpa/number_of_fishing_boats)*100)) %>%
    # mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    dplyr::select(country,percentage_inside_mpa )
  
   #Number of unmatched fishing vessles inside EEZ
  SAR_unmatched_inside_mpa <- SAR_stats %>%
    filter(matched_category == "unmatched") %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(country, unique_id)
  
  unmatched_fishing_vessels_eez <- SAR_eez_final %>%
    filter(matched_category == "unmatched") %>%
    st_drop_geometry() %>%
    mutate(SOVEREIGN1 = ifelse(TERRITORY1 == "Cook Islands","Cook Islands",SOVEREIGN1)) %>%
    dplyr::rename(country = "SOVEREIGN1") %>%
    # mutate(country = countrycode(ISO_TER1,origin="iso3c",destination="country.name")) %>%
    #Have to manually rename some stuff
    dplyr::mutate(country = ifelse(country == "Antigua and Barbuda", "Antigua & Barbuda",
                                   ifelse(country == "Democratic Republic of the Congo","Congo - Kinshasa",
                                          ifelse(country == "Republic of the Congo","Congo - Brazzaville",
                                                 ifelse(country == "Comores","Comoros",
                                                        ifelse(country == "Saint Kitts and Nevis", "St. Kitts & Nevis",
                                                               ifelse(country == "Saint Lucia","St. Lucia",
                                                                      ifelse(country == "Myanmar","Myanmar (Burma)",
                                                                             ifelse(country == "Republic of Mauritius","Mauritius",
                                                                                    ifelse(country == "East Timor","Timor-Leste",
                                                                                           ifelse(country == "Saint Vincent and the Grenadines","St. Vincent & Grenadines",country))))))))))) %>%
    distinct(country, unique_id) %>%
    bind_rows(SAR_unmatched_inside_mpa) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    group_by(country) %>%
    reframe(number_of_unmatched_fishing_boats = n()) %>%
    ungroup()
  
  #FRACTION OF unmatched fishing vessels in EEZ
  percentage_unmatched_eez <- fishing_vessels_eez %>%
    left_join(unmatched_fishing_vessels_eez, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_unmatched_eez = round((number_of_unmatched_fishing_boats/number_of_fishing_boats)*100,2)) %>%
    dplyr::select(country,percentage_unmatched_eez)
  
  #Number of unmatched fishing vessels inside mpas
  unmatched_fishing_vessels_mpa <- SAR_stats %>%
    filter(matched_category == "unmatched") %>% 
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name")) %>%
    distinct(country, unique_id) %>%
    drop_na(country) %>%
    group_by(country) %>%
    reframe(number_of_unmatched_fishing_boats_mpa = n()) %>%
    ungroup()
  
  #FRACTION OF unmatched fishing vessels in mpa
  percentage_unmatched_eez <- fishing_vessels_mpa %>%
    left_join(unmatched_fishing_vessels_mpa, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_unmatched_mpa = round((number_of_unmatched_fishing_boats_mpa/number_of_fishing_boats_mpa)*100,2)) %>%
    dplyr::select(country,percentage_unmatched_mpa )
  
  #Fraction of unmatched fishng vessels inside MPAs
  percentage_unmatched_mpa_vs_eez <- unmatched_fishing_vessels_eez %>%
    left_join(unmatched_fishing_vessels_mpa, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(percentage_unmatched_mpa_vs_eez = round((number_of_unmatched_fishing_boats_mpa/number_of_unmatched_fishing_boats)*100,2)) %>%
    dplyr::select(country,percentage_unmatched_mpa_vs_eez)
  
  full_table <- countries_mpas %>%
    full_join(surface_mpas, by = "country") %>%
    full_join(percentage_protected, by = "country") %>%
    full_join(countries_mpas_fishing, by = "country") %>%
    full_join(surface_mpas_fishing, by = "country") %>%
    full_join(percentge_mpas_fishing, by = "country") %>%
    full_join(percentge_surface_mpas_fishing, by = "country") %>%
    full_join(fishing_vessels_eez, by = "country") %>%
    full_join(fishing_vessels_mpa, by = "country") %>%
    full_join(percentage_inside_mpas, by = "country") %>%
    full_join(unmatched_fishing_vessels_eez, by = "country") %>%
    full_join(unmatched_fishing_vessels_mpa, by = "country") %>%
    full_join(percentage_unmatched_eez, by = "country") %>%
    full_join(percentage_unmatched_mpa_vs_eez, by = "country") %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    na.omit()
  
  write.csv(full_table, "figures/full_table.csv")
  
}