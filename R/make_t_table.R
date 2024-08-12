make_t_table <- function(){
  
  load("output/mod_spamm.Rdata")
  load("output/mod_spamm_unmatched.Rdata")
  load("output/mod_spamm_size.Rdata")
  load("output/mod_spamm_binomial.Rdata")
  
  
  mod_spamm_results <- summary(mod_spamm,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",
                            ifelse(p_value < 0.01, "<0.01",
                                   ifelse(p_value < 0.05, "<0.05",p_value))))
  
  mod_spamm_unmatched_results <- summary(mod_spamm_unmatched,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",
                            ifelse(p_value < 0.01, "<0.01",
                                   ifelse(p_value < 0.05, "<0.05",p_value))))
  
  mod_spamm_binomial_results <- summary(mod_spamm_binomial,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",
                            ifelse(p_value < 0.01, "<0.01",
                                   ifelse(p_value < 0.05, "<0.05",p_value))))
  
  mod_spamm_size_results <- summary(mod_spamm_size,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "gis_m_area","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",
                                                                                                                 ifelse(Variable == "iucn_catII","IUCN category II",
                                                                                                                        ifelse(Variable == "iucn_catIV", "IUCN category IV",
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      )))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",
                            ifelse(p_value < 0.01, "<0.01",
                                   ifelse(p_value < 0.05, "<0.05",p_value))))
  
  all_t_values <- mod_spamm_binomial_results %>%
    left_join(mod_spamm_results, by = "variable")
  
  
  
  
}
