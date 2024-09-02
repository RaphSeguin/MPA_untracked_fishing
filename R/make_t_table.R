make_t_table <- function(){
  
  load("output/mod_spamm.Rdata")
  load("output/mod_spamm_unmatched.Rdata")
  load("output/mod_spamm_size.Rdata")
  load("output/mod_spamm_binomial.Rdata")
  
  
  mod_spamm_results <- summary(mod_spamm, details = list(p_value = TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = case_when(
      Variable == "HDI" ~ "Human development index",
      Variable == "iucn_catI" ~ "IUCN category I",
      Variable == "area_correct" ~ "MPA size", 
      Variable == "depth" ~ "Depth",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      Variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      Variable == "conflicts" ~ "Conflicts",
      Variable == "hf" ~ "Human footprint",
      Variable == "Chla" ~ "Primary productivity",
      Variable == "gis_m_area" ~ "MPA size",
      Variable == "Bathymetry" ~ "Depth",
      Variable == "SST" ~ "Sea surface temperature",
      Variable == "DistToCoast" ~ "Distance to coast",
      Variable == "salinity" ~ "Salinity",
      Variable == "gdp" ~ "Gross domestic product",
      Variable == "travel_time" ~ "Travel time to the nearest city",
      Variable == "marine2" ~ "Fully marine or both marine and terrestrial MPA",
      Variable == "iucn_catII" ~ "IUCN category II",
      Variable == "iucn_catIV" ~ "IUCN category IV",
      Variable == "iucn_catV" ~ "IUCN category V",
      Variable == "iucn_catVI" ~ "IUCN category VI",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate(p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      p_value < 0.05 ~ "<0.05",
      TRUE ~ as.character(p_value)
    ))
  
  
  mod_spamm_unmatched_results <- summary(mod_spamm_unmatched,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = case_when(
      Variable == "HDI" ~ "Human development index",
      Variable == "iucn_catI" ~ "IUCN category I",
      Variable == "area_correct" ~ "MPA size", 
      Variable == "depth" ~ "Depth",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      Variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      Variable == "conflicts" ~ "Conflicts",
      Variable == "hf" ~ "Human footprint",
      Variable == "Chla" ~ "Primary productivity",
      Variable == "gis_m_area" ~ "MPA size",
      Variable == "Bathymetry" ~ "Depth",
      Variable == "SST" ~ "Sea surface temperature",
      Variable == "DistToCoast" ~ "Distance to coast",
      Variable == "salinity" ~ "Salinity",
      Variable == "gdp" ~ "Gross domestic product",
      Variable == "travel_time" ~ "Travel time to the nearest city",
      Variable == "marine2" ~ "Fully marine or both marine and terrestrial MPA",
      Variable == "iucn_catII" ~ "IUCN category II",
      Variable == "iucn_catIV" ~ "IUCN category IV",
      Variable == "iucn_catV" ~ "IUCN category V",
      Variable == "iucn_catVI" ~ "IUCN category VI",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate(p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      p_value < 0.05 ~ "<0.05",
      TRUE ~ as.character(p_value)
    ))
  
  
  mod_spamm_binomial_results <- summary(mod_spamm_binomial,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = case_when(
      Variable == "HDI" ~ "Human development index",
      Variable == "iucn_catI" ~ "IUCN category I",
      Variable == "area_correct" ~ "MPA size", 
      Variable == "depth" ~ "Depth",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      Variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      Variable == "conflicts" ~ "Conflicts",
      Variable == "hf" ~ "Human footprint",
      Variable == "Chla" ~ "Primary productivity",
      Variable == "gis_m_area" ~ "MPA size",
      Variable == "Bathymetry" ~ "Depth",
      Variable == "SST" ~ "Sea surface temperature",
      Variable == "DistToCoast" ~ "Distance to coast",
      Variable == "salinity" ~ "Salinity",
      Variable == "gdp" ~ "Gross domestic product",
      Variable == "travel_time" ~ "Travel time to the nearest city",
      Variable == "marine2" ~ "Fully marine or both marine and terrestrial MPA",
      Variable == "iucn_catII" ~ "IUCN category II",
      Variable == "iucn_catIV" ~ "IUCN category IV",
      Variable == "iucn_catV" ~ "IUCN category V",
      Variable == "iucn_catVI" ~ "IUCN category VI",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate(p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      p_value < 0.05 ~ "<0.05",
      TRUE ~ as.character(p_value)
    ))
  
  
  mod_spamm_size_results <- summary(mod_spamm_size,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = case_when(
      Variable == "HDI" ~ "Human development index",
      Variable == "iucn_catI" ~ "IUCN category I",
      Variable == "area_correct" ~ "MPA size", 
      Variable == "depth" ~ "Depth",
      Variable == "dist_to_shore" ~ "Distance to the shore",
      Variable == "sd_chl" ~ "Primary productivity (standard deviation)",
      Variable == "mean_chl" ~ "Primary productivity (average)",
      Variable == "mean_sst" ~ "Sea surface temperature (average)",
      Variable == "sd_sst" ~ "Sea surface temperature (standard deviation)",
      Variable == "MarineEcosystemDependency" ~ "Marine ecosystem dependency",
      Variable == "conflicts" ~ "Conflicts",
      Variable == "hf" ~ "Human footprint",
      Variable == "Chla" ~ "Primary productivity",
      Variable == "gis_m_area" ~ "MPA size",
      Variable == "Bathymetry" ~ "Depth",
      Variable == "SST" ~ "Sea surface temperature",
      Variable == "DistToCoast" ~ "Distance to coast",
      Variable == "salinity" ~ "Salinity",
      Variable == "gdp" ~ "Gross domestic product",
      Variable == "travel_time" ~ "Travel time to the nearest city",
      Variable == "marine2" ~ "Fully marine or both marine and terrestrial MPA",
      Variable == "iucn_catII" ~ "IUCN category II",
      Variable == "iucn_catIV" ~ "IUCN category IV",
      Variable == "iucn_catV" ~ "IUCN category V",
      Variable == "iucn_catVI" ~ "IUCN category VI",
      TRUE ~ Variable
    )) %>%
    clean_names() %>%
    mutate(p_value = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      p_value < 0.05 ~ "<0.05",
      TRUE ~ as.character(p_value)
    ))
  
  # Combine results from all models with an identifier
  mod_spamm_results <- mod_spamm_results %>% mutate(Model = "Presence/absence")
  mod_spamm_unmatched_results <- mod_spamm_unmatched_results %>% mutate(Model = "Unmatched fishing vessel density")
  mod_spamm_binomial_results <- mod_spamm_binomial_results %>% mutate(Model = "Fishing vessel density")
  mod_spamm_size_results <- mod_spamm_size_results %>% mutate(Model = "Vessel size")
  
  # Combine all models into one dataframe
  combined_results <- bind_rows(
    mod_spamm_results,
    mod_spamm_unmatched_results,
    mod_spamm_binomial_results,
    mod_spamm_size_results
  )
  
  # Pivot the data to have one row per variable, with model results as columns
  combined_results_pivot <- combined_results %>%
    dplyr::select(Variable = variable, Estimate = estimate, p_value, Model) %>%
    mutate(Estimate = round(Estimate, 2)) %>%
    mutate(Estimate = case_when(
      p_value == "<0.001" ~ paste0(Estimate, "*"),
      TRUE ~ as.character(Estimate)
    )) %>%
    dplyr::select(-p_value) %>%
    pivot_wider(names_from = Model, values_from = Estimate)
  
  # Replace NA with "Not included in this model"
  combined_results_pivot <- combined_results_pivot %>%
    mutate(across(everything(), ~replace_na(.x, "Not included in this model")))
  
  # Arrange the columns in the desired order
  final_results <- combined_results_pivot %>%
    dplyr::select(Variable, `Presence/absence`, `Vessel size`, `Fishing vessel density`, `Unmatched fishing vessel density`)
  
  # Print the final results
  write.csv(final_results, file = "figures/Tabel_1.csv")
  
}
