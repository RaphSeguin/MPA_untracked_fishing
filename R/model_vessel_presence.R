model_vessel_presence <- function(){
  
  mpa_vessel_model$iucn_cat <- relevel(mpa_vessel_model$iucn_cat, ref = "VI")
  
  #Binomial model
  avail_thr <- parallel::detectCores(logical=F) - 1L
  
  mod_spamm_binomial <- fitme(SAR_presence ~  iucn_cat + marine + area_correct + 
                                mean_chl + sd_chl + mean_sst + sd_sst + gdp + HDI + hf + MarineEcosystemDependency + 
                                depth + dist_to_shore + travel_time + (1|parent_iso) + Matern(1 | X + Y),
                              data = mpa_vessel_model, family=binomial(),
                              control.HLfit=list(NbThreads=6),method="PQL/L")
  
  save(mod_spamm_binomial, file = "output/mod_spamm_binomial.Rdata")
  
  #Compute AUC
  predicted_probs <- predict(mod_spamm_binomial, type = "response",re.form=NA)
  roc_curve <- roc((mpa_vessel_model)$SAR_presence, predicted_probs)
  auc_value <- auc(roc_curve)
  auc_value
  
  #Check collinerarity
  check_collinearity(mod_spamm_binomial)
  
  #Output
  # Output
  mod_spamm_binomial_output <- summary(mod_spamm_binomial, details = list(p_value = TRUE))$beta_table %>%
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
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001", p_value)) 
  
  write.csv(mod_spamm_binomial_output, "figures/supp/mod_spamm_binomial_output.csv")
  
  plot_and_save_partial_effects(mod_spamm_binomial, mpa_vessel_model, "mod_spamm_binomial")

}