binomial_model <- function(){
  
  
  #MPA model data
  MPA_model_data_final <- MPA_model_data %>%
   dplyr::select(id_iucn, relative_sum_all) %>%
    #Joining with MPA data
   full_join(mpa_full_data, by = "id_iucn") %>%
    #Add variable to model
   dplyr::mutate(fishing_var = as.factor(ifelse(is.na(relative_sum_all),0,1))) %>%
  #Selecting variables to model
   dplyr::select(id_iucn, area_correct, iucn_cat,fishing_var,marine) %>%
    #adding geometry
     left_join(mpa_wdpa %>% dplyr::select(id_iucn, geometry), by = "id_iucn") %>%
     st_as_sf() %>%
     #Joining with full MPA covariates data to get covariates
     st_join(AMP_model_data,join = st_nearest_feature) %>%
     st_drop_geometry() %>%
    #Mean of covariates by group
    group_by(id_iucn,fishing_var,iucn_cat,area_correct,marine) %>%
    reframe(ID =ID,
            conflicts = mean(conflicts),
            Voice = mean(as.numeric(as.character(Voice))),
            HDI = mean(HDI),
            NGO = mean(NGO),
            MarineEcosystemDependency = mean(MarineEcosystemDependency),
            Bathymetry = mean(Bathymetry),
            DistToCoast = mean(DistToCoast),
            travel_time = mean(travel_time),
            DistToSeamounts = mean(DistToSeamounts),
            SST = mean(SST),
            Chla = mean(Chla),
            salinity = mean(salinity),
            gdp = mean(gdp),
            hf = mean(hf)) %>%
    ungroup() %>%
    #Log10 + 1 almost everything
    dplyr::mutate(
      HDI = log10(HDI+1),
      marine = as.factor(marine),
      MarineEcosystemDependency = log10(MarineEcosystemDependency+1),
      NGO = log10(NGO+1),
      conflicts = log10(conflicts+1),
      Chla = log10(Chla +1),
      Bathymetry = log10(abs(Bathymetry)+1),
      DistToSeamounts = log10(DistToSeamounts+1),
      DistToCoast = log10(DistToCoast+1),
      salinity = log10(salinity+1),
      gdp = gdp,
      SST = log10(SST+1),
      gdp = log10(gdp+1),
      hf = log10(hf+1),
      travel_time = log10(travel_time+1),
      area_correct = log10(area_correct+1),
      iucn_cat = as.factor(iucn_cat)) %>%
    #Adding coordinates
    left_join(coords, by = "ID") %>%
    distinct(id_iucn,X,Y, .keep_all = T) %>%
    na.omit() 
  
  #Description of MPA model data
  fishing_vessel_presence <- MPA_model_data %>%
    dplyr::select(id_iucn, relative_sum_all) %>%
    #Joining with MPA data
    full_join(mpa_full_data, by = "id_iucn") %>%
    #Add variable to model
    dplyr::mutate(fishing_var = as.factor(ifelse(is.na(relative_sum_all),0,1))) %>%
    add_count(iso3, fishing_var, name = "presence_count") %>%
    distinct(iso3, fishing_var, presence_count) %>%
    pivot_wider(names_from = "fishing_var",values_from = "presence_count") %>%
    mutate(proportion = `1`/(`1`+`0`))
  
  #Plot fishing vessel presence
  fishing_vessel_plot <- MPA_model_data %>%
    dplyr::select(id_iucn, relative_sum_all) %>%
    #Joining with MPA data
    full_join(mpa_full_data, by = "id_iucn") %>%
    #Add variable to model
    dplyr::mutate(fishing_var = as.factor(ifelse(is.na(relative_sum_all),0,1))) %>%
    add_count(iso3, fishing_var, name = "presence_count") %>%
    distinct(iso3, fishing_var, presence_count) %>%
    group_by(iso3) %>%
    mutate(sum = sum(presence_count)) %>%
    arrange(-sum) %>%
    head(21) %>%
    ungroup() %>%
    mutate(country = countrycode(iso3,origin="iso3c",destination="country.name")) 
  
  mpa_vessel_presence_plot <- ggplot(fishing_vessel_plot, aes(reorder(country,sum), presence_count, fill = fishing_var)) + 
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(values = c(`0`="#2684A4",`1`="#F6C85F"), labels = c(`0` = "No vessels detected",`1` = "Vessels detected")) + 
    theme_minimal() +
    labs(x = " ",
         y = "Number of MPAs",
         fill = "Fished or not fished") 
  
  #In terms of surface
  fishing_vessel_presence <- MPA_model_data %>%
    dplyr::select(id_iucn, relative_sum_all) %>%
    #Joining with MPA data
    full_join(mpa_full_data, by = "id_iucn") %>%
    #Add variable to model
    dplyr::mutate(fishing_var = as.factor(ifelse(is.na(relative_sum_all),0,1))) %>%
    group_by(parent_iso, fishing_var) %>%
    mutate(surface_sum = sum(area_correct)) %>%
    ungroup() %>%
    distinct(parent_iso, fishing_var, surface_sum) %>%
    pivot_wider(names_from = "fishing_var",values_from = "surface_sum") %>%
    mutate(proportion = `1`/(`1`+`0`))
  
  #Binomial model
  avail_thr <- parallel::detectCores(logical=F) - 1L
  
  mod_spamm_binomial <- fitme(fishing_var ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + area_correct + Bathymetry + SST  + 
                       DistToCoast + salinity + gdp + travel_time + marine + Matern(1 | X + Y),
                       data = MPA_model_data_final, family=binomial(),control.HLfit=list(NbThreads=max(avail_thr, 1L)),method="PQL/L")
  
  save(mod_spamm_binomial, file = "output/mod_spamm_binomial.Rdata")
  
  mod_spamm_binomial_output <- summary(mod_spamm_binomial,details=list(p_value=TRUE))$beta_table %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    mutate(Variable = ifelse(Variable == "HDI", "Human development index",
                             ifelse(Variable == "MarineEcosystemDependency","Marine ecosystem dependency",
                                    ifelse(Variable == "conflicts","Conflicts",
                                           ifelse(Variable == "hf","Human footprint",
                                                  ifelse(Variable == "Chla","Primary productivity",
                                                         ifelse(Variable == "area_correct","MPA size",
                                                                ifelse(Variable == "Bathymetry","Depth",
                                                                       ifelse(Variable == "SST", "Sea surface temperature",
                                                                              ifelse(Variable == "DistToCoast", "Distance to coast",
                                                                                     ifelse(Variable == "salinity","Salinity",
                                                                                            ifelse(Variable == "gdp","Gross domestic product",
                                                                                                   ifelse(Variable == "travel_time","Travel time to the nearest ciy",
                                                                                                          ifelse(Variable == "marine2","Fully marine or both marine and terrestrial MPA",Variable)
                                                                                                          ))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
    
  write.csv(mod_spamm_binomial_output, "output/mod_spamm_binomial_output.csv")
  
  test <- plot.HLfit(mod_spamm)
  
  save(mod_spamm_binomial, file = "output/mod_spamm_binomial.Rdata")

  
  #Plot sims
  sims <- simulateResiduals(mod_spamm_binomial)
  plot(sims)
   
  AIC(mod_spamm_binomial)
  
  #calculate probability of default for each individual in test dataset
  predicted <- predict(mod_spamm_binomial, MPA_model_data_final, type="response")
  #calculate AUC
  library(pROC)
  auc(MPA_model_data_final$fishing_var, predicted)

  
  #Partial effects
  partial_effects_area <- visreg(mod_spamm_binomial,"area_correct",type="conditional",scale="response")$fit %>% mutate(var = "MPA area") %>% dplyr::rename(pred_var = "area_correct")
  partial_effects_marine<- visreg(mod_spamm_binomial,"marine",type="conditional",scale = "response")$fit %>% dplyr::select(marine, visregFit) %>% mutate(var = "Marine or Terrestrial MPA") %>% dplyr::rename(pred_var = "marine")
  partial_effects_travel_time <- visreg(mod_spamm_binomial,"travel_time",type="conditional",scale="response")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel time to the nearest city") %>% dplyr::rename(pred_var = "travel_time")
  partial_effects_sst <- visreg(mod_spamm_binomial,"SST",type="conditional",scale="response")$fit %>% dplyr::select(SST, visregFit) %>% mutate(var = "Sea surface temperature") %>% dplyr::rename(pred_var = "SST")
  
  plot_effects_area <- ggplot(partial_effects_area) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "MPA area",
         y = "Probability of containing fishing vessels") 
  
  plot_effects_marine <- ggplot(partial_effects_marine) + 
    geom_boxplot(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Fully marine or marine and terrestrial MPA",
         y = "Probability of containing fishing vessels") 
  
  plot_effects_traveltime <- ggplot(partial_effects_travel_time) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time to the nearest city",
         y = "Probability of containing fishing vessels") 
  
  # plot_effects_sst <- ggplot(partial_effects_sst) + 
  #   geom_line(aes(pred_var,visregFit)) +
  #   theme_minimal(base_size = 14) + 
  #   labs(x = "Sea surface temperature,
  #        y = "Probability of containing fishing vessels") 
  # 
  # 
  # mod_binomial_partial <- ggarrange(plot_effects_area,plot_effects_marine + rremove("ylab"),plot_effects_traveltime + rremove("ylab"),nrow = 1)
  # mod_binomial_partial <- annotate_figure(mod_binomial_partial, top = text_grob("Presence/absence of fishing vessels in MPA", face = "bold", size = 14))
  # 
  # ggsave(mod_binomial_partial, file = "figures/mod_binomial_partial.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  # 
  # 
  # # visreg(mod_gls_spatial_best,"conflicts",scale="response")
  # # visreg(mod_gls_spatial_best,"NGO",scale="response")
  # visreg(mod_spamm,"MarineEcosystemDependency",scale="response")
  # visreg(mod_spamm,"HDI",scale="response")
  # visreg(mod_spamm,"travel_time",scale="response")
  # visreg(mod_spamm,"iucn_cat",scale="response")
  # visreg(mod_spamm,"travel_time",by="iucn_cat",scale="response")
  
  

  }
