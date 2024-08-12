model_size <- function() {
  
  #Model data
  load("data/dataAMP_150922.Rdata")
  # # load("data/dataOutAMP_150922.Rdata")
  load("data/IUCN.I.VI.AMP.Rdata")
  
  #Keep ID for join 
  AMP_ID <- dataAMP %>% dplyr::select(ID,WDPAID) %>% mutate(ID = as.factor(ID),
                                                            WDPAID = as.factor(WDPAID))
  #Keep only numeric variables
  AMP_model_data <- IUCN.I.VI.AMP %>%
    rownames_to_column("ID") %>%
    left_join(AMP_ID, by = "ID") %>%
    st_as_sf(coords = c("long","lat"), crs = 4326) %>% 
    dplyr::select(-c(IUCN_CAT,WDPAID,ISO3))
  
  coords <- cbind(st_coordinates(AMP_model_data),AMP_model_data %>% st_drop_geometry %>% dplyr::select(ID))
  
  #We model the density of unmatched ships for each mpa
  data_model_size <- SAR_stats_no_covid %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    #Mean size per MPA
    group_by(id_iucn) %>%
    mutate(mean_size = mean(length_m)) %>%
    ungroup() %>%
    #Keep variables for model
    dplyr::select(id_iucn, mean_size,gis_m_area,iucn_cat,iso3,marine) %>%
    distinct(id_iucn, .keep_all = T) %>%
    #Joining with MPA data to get MPA geometries
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, geometry), by = "id_iucn") %>%
    st_as_sf() %>%
    #Joining with full MPA covariates data to get covariates
    st_join(AMP_model_data,join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    #Mean of covariates by group
    group_by(id_iucn,mean_size,iucn_cat,gis_m_area,iso3,marine) %>%
    reframe(ID = ID,
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
      mean_size = log10(mean_size+1),
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
      gis_m_area = log10(gis_m_area+1),
      iucn_cat = as.factor(iucn_cat)) %>%
    left_join(coords, by = "ID") %>%
    distinct(id_iucn,X,Y, .keep_all = T) %>%
    filter(mean_size < quantile(mean_size,0.99)) %>%
    na.omit() 
  
  hist(data_model_size$mean_size)
  
  #Modeling size
  avail_thr <- parallel::detectCores(logical=FALSE) - 1L
  
  mod_spamm_size <- fitme(mean_size ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST  + marine + 
                       DistToCoast + salinity + gdp + travel_time + (1|iso3) + Matern(1 | X + Y),
                     data = data_model_size, family=gaussian(),control.HLfit=list(NbThreads=6))
  
  
  save(mod_spamm_size, file = "output/mod_spamm_size.Rdata")
  
  mod_spamm_size_output <- summary(mod_spamm_size,details=list(p_value=TRUE))$beta_table %>%
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
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(mod_spamm_size_output,"output/mod_spamm_size_output.csv")

  #Model results
  summary(mod_spamm_size)
  
  #Partial effects
  partial_effects_gis_m_area<- visreg(mod_spamm_size,"gis_m_area",type="conditional")$fit %>% dplyr::select(gis_m_area, visregFit) %>% mutate(var = "Travel time") %>% dplyr::rename(pred_var = "gis_m_area")
  # partial_effects_marine <- visreg(mod_spamm_unmatched,"marine",type="contrast",scale="response")$fit %>% dplyr::select(marine, visregFit) %>% mutate(var = "Travel time") %>% dplyr::rename(pred_var = "marine")
  partial_effects_bathymetry<- visreg(mod_spamm_size,"Bathymetry",type="conditional")$fit %>% dplyr::select(Bathymetry, visregFit) %>% mutate(var = "Travel time") %>% dplyr::rename(pred_var = "Bathymetry")
  partial_effects_SST <- visreg(mod_spamm_size,"SST",type="conditional")$fit %>% dplyr::select(SST, visregFit) %>% mutate(var = "Primary productivity") %>% dplyr::rename(pred_var = "SST")
  partial_effects_DistToCoast <- visreg(mod_spamm_size,"DistToCoast",type="conditional")$fit %>% dplyr::select(DistToCoast, visregFit) %>% mutate(var = "Depth") %>% dplyr::rename(pred_var = "DistToCoast")
  
  plot_gis_m_area<- ggplot(partial_effects_gis_m_area) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "MPA area",
         y = "Fitted coefficients")
  
  plot_Bathymetry<- ggplot(partial_effects_bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Depth",
         y = "Fitted coefficients")
  
  plot_SST <- ggplot(partial_effects_SST) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Sea surface temperature",
         y = "Fitted coefficients")
  
  plot_DistToCoast<- ggplot(partial_effects_DistToCoast) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Distance to the coast",
         y = "Fitted coefficients")
  
  partial_effects_size <- ggarrange(plot_gis_m_area,plot_Bathymetry + rremove("ylab"),plot_SST,plot_DistToCoast+ rremove("ylab"),nrow = 2, ncol = 2)
  ggsave(partial_effects_size, file = "figures/partial_effects_size.jpg", width = 297, height = 210, dpi = 300, units = "mm")
  
  #Plot sims
  sims <- simulateResiduals(mod_spamm_size)
  plot(sims)
  
   
  R2 <- cor(data_model_size$mean_size,predict(mod_spamm_size))^2
  R2
  
}

