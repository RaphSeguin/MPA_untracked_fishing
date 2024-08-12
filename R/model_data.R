model_data <- function(SAR_stats_no_covid){
  
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
  data_model <- SAR_stats_no_covid %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    #Keep variables for model
    dplyr::select(id_iucn, relative_sum_all,gis_m_area,iucn_cat,iso3,marine) %>%
    distinct(id_iucn, .keep_all = T) %>%
    #Joining with MPA data to get MPA geometries
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, geometry), by = "id_iucn") %>%
    st_as_sf() %>%
    #Joining with full MPA covariates data to get covariates
    st_join(AMP_model_data,join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    #Mean of covariates by group
    group_by(id_iucn,relative_sum_all,iucn_cat,gis_m_area,iso3,marine) %>%
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
    filter(relative_sum_all < quantile(relative_sum_all,0.99)) %>%
    na.omit() 
  
  corr_matrix <- cor(data_model %>% dplyr::select_if(is.numeric))
  
  corrplot(corr_matrix,method = 'number')
  
  hist(data_model$relative_sum_all)
  
  #------SPAMM----
  
  avail_thr <- parallel::detectCores(logical=FALSE) - 1L
  
  mod_spamm <- fitme(relative_sum_all ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST  + marine + 
                       DistToCoast + salinity + gdp + travel_time + (1|iso3) + Matern(1 | X + Y),
                    data = data_model, family=Gamma(log),control.HLfit=list(NbThreads=max(avail_thr, 1L)))
  
  save(mod_spamm, file = "output/mod_spamm.Rdata")
  
  mod_spamm_output <- summary(mod_spamm,details=list(p_value=TRUE))$beta_table %>%
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
                                                                                                                               ifelse(Variable == "iucn_catI", "IUCN category I", 
                                                                                                                               ifelse(Variable == "iucn_catV","IUCN category V",
                                                                                                                                      ifelse(Variable == "iucn_catVI","IUCN category VI",Variable
                                                                                                                                      ))))))))))))))))))) %>%
    clean_names() %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(p_value = ifelse(p_value < 0.001, "<0.001",p_value)) 
  
  write.csv(mod_spamm_output, file = "output/mod_spamm_output.csv")
  
  #Plot sims
  sims <- simulateResiduals(mod_spamm)
  plot(sims)
  
  AIC(mod_spamm)

  summary(mod_spamm)
  data_model$preds <- as.data.frame(predict(mod_spamm))
  
  ggplot(data_model, aes(relative_sum_all, preds$V1)) +
    geom_point()
 
  R2 <- cor(data_model$relative_sum_all,predict(mod_spamm))^2
  R2
  
  visreg(mod_spamm,"Chla",scale="response")
  visreg(mod_spamm,"SST",scale="response")
  # visreg(mod_gls_spatial_best,"conflicts",scale="response")
  # visreg(mod_gls_spatial_best,"NGO",scale="response")
  visreg(mod_spamm,"MarineEcosystemDependency",scale="response")
  visreg(mod_spamm,"HDI",scale="response")
  visreg(mod_spamm,"travel_time",scale="response")
  visreg(mod_spamm,"iucn_cat",scale="response")
  visreg(mod_spamm,"travel_time",by="iucn_cat",scale="response")

  summary(mod_spamm)
  
  #Partial effects
  partial_effects_mpa_size<- visreg(mod_spamm,"gis_m_area",type="conditional")$fit %>% dplyr::select(gis_m_area, visregFit) %>% mutate(var = "MPA size") %>% dplyr::rename(pred_var = "gis_m_area")
  partial_effects_marine<- visreg(mod_spamm,"marine",type="conditional")$fit %>% dplyr::select(marine, visregFit) %>% mutate(var = "Fully/Partially marine MPA") %>% dplyr::rename(pred_var = "marine")
  partial_effects_Bathymetry <- visreg(mod_spamm,"Bathymetry",type="conditional")$fit %>% dplyr::select(Bathymetry, visregFit) %>% mutate(var = "Depth") %>% dplyr::rename(pred_var = "Bathymetry")
  partial_effects_traveltime <- visreg(mod_spamm,"travel_time",type="conditional")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel Time") %>% dplyr::rename(pred_var = "travel_time")
  partial_effects_DistToCoast <- visreg(mod_spamm,"DistToCoast",type="conditional")$fit %>% dplyr::select(DistToCoast, visregFit) %>% mutate(var = "Distance to coast") %>% dplyr::rename(pred_var = "DistToCoast")
  partial_effects_SST <- visreg(mod_spamm,"SST",type="conditional")$fit %>% dplyr::select(SST, visregFit) %>% mutate(var = "SST") %>% dplyr::rename(pred_var = "SST")
  partial_effects_Chla <- visreg(mod_spamm,"Chla",type="conditional")$fit %>% dplyr::select(Chla, visregFit) %>% mutate(var = "Chla") %>% dplyr::rename(pred_var = "Chla")
  partial_effects_HDI<- visreg(mod_spamm,"HDI",type="conditional")$fit %>% dplyr::select(HDI, visregFit) %>% mutate(var = "HDI") %>% dplyr::rename(pred_var = "HDI")
  
  # plot_DistToCoast<- ggplot(partial_effects_Bathymetry) + 
  #   geom_line(aes(pred_var,visregFit)) +
  #   theme_minimal(base_size = 14) + 
  #   labs(x = "Distance to the coast",
  #        y = "Fitted coefficients")
  
  plot_Bathymetry<- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Depth (log-scale)",
         y = "Fitted coefficients")
  
  plot_traveltime<- ggplot(partial_effects_traveltime) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time (log-scale)",
         y = "Fitted coefficients")
  
  plot_Chla <- ggplot(partial_effects_Chla) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Primary productivity (log-scale)",
         y = "Fitted coefficients")
  
  mod1_partial <- ggarrange(plot_Bathymetry, plot_traveltime + rremove("ylab"), plot_Chla + rremove("ylab"),nrow = 1)
  mod1_partial <- annotate_figure(mod1_partial, top = text_grob("Response: density of fishing vessels", face = "bold", size = 14))
  
  ggsave(mod1_partial,file="figures/mod1_partial.jpg", width = 297, height = 210, dpi = 300, units = "mm")
  
  #Unmatched
  #We model the density of unmatched ships for each mpa
  data_model_unmatched <- SAR_stats_no_covid %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    #Keep variables for model
    dplyr::select(id_iucn, unmatched_relative,gis_m_area,iucn_cat,iso3,marine) %>%
    distinct(id_iucn, .keep_all = T) %>%
    # dplyr::rename(WDPAID = "id") %>%
    #Joining with MPA data to get MPA geometries
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, geometry), by = "id_iucn") %>%
    st_as_sf() %>%
    #Joining with full MPA covariates data to get covariates
    st_join(AMP_model_data,join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    #Mean of covariates by group
    group_by(id_iucn,unmatched_relative,iucn_cat,gis_m_area,iso3,marine) %>%
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
    filter(unmatched_relative < quantile(unmatched_relative,0.99)) %>%
    filter(unmatched_relative > 0) %>%
    # mutate(relative_sum_all_log = log10(relative_sum_all +1)) %>%
    na.omit()
  
  mod_spamm_unmatched <- fitme(unmatched_relative ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + marine + 
                       DistToCoast + salinity + gdp + travel_time + (1|iso3) + Matern(1 | X + Y),
                     data = data_model_unmatched, family=Gamma(log),control.HLfit=list
                     (NbThreads=max(avail_thr, 1L)))
  
  save(mod_spamm_unmatched, file = "output/mod_spamm_unmatched.Rdata")
  
  
  
  #Plot sims
  sims <- simulateResiduals(mod_spamm_unmatched)
  plot(sims)
  
  AIC(mod_spamm_unmatched)
  
  summary(mod_spamm_unmatched)
  data_model$preds <- as.data.frame(predict(mod_spamm))
  
  ggplot(data_model, aes(unmatched_relative, preds$V1)) +
    geom_point()
  
  R2 <- cor(data_model_unmatched$unmatched_relative,predict(mod_spamm_unmatched))^2
  R2
  
  #Partial effects
  partial_effects_traveltime <- visreg(mod_spamm_unmatched,"travel_time",type="conditional")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel time") %>% dplyr::rename(pred_var = "travel_time")
  partial_effects_Chla <- visreg(mod_spamm_unmatched,"Chla",type="conditional")$fit %>% dplyr::select(Chla, visregFit) %>% mutate(var = "Primary productivity") %>% dplyr::rename(pred_var = "Chla")
  # partial_effects_DistToSeamounts <- visreg(mod_spamm_unmatched,"DistToSeamounts",type="conditional")$fit %>% dplyr::select(DistToSeamounts, visregFit) %>% mutate(var = "Distance to seamount") %>% dplyr::rename(pred_var = "DistToSeamounts")
  partial_effects_Bathymetry <- visreg(mod_spamm_unmatched,"Bathymetry",type="conditional")$fit %>% dplyr::select(Bathymetry, visregFit) %>% mutate(var = "Depth") %>% dplyr::rename(pred_var = "Bathymetry")
  
  # partial_effects <- bind_rows(partial_effects_traveltime,partial_effects_Chla,partial_effects_DistToSeamounts)
  
  plot_travel_time <- ggplot(partial_effects_traveltime) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time to the nearest city (log-scale)",
         y = "Fitted coefficients")
  
  plot_Chla <- ggplot(partial_effects_Chla) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Primary productivity (Chla, log-scale)",
         y = "Fitted coefficients")
  
  plot_bathymetry <- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Depth (log-scale)",
         y = "Fitted coefficients")
  
  
  mod2_partial <- ggarrange(plot_travel_time,plot_Chla + rremove("ylab"),plot_bathymetry+ rremove("ylab"),nrow = 1)
  mod2_partial <- annotate_figure(mod2_partial, top = text_grob("Response: density of unmatched fishing vessels", face = "bold", size = 14))
  
  ggsave(mod2_partial,file="figures/mod2_partial.jpg", width = 297, height = 210, dpi = 300, units = "mm")
  
  partial_complete <- ggarrange(mod1_partial,mod2_partial, nrow = 2)
  partial_complete <- annotate_figure(partial_complete, top = text_grob("Partial effects of main predictors of both models", face = "bold", size = 16))
  
  ggsave(partial_complete, file = "figures/partial_complete.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  #According to IUCN cat
  
  level_order <- c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ") 
  legend = c("I" = "#051D41",
             "II" = "#092C63",
             # "III" = "#0E58C6",
             "IV" = "#0E58C6",
             "V" = "#5090EF",
             "VI"= "#93BAF8",
             "Not Applicable" = "#F29F79",
             "Not Assigned" = "#EF8B5B",
             "Not Reported" = "#D87458",
             "EEZ" = "#9B3742")
  
  partial_iucn_cat <-  visreg(mod_spamm,"travel_time",type="conditional",by="iucn_cat",
                              control.HLfit=list(NbThreads=7))$fit 

  partial_iucn_cat_plot <- ggplot(partial_iucn_cat) +
    geom_line(aes(travel_time,visregFit,color = iucn_cat)) +
    scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    labs(x = "Travel time to the nearest city",
         y = "Fitted coefficients",
         color = "IUCN Category",
         title = "Response: Density of fishing vessels")
  
  partial_iucn_cat_unmatched <-  visreg(mod_spamm_unmatched,"travel_time",type="conditional",by="iucn_cat")$fit 
  
  partial_iucn_cat_unmatched_plot <- ggplot(partial_iucn_cat_unmatched) +
    geom_line(aes(travel_time,visregFit,color = iucn_cat)) +
    scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    labs(x = "Travel time to the nearest city",
         y = "Fitted coefficients",
         color = "IUCN Category",
         title = "Response: Density of unmatched fishing vessels")
  
  iucn_cats <- ggarrange(partial_iucn_cat_plot,partial_iucn_cat_unmatched_plot+ rremove("ylab"),nrow = 1, common.legend=T,legend="bottom")
  iucn_cats <-  annotate_figure(iucn_cats, top = text_grob("Effect of IUCN category on fishing vessel density by travel time", face = "bold", size = 16))
  
  #Full plot
  # full_plot <- ggarrange(partial_complete, iucn_cats, nrow = 2)
  
  ggsave(iucn_cats, file = "figures/iucn_cats.jpg", width = 297, height = 210, units ="mm", dpi = 300)
  
  #Modelling ratio
  data_model_ratio<- SAR_stats_no_covid %>%
    filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
    # mutate(matched_fishing = ifelse(matched_fishing == 0, 1, matched_fishing)) %>%
    # mutate(matched_fishing = log(matched_fishing),
    #        unmatched_fishing = log(unmatched_fishing)) %>%
    # mutate(ratio = log((unmatched_fishing/matched_fishing))) %>%
    mutate(ratio = unmatched_fishing/(matched_fishing+unmatched_fishing)) %>%
    #Keep variables for model
    dplyr::select(id_iucn,ratio,unmatched_fishing,matched_fishing, gis_m_area,iucn_cat,iso3,marine) %>%
    distinct(id_iucn, .keep_all = T) %>%
    # dplyr::rename(WDPAID = "id") %>%
    #Joining with MPA data to get MPA geometries
    left_join(mpa_wdpa %>% dplyr::select(id_iucn, geometry), by = "id_iucn") %>%
    st_as_sf() %>%
    #Joining with full MPA covariates data to get covariates
    st_join(AMP_model_data,join = st_nearest_feature) %>%
    st_drop_geometry() %>%
    #Mean of covariates by group
    group_by(id_iucn,ratio,iucn_cat,gis_m_area,iso3,marine) %>%
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
    # filter(unmatched_relative < quantile(unmatched_relative,0.95)) %>%
    # filter(unmatched_relative > 0) %>%
    # mutate(relative_sum_all_log = log10(relative_sum_all +1)) %>%
    na.omit() %>%
    filter(iucn_cat != "III") %>%
    na.omit() 
  
  #Transform for model
  #Source
  #A_better_lemon_squeezer_Maximum-likelihood_regression_with_beta-distributed_dependent_variables
  
  data_model_ratio <- data_model_ratio %>%
    mutate(ratio_model = (ratio * (nrow(data_model_ratio) -1) + 0.5)/(nrow(data_model_ratio))) %>%
    mutate(x_het=runif(1194))
  
  summary(data_model_ratio)

  #Model ratio
  
  # Fixed-effect model
  mod_spamm_ratio <- fitme(ratio_model ~  HDI + MarineEcosystemDependency + conflicts + iucn_cat +
                                 hf + Chla  + gis_m_area + Bathymetry + SST + marine + 
                                 DistToCoast + salinity + gdp + travel_time + 
                             (1 |iso3) +  Matern(1 | X + Y) ,
                               data = data_model_ratio,family=beta_resp(prec = 1),
                           control.HLfit=list(NbThreads=max(avail_thr, 1L)))
  
  save(mod_spamm_ratio, file = "output/mod_spamm_ratio.Rdata")
  
  mod_spamm_ratio_results <- summary(mod_spamm_ratio,details=list(p_value=TRUE))$beta_table %>%
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
  
  write.csv(mod_spamm_ratio_results, "output/mod_spamm_ratio_results.csv")
  
  R2 <- cor(data_model_ratio$ratio,predict(mod_spamm_ratio))^2
  R2
  
  #Partial effects
  partial_effects_area <- visreg(mod_spamm_ratio,"iucn_cat",type="conditional")$fit %>% dplyr::select(gis_m_area, visregFit) %>% mutate(var = "MPA area") %>% dplyr::rename(pred_var = "gis_m_area")
  partial_effects_hdi <- visreg(mod_spamm_ratio,"HDI",type="conditional")$fit %>% dplyr::select(HDI, visregFit) %>% mutate(var = "Human development index") %>% dplyr::rename(pred_var = "HDI")
  
  plot_area <- ggplot(partial_effects_area) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "MPA size (log-scale)",
         y = "Fitted coefficients")
  
  plot_hdi <- ggplot(partial_effects_hdi) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_light(base_size = 20) + 
    labs(x = "Human development index",
         y = "Unmatched/matched (log-scale)")
  
  ggsave(plot_hdi, file = "figures/plot_hdi.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  partial_complete_ratio <- ggarrange(plot_area,plot_hdi, nrow = 1)
  partial_complete_ratio <- annotate_figure(partial_complete, top = text_grob("Partial effects of MPA size and HDI on the ratio between unmatched and matched fishing vessels", face = "bold", size = 16))
  
  ggsave(partial_complete_ratio, file = "figures/partial_complete_ratio.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
  
  summary(mod_spamm_ratio)
  
  #Fit a non spatial model
  
  # # fitst we need to create a numeric factor recording the coordinates of the sampled locations
  # data_model_ratio$pos <- numFactor(scale(data_model_ratio$X), scale(data_model_ratio$Y))
  # # then create a dummy group factor to be used as a random term
  # data_model_ratio$dummy <- factor(rep(1, nrow(data_model_ratio)))
  # 
  # mod_ratio <- glmmTMB(unmatched_ratio ~  HDI + MarineEcosystemDependency + conflicts + 
  #                            hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + marine + 
  #                            DistToCoast + salinity + gdp + travel_time + mat(pos + 0| dummy),
  #                      data = data_model_ratio, family=beta_family(link = "logit"))
  #           
  # save(mod_ratio, file = "output/mod_ratio.Rdata")
  # 
  # summary(mod_ratio)
  # 
  #Check resids
  sims <- simulateResiduals(mod_spamm_ratio)
  plot(sims)
  
  
  rsq.glmm(mod_ratio)
  

  }
