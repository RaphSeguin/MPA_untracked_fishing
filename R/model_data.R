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
    # filter(iucn_cat %in% c("I","II","IV","V","VI")) %>%
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
    filter(relative_sum_all < quantile(relative_sum_all,0.95)) %>%
    na.omit() %>%
    filter(iucn_cat != "III")
  
  hist(data_model$relative_sum_all)
  # 
  # coords <- data_model[,c("X","Y")]
  # 
  #Model selection
  # mod_gls <- gls(relative_sum_all ~  HDI + MarineEcosystemDependency + NGO + conflicts + hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #               data = data_model , method="ML") 
  # 
  # mod_gls_spatial_1 <- gls(relative_sum_all ~  HDI + MarineEcosystemDependency + NGO + conflicts + hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #                correlation = corSpher(form = ~X + Y|id, nugget = TRUE),
  #              data = data_model , method="ML")
  # 
  # mod_gls_spatial_2 <- gls(relative_sum_all ~  HDI + MarineEcosystemDependency + NGO + conflicts + hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #                        correlation = corGaus(form = ~X + Y|id, nugget = TRUE),
  #                        data = data_model , method="ML")
  # 
  # mod_gls_spatial_3 <- gls(relative_sum_all ~  HDI + MarineEcosystemDependency + NGO + conflicts + hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #                        correlation = corExp(form = ~X + Y|id, nugget = TRUE),
  #                        data = data_model , method="ML")
  # 
  # mod_gls_spatial_4 <- gls(relative_sum_all ~  HDI + MarineEcosystemDependency + NGO + conflicts + hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #                        correlation = corRatio(form = ~X + Y|id, nugget = TRUE),
  #                        data = data_model , method="ML")
  # 
  # AIC(mod_gls,mod_gls_spatial_1, mod_gls_spatial_2, mod_gls_spatial_3, mod_gls_spatial_4)
  # stepAIC(mod_gls_spatial_2)
  # # 
  # mod_gls_spatial_best <- gls(relative_sum_all ~ iucn_cat + MarineEcosystemDependency + Chla  + Bathymetry + SST + DistToSeamounts + DistToCoast + salinity + gdp + travel_time,
  #                          correlation = corGaus(form = ~X + Y|id, nugget = TRUE),
  #                          data = data_model , method="ML")
  # 
  
  # mod_gls_spatial_best <- gls(relative_sum_all_log ~  iucn_cat*(MarineEcosystemDependency + NGO + Chla + travel_time) ,
  #                          correlation = corGaus(form = ~X + Y|id, nugget = TRUE),
  #                          data = data_model , method="ML")
  # 
  # #Model performance
  # summary(mod_gls_spatial_best)
  # rsquared(mod_gls_spatial_best)
  # anova(mod_gls_spatial_best)
  # R2 <- cor(data_model$relative_sum_all,predict(mod_gls_spatial_best))^2
  # R2
  # 
  # visreg(mod_gls_spatial_best,"Chla",scale="response")
  # # visreg(mod_gls_spatial_best,"conflicts",scale="response")
  # visreg(mod_gls_spatial_best,"NGO",scale="response")
  # visreg(mod_gls_spatial_best,"MarineEcosystemDependency",scale="response")
  # visreg(mod_gls_spatial_best,"HDI",scale="response")
  # visreg(mod_gls_spatial_best,"travel_time",scale="response")
  # visreg(mod_gls_spatial_best,"iucn_cat",scale="response")
  # visreg(mod_gls_spatial_best,"travel_time",by = "iucn_cat",scale="response")
  # 
  # SpatialML
  
  #------SPAMM----
  
  avail_thr <- parallel::detectCores(logical=FALSE) - 1L
  
  mod_spamm <- fitme(relative_sum_all ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST  + marine + 
                       DistToCoast + salinity + gdp + travel_time + Matern(1 | X + Y),
                    data = data_model, family=Gamma(log),control.HLfit=list(NbThreads=max(avail_thr, 1L)))
  
  save(mod_spamm, file = "output/mod_spamm.Rdata")
  
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

  
  #Partial effects
  partial_effects_traveltime <- visreg(mod_spamm,"travel_time",type="conditional")$fit %>% dplyr::select(travel_time, visregFit) %>% mutate(var = "Travel time") %>% dplyr::rename(pred_var = "travel_time")
  # partial_effects_DistToCoast<- visreg(mod_spamm,"DistToCoast",type="conditional")$fit %>% dplyr::select(DistToCoast, visregFit) %>% mutate(var = "Distance to the coast") %>% dplyr::rename(pred_var = "DistToCoast")
  partial_effects_Bathymetry <- visreg(mod_spamm,"Bathymetry",type="conditional")$fit %>% dplyr::select(Bathymetry, visregFit) %>% mutate(var = "Depth") %>% dplyr::rename(pred_var = "Bathymetry")
  partial_effects_Chla <- visreg(mod_spamm,"Chla",type="conditional")$fit %>% dplyr::select(Chla, visregFit) %>% mutate(var = "Primary productivity") %>% dplyr::rename(pred_var = "Chla")
  
  plot_travel_time <- ggplot(partial_effects_traveltime) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time to the nearest city",
         y = "Fitted coefficients") 
  
  plot_Chla <- ggplot(partial_effects_Chla) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Primary productivity (Chla)",
         y = "Fitted coefficients")
  
  plot_DistToCoast<- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Distance to the coast",
         y = "Fitted coefficients")
  
  plot_Bathymetry<- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Depth",
         y = "Fitted coefficients")
  
  mod1_partial <- ggarrange(plot_travel_time, plot_Chla + rremove("ylab"), plot_Bathymetry + rremove("ylab"),nrow = 1)
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
    filter(unmatched_relative < quantile(unmatched_relative,0.95)) %>%
    filter(unmatched_relative > 0) %>%
    # mutate(relative_sum_all_log = log10(relative_sum_all +1)) %>%
    na.omit() %>%
    filter(iucn_cat != "III")

  mod_spamm_unmatched <- fitme(unmatched_relative ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + marine + 
                       DistToCoast + salinity + gdp + travel_time + Matern(1 | X + Y),
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
  
  partial_effects <- bind_rows(partial_effects_traveltime,partial_effects_Chla,partial_effects_DistToSeamounts)
  
  plot_travel_time <- ggplot(partial_effects_traveltime) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Travel time to the nearest city",
         y = "Fitted coefficients")
  
  plot_Chla <- ggplot(partial_effects_Chla) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Primary productivity (Chla)",
         y = "Fitted coefficients")
  
  plot_bathymetry <- ggplot(partial_effects_Bathymetry) + 
    geom_line(aes(pred_var,visregFit)) +
    theme_minimal(base_size = 14) + 
    labs(x = "Bathymetry",
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
  
  partial_iucn_cat <-  visreg(mod_spamm,"travel_time",type="conditional",by="iucn_cat")$fit 

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
    mutate(ratio = log((unmatched_fishing/matched_fishing))) %>%
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
  
  data_model_ratio <- data_model_ratio[is.finite(data_model_ratio$ratio),]
  
  hist(data_model_ratio$ratio)

  #Model ratio
  
  # Fixed-effect model
  mod_spamm_ratio <- fitme(ratio ~  HDI + MarineEcosystemDependency + conflicts + 
                                 hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST + marine + 
                                 DistToCoast + salinity + gdp + travel_time + Matern(1 | X + Y),
                               data = data_model_ratio, family=gaussian(),
                           control.HLfit=list(NbThreads=max(avail_thr, 1L)))
  
  R2 <- cor(data_model_ratio$ratio,predict(mod_spamm_ratio))^2
  R2
  
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
