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
    filter(mean_size < quantile(mean_size,0.95)) %>%
    na.omit() %>%
    filter(iucn_cat != "III")
  
  #Modeling size
  avail_thr <- parallel::detectCores(logical=FALSE) - 1L
  
  mod_spamm_size <- fitme(mean_size ~  HDI + MarineEcosystemDependency + conflicts + 
                       hf + Chla + iucn_cat + gis_m_area + Bathymetry + SST  + marine + 
                       DistToCoast + salinity + gdp + travel_time + Matern(1 | X + Y),
                     data = data_model_size, family=gaussian(),control.HLfit=list(NbThreads=max(avail_thr, 1L)))
  
  save(mod_spamm_size, file = "output/mod_spamm_size.Rdata")

  #Model results
  summary(mod_spamm_size)
  
}

