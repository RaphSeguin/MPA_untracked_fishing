#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","ggspatial","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr","sfarrow","corrplot","DHARMa",
          "harrypotter","wesanderson","ranger","missForest","rgdal","countrycode","ggpubr","data.table","randomForestExplainer","spatialRF","spaMM","DHARMa","glmmTMB","performance","spdep","rstatix","formatdown","ggrepel","tidync","nngeo","ncdf4","e1071",
          "units","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","imputeTS","rgeos","visreg","piecewiseSEM","furrr","future","yardstick","kernelshap","gbm")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

key <- gfw_auth()
sf_use_s2(FALSE)
#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#-----------------Loading all data---------------------
setwd(here())

#Joining SAR_data with eez data which is not MPA
#eez data from https://www.marineregions.org/downloads.php
eez <- st_read("data/World_EEZ_v12_20231025/eez_v12.shp")
LME <- st_read("data/LME66/LMEs66.shp")
MEOW <- st_read("data/MEOW-TNC/meow_ecos.shp") %>% dplyr::select(ECOREGION)

#World for maps
world <- rnaturalearth::ne_countries(scale = "medium")

#Load and clean SAR detection footpritns
SAR_footprints <- load_SAR_footprints()

#WDPA database
#Download from
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA

#Compiling WDPA database
prep_mpa_data()

# #Unionzed MPA for country comparison
# MPA_union <- mpa_wdpa %>%
#   group_by(parent_iso) %>%
#   reframe(geometry = st_union(geometry)) %>%
#   ungroup()

#Load Rdata
path = (here::here("data"))
setwd(path)
files <- list.files(pattern=".Rdata|RData")
data_list = lapply(files, load, .GlobalEnv)

#####------ANALYSIS-------

setwd(here())

#INtersecting
SAR_data <- create_SAR_data()
SAR_data_sf <- SAR_data %>% st_as_sf(coords = c("lon","lat"),crs = 4326)

#Get coordinates of SAR_data_sf
lonlat = cbind(lon = SAR_data$lon,lat = SAR_data$lat)

#Create grid
# SAR_grid <- create_grid() 
SAR_mpa <- st_join(SAR_data_sf %>% cbind(lonlat), mpa_wdpa,left = F) %>%
  #replacing MPA area with REAL mpa area
  dplyr::select(-gis_m_area) %>%
  dplyr::rename(gis_m_area = "area_correct") %>%
  st_drop_geometry()

#First, normalize detections by number of satellite overpasses

#Temp solution for 2023
SAR_footprints_2023 <- SAR_footprints %>% filter(grepl("2023", date))

SAR_mpa_final <- normalize_detections(SAR_mpa, SAR_footprints_2023)

save(SAR_mpa_final, file = "output/SAR_mpa_final.Rdata")

#Calculate stats for MPA data

#For each MPA, calculating various statistics
SAR_stats <- calculate_stats(SAR_mpa_final)

save(SAR_stats, file = "output/SAR_stats.Rdata")

#Add MPAs with 0 fishing 
SAR_footprints_sf <- st_as_sf(SAR_footprints_2023, wkt = "footprint_wkt", crs = 4326)

MPA_no_fishing <- mpa_wdpa %>%
  filter(!id_iucn %in% SAR_stats$id_iucn) %>%
  #Keep only 0 where we have at least 20 images
  st_join(SAR_footprints_sf) %>%
  st_drop_geometry() %>%
  #Count number of images per MPA
  group_by(id_iucn) %>%
  mutate(image_count = n()) %>%
  ungroup() %>%
  #Select only MPAs where at least 20 images were taken
  filter(image_count >= 20) %>%
  distinct(id_iucn, .keep_all = T)

#Final MPA dataset with fishing and non fishing
all_mpas_SAR <- mpa_wdpa %>%
  filter(id_iucn %in% SAR_stats$id_iucn | id_iucn %in% MPA_no_fishing$id_iucn)

save(all_mpas_SAR, file = "output/all_mpas_SAR.Rdata")

load("output/all_mpas_SAR.Rdata")

#Calculate the number of non fishing vessels per MPA
mpa_SAR_non_fishing <- calculate_non_fishing_SAR()

# SAR_stats_time <- calculate_stats_time(SAR_mpa)

#Calculating fishing effort inside EEZs
# SAR_eez_final <- eez_mpa_difference(SAR_data,mpa_wdpa)

#----MODELLING FISHING EFFORT-------

#Calculate covariates in each MPA
MPA_covariates <- calc_covariates_MPA()

#Prep fishing effort for the model 
mpa_wdpa_fishing <- prep_fishing_effort()

load("output/MPA_covariates.Rdata")
load("output/mpa_wdpa_fishing.Rdata")

#Now model the presence/absence of fishing effort in MPAs and predict fishing presence and hours
model_fishing()

#---DESCRIBE RESULTS----

#Print various statistics on MPA data
describe_results(SAR_stats)


