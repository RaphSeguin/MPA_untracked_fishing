#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr","sfarrow","corrplot",
          "harrypotter","wesanderson","ranger","missForest","rgdal","countrycode","ggpubr","data.table","randomForestExplainer","spatialRF","spaMM","DHARMa","glmmTMB","performance","spdep",
          "units","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","imputeTS","rgeos","visreg","piecewiseSEM")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

# key <- gfw_auth()
sf_use_s2(FALSE)
#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#-----------------Loading all data---------------------

#WDPA database
#Download from
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA

# #Compiling WDPA database
# prep_mpa_data()

#Load Rdata
path = (here::here("data"))
setwd(path)
files <- list.files(pattern=".Rdata|RData")
data_list = lapply(files, load, .GlobalEnv)

#-----------------Loading shapefiles--------------------

#####------ANALYSIS-------

setwd(here())

# prep_mpa_data()

load("output/SAR_data.Rdata")

#Joining SAR_data with eez data which is not MPA
#eez data from https://www.marineregions.org/downloads.php
eez <- st_read("data/World_EEZ_v12_20231025/eez_v12.shp")

#Load all outputs from intersection
# path = (here::here("output/SAR_intersections/"))
# setwd(path)
# SAR_mpa_files <- list.files(pattern = ".Rdata")

#First convert them to matrix to save space
# pbmclapply(1:length(SAR_mpa_files),function(i){
#   
#   intersection_df <- get(load(SAR_mpa_files[i])) %>% st_drop_geometry() %>% dplyr::select(unique_id,id) %>% as.matrix()
#   
#   save(intersection_df, file = paste0("SAR_intersections_final/SAR_intersection",unique(intersection_df[,2]),".Rdata"))
#   
#   #Clean workspace to save memory
#   rm(list = ls(all.names = TRUE))
#   gc()
#   
# },mc.cores = 2)
# 
# setwd(here())

# And now load them all and save
setwd(here())
path = ("output/SAR_intersections")
setwd(path)

SAR_mpa <- list.files()  %>%
  map_df(~ as.data.frame(get(load(file = .x)))) %>%
  inner_join(mpa_wdpa_no_sf, by = "id_iucn") %>%
  left_join(SAR_data, by = "unique_id") %>%
  #replacing MPA area with REAL mpa area
  dplyr::select(-gis_m_area) %>%
  dplyr::rename(gis_m_area = "area_correct")

setwd(here())
save(SAR_mpa, file = "output/SAR_mpa.Rdata")

load("output/SAR_mpa.Rdata")
load("output/SAR_eez_final.Rdata")

#For each MPA, calculating various statistics
SAR_stats <- calculate_stats(SAR_mpa)
SAR_stats_time <- calculate_stats_time(SAR_mpa)

#Calculating fishing effort inside EEZs
SAR_eez_final <- eez_mpa_difference(SAR_data)
SAR_stats_no_covid <- calculate_stats_no_covid(SAR_mpa)

#Print various statistics on MPA data
describe_results(SAR_stats)

#------Figures-------


#----MODELLING-------

