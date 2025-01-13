#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","ggspatial","sf","RColorBrewer","ggridges",
          "plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr",
          "sfarrow","corrplot","DHARMa", "harrypotter","wesanderson","ranger","missForest","rgdal","countrycode","ggpubr",
          "data.table","randomForestExplainer","spatialRF","spaMM","DHARMa","glmmTMB","performance","spdep","rstatix",
          "formatdown","ggrepel","tidync","nngeo","ncdf4","e1071","pROC","units","xml2","XML","rnaturalearth","ggExtra",
          "raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","imputeTS","rgeos","visreg",
          "piecewiseSEM","furrr","future","yardstick","kernelshap","gbm","spatialsample","s2","merTools","wdpar","stringi")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

key <- gfw_auth()
sf_use_s2(F)

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

#LMEs downloaded from https://www.sciencebase.gov/catalog/item/55c77722e4b08400b1fd8244
LME <- st_read("data/LME66/LMEs66.shp")

#MEOW https://academic.oup.com/bioscience/article/57/7/573/238419
MEOW <- st_read("data/MEOW-TNC/meow_ecos.shp") %>% dplyr::select(ECOREGION)

#World for maps
world <- rnaturalearth::ne_countries(scale = "medium")

#Load and clean SAR detection footpritns
SAR_footprints <- load_SAR_footprints()

#Theme for plot
#Base theme
my_custom_theme <- function() {
  theme_minimal(base_size = 18) +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      legend.position = "bottom",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18)
    )
}

#WDPA database
#Download from
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA

#Compiling WDPA database
prep_mpa_data()

# #Unionzed MPA for country comparison
load("data/mpa_wdpa.Rdata")

MPA_union <- mpa_wdpa %>%
  group_by(parent_iso) %>%
  reframe(geometry = st_union(geometry)) %>%
  ungroup()

save(MPA_union, file = "output/MPA_union.Rdata")

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
SAR_mpa <- st_join(SAR_data_sf %>% cbind(lonlat), mpa_wdpa,left = F) %>%
  #replacing MPA area with REAL mpa area
  # dplyr::select(-gis_m_area) %>%
  # dplyr::rename(gis_m_area = "area_correct") %>%
  st_drop_geometry() %>%
  #Add year
  mutate(year = substr(timestamp, 1, 4))

#First, normalize detections by number of satellite overpasses
SAR_footprints_sf <- st_as_sf(SAR_footprints, wkt = "footprint_wkt", crs = 4326)

#Calculatre stats for EEZ
SAR_eez_stats <- calculate_stats_eez()

#Calculate stats for MPA data
SAR_stats <- calculate_stats_mpa()

#Calculate stats for MPA data per year
SAR_mpa_final <- normalize_detections(SAR_mpa, SAR_footprints_sf)
save(SAR_mpa_final, file = "output/SAR_mpa_final.Rdata")

SAR_stats_2022 <- calculate_stats(SAR_mpa_final, 2022)
SAR_stats_2023 <- calculate_stats(SAR_mpa_final, 2023)

#Add mpas with 0 fishing
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

#Function to download GFW data
download_GFW_data()

#----MODELLING-------

level_order <- c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ") 
legend = c("I" = "#051D41",
           "II" = "#092C63",
           "III" = "#0E58C6",
           "IV" = "#2F79EE",
           "V" = "#5090EF",
           "VI"= "#93BAF8",
           "Not Applicable" = "#F29F79",
           "Not Assigned" = "#EF8B5B",
           "Not Reported" = "#D87458",
           "EEZ" = "#9B3742")

#Calculate covariates in each MPA
MPA_covariates <- calc_covariates_MPA()

load("output/MPA_covariates.Rdata")
load("output/SAR_stats.Rdata")
load("output/all_mpas_SAR.Rdata")
load("output/SAR_eez_stats.Rdata")
load("output/eez_no_mpa.Rdata")

MPA_final_vars <- prep_data_for_analysis(SAR_stats, mpa_wdpa)
EEZ_final_vars <- prep_eez_data_for_analysis(SAR_eez_stats, ezz_no_mpa)

#Numbers
describe_results()

#---Model vessels---

model_vessels()

#---Model fishing effort----

model_fishing()

#---Figures----

#FIgure 1 
global_map()

#Figure 2 
figure_2()

#Figure 3
plot_effects()

#Fig 4 is create in model_fishing()

#Make supplementary
make_supp(MPA_final_vars) 

#Print various statistics on MPA data
#open the function to check results
describe_results()


