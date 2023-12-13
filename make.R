#' Run the Entire Project
#'

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","janitor","gfwr","arrow","beepr","sfarrow",
          "harrypotter","wesanderson","ranger","ggpubr","data.table","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

key <- gfw_auth()
sf_use_s2(FALSE)

#-----------------Loading all data---------------------

#Load Rdata
path = (here::here("data"))
setwd(path)
files <- list.files(pattern="Rdata")
data_list = lapply(files, load, .GlobalEnv)

#Loading all outputs

path = (here::here("output"))
setwd(path)
files <- list.files(pattern="Rdata")
data_list = lapply(files,load, .GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#####------ANALYSIS-------
setwd(here())

#WDPA database
#Download from
#https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA

#Compiling WDPA database
# mpa_wdpa <- bind_rows(st_read(dsn = "maps/WDPA/WDPA_Nov2023_Public_shp_0",
#                               layer = "WDPA_Nov2023_Public_shp-polygons",
#                               quiet = TRUE),
#                       st_read(dsn =
#                                 "maps/WDPA/WDPA_Nov2023_Public_shp_1",
#                               layer = "WDPA_Nov2023_Public_shp-polygons",
#                               quiet = TRUE),
#                       st_read(dsn =
#                                 "maps/WDPA/WDPA_Nov2023_Public_shp_2",
#                               layer = "WDPA_Nov2023_Public_shp-polygons",
#                               quiet = TRUE)) %>%
#   clean_names() %>%
#   dplyr::filter(marine %in% c(1,2),
#                 !status_yr == 0) %>%
#   dplyr::select(id= wdpaid, name, desig_eng, iucn_cat, status_yr, gov_type, own_type, mang_auth, mang_plan, iso3, parent_iso, gis_m_area,marine) %>%
#   dplyr::mutate(source = "wdpa",
#                 marine = as.factor(marine))
# 
# save(mpa_wdpa, file = "output/mpa_wdpa.Rdata")

#TestGFW
test = get_raster(
  spatial_resolution = 'high',
  temporal_resolution = 'daily',
  group_by = 'flag',
  date_range = '2021-01-01,2021-12-31',
  region = 5677,
  region_source = 'eez',
  key = key
)

