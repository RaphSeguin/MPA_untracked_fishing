calculate_stats_mpa <- function(){
  
  #Normalize detections all together
  SAR_mpa_all <- normalize_detections_all(SAR_mpa, SAR_footprints_sf)
  
  SAR_stats <- calculate_stats_all(SAR_mpa_all)
  
  save(SAR_stats, file = "output/SAR_stats.Rdata")
  
  return(SAR_stats)
}