  calculate_stats_eez <- function(){
  
  message("Calculating EEZ without MPAs...")
  eez_no_mpa <- eez_mpa_difference(eez, mpa_wdpa, SAR_footprints_sf)
    
  message("Calculating SAR points outside MPAs...")
  SAR_outside_mpas <- calc_SAR_outside_mpa(SAR_data_sf, eez_no_mpa)
    
  message("Normalizing detections outside MPAs...")
  SAR_eez_final <- normalize_detections_EEZ(SAR_outside_mpas, SAR_footprints_sf)
    
  message("Calculating stats outside MPAs...")
  SAR_eez_stats <- calculate_stats_outside_mpas(SAR_eez_final)
    
  message("Preparing EEZ data for analysis...")
  EEZ_final_vars <- prep_eez_data_for_analysis(SAR_eez_stats, eez_no_mpa)
  
  save(SAR_eez_stats, file = "output/SAR_eez_stats.Rdata")
  save(EEZ_final_vars, file = "output/EEZ_final_vars.Rdata")

}