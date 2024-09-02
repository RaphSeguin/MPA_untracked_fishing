calculate_stats_eez <- function(){
  
  #Create eez without the MPAs
  eez_no_mpa <- eez_mpa_difference(eez, mpa_wdpa)
  
  #Calculate SAR points which fall inside the EEZ
  SAR_outside_mpas <- calc_SAR_outside_mpa(SAR_data_sf, eez_no_mpa)

  #Normalize detections outside of MPAs
  SAR_eez_final <- normalize_detections_EEZ(SAR_outside_mpas, SAR_footprints_sf)
  
  #Calculate stats outside of MPAs
  SAR_eez_stats <- calculate_stats_outside_mpas(SAR_eez_final)
  
  #Summarize data
  EEZ_final_vars <- prep_eez_data_for_analysis(SAR_eez_stats, eez)
  
  save(SAR_eez_stats, file = "output/SAR_eez_stats.Rdata")
  save(EEZ_final_vars, file = "output/EEZ_final_vars.Rdata")
  
  return(EEZ_final_vars)

}