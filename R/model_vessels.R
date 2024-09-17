
model_vessels <- function(){
  
  #Prep data for model
  mpa_vessel_model  <- prep_vessel_data(MPA_final_vars, MPA_covariates, mpa_wdpa)
  
  #Model vessel presence
  model_vessel_presence(mpa_vessel_model)
  
  #Model vessel number - all vessels
  model_all_vessels(mpa_vessel_model)
  
  #Model vessel size
  model_vessel_size(mpa_vessel_model)
  
  #model vessel number - only unmatched
  model_unmatched_vessels(mpa_vessel_model)
  
}

