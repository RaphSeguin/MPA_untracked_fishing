model_fishing_effort_regression <- function(MPA_covariates, mpa_wdpa_fishing){
  
  
  #Size of blocks need to be 200 000 meters
  # Create spatial blocks for cross-validation
  spatial_folds <- blockCV::cv_spatial(mpa_model_regression,
                                       size = 200000, # Define the block size based on your study's scale
                                       k = 10, # Number of folds
                                       selection = "random",
                                       iteration = 100) # Number of iterations
  
  #coordinates
  mpa_model_coordinates <- mpa_model_regression %>% st_centroid() %>% st_coordinates()
  
  mpa_model_regression <- mpa_model_regression %>% st_drop_geometry() %>% bind_cols(mpa_model_coordinates)

  #Spatial cross validation
  folds <- spatial_folds$folds_ids
  
  #Tune model
  best_params_regression <- tune_regression_model(mpa_model_regression) 
  save(best_params_regression, file = "output/best_params_regression.Rdata")
  
 
 
 
  
}