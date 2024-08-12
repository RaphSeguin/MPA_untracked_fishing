calculate_shapley_values <- function(){
  
  # Final binomial model
  mod_binomial <- ranger(
    fishing_presence ~ iucn_cat + area_correct + 
      seamount_distance + 
      mean_sst + sd_sst + mean_chl + sd_chl + depth + n_non_fishing +
      ais_reception_positions_per_day_class_A + ais_reception_positions_per_day_class_B +
      dist_to_shore + 
      gdp + X + Y + 
      dist_to_port + fishing,
    data = mpa_model_binomial,
    mtry = best_params$mtry,            # Use best mtry
    min.node.size = best_params$min_n,  # Use best min_n
    num.trees = 1000,
    probability = TRUE,                 # To get probabilities for thresholding
    num.threads = 4,
    importance = "permutation"
  )
  
  # Variables used in the model
  xvars <- c("iucn_cat", "area_correct", "seamount_distance", 
             "mean_sst", "sd_sst", "mean_chl", "sd_chl", "depth", "n_non_fishing",
             "ais_reception_positions_per_day_class_A", "ais_reception_positions_per_day_class_B",
             "dist_to_shore", "gdp", "X", "Y", 
             "dist_to_port", "fishing")
  
  # 1) Sample rows to be explained
  set.seed(10)
  X <- mpa_model_binomial[sample(nrow(mpa_model_binomial), 1000), xvars]
  
  # 2) Sample background data. Ensure it contains only the variables in `xvars`
  bg_X <- mpa_model_binomial[sample(nrow(mpa_model_binomial), 100), xvars]
  
  # 3) Calculate Shapley values
  ks <- kernelshap(mod_binomial, X, bg_X = bg_X)
  
  # 4) Analyze with our sister package {shapviz}
  ps <- shapviz(ps)
  sv_importance(ps)
  sv_dependence(ps, xvars)
  
}