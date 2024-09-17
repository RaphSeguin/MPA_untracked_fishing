check_spatial_performance <- function(){
  
  folds_regression_2022
  
  
  # Now let’s write a function that will, for each resample:
  #   
  # obtain the analysis set for model fitting
  # fit a linear model with a interaction term
  # predict the assessment set and return both the true and predicted price, on the log scale
  
  compute_preds <- function(splits) {
    
    # fit the model to the analysis set
    mod <- lmer(formula_regression,
                data = analysis(splits))
  
    # identify the assessment set
    holdout <- assessment(splits)
    
    # return the assessment set, with true and predicted price
    tibble::tibble(
      geometry = holdout$geometry,
      AIS_fishing = holdout[[pred_var]],
      .pred = exp(predict(mod, holdout, re.form = NA))
    )
    
  }
  
  #Apply to each of the split
  cv_res <- folds_regression %>%
    mutate(.preds = map(splits, compute_preds))
  
  #Unnest the results
  cv_rmse <- cv_res %>%
    unnest(.preds) %>%
    group_by(id) %>%
    rmse(AIS_fishing, .pred)
  
  #Plot RMSE
  plot <- cv_res %>%
    unnest(.preds) %>%
    left_join(cv_rmse, by = "id") %>%
    ggplot(aes(color = .estimate)) +
    geom_sf(aes(geometry = geometry), alpha = 0.5) +
    labs(color = "RMSE") +
    scale_color_viridis_c()
  

}