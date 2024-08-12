check_correlation <- function(mpa_model_regression_no_val){
  
  # Select the covariates you want to check for correlation
  covariates <- mpa_model_regression_no_val %>%
    dplyr::select(mpa_fishing_GFW_log, fishing, area_correct, depth, seamount_distance, 
                  mean_sst, mean_chl, ais_reception_positions_per_day_class_A, 
                  ais_reception_positions_per_day_class_B, dist_to_shore, gdp, 
                  X, Y, length_matched, fishing, fishing_2022, fishing_2022_log)
  
  # Calculate the correlation matrix
  cor_matrix <- cor(covariates, use = "complete.obs")
  
  # Print the correlation matrix
  corrplot(cor_matrix, method = "color", type = "upper", 
           tl.col = "black", tl.srt = 45, 
           addCoef.col = "black", # add correlation coefficients on the plot
           number.cex = 0.7) # text size for coefficients
  
  # Find pairs of variables with correlation higher than 0.7
  high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
  
  # Extract the variable names and their correlation values
  high_cor_pairs <- data.frame(
    Variable1 = rownames(cor_matrix)[high_cor[,1]],
    Variable2 = colnames(cor_matrix)[high_cor[,2]],
    Correlation = cor_matrix[high_cor]
  )
  
  print(high_cor_pairs)
  
}