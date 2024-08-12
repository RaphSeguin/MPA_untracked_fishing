analyze_SAR <- function(){
  
  load("output/mpa_SAR.Rdata")
  
  mpa_SAR_clean <- mpa_SAR %>%
    mutate(iucn_cat = factor(iucn_cat, level = level_order)) %>%
    #Distinct cells only 
    group_by(grid_id) %>%
    distinct(unmatched_number, .keep_all = T) %>%
    ungroup() 
  
  save(mpa_SAR_clean, file = "output/mpa_SAR_clean.Rdata")
  
  #Number of cells with unmatched
  cells_unmatched_unprotected <- nrow(mpa_SAR_clean %>% filter(iucn_cat == "Unprotected" & unmatched_presence == "Unmatched"))
  print(cells_unmatched_unprotected)
  
  cells_unmatched_protected <- nrow(mpa_SAR_clean %>% filter(iucn_cat != "Unprotected" & unmatched_presence == "Unmatched"))
  print(cells_unmatched_protected)
  
  #Percentage of unprotected cells with ports
  print(paste("Percentage of unprotected cells with unmatched:",cells_unmatched_unprotected/n_unprotected))
  
  #Percentage of protected cells with ports
  print(paste("Percentage of protected cells with unmatched:",cells_unmatched_protected/n_protected))
  
  #Average number of unmatched vessels in unprotected cells
  print(paste("Average number of unmatched vessels in unprotected cells:",
              mean((mpa_SAR_clean %>% filter(iucn_cat == "Unprotected"))$unmatched_number)))
  
  #Average number of unmatched vessels in protected cells
  print(paste("Average number of unmatched vessels in protected cells:",
              mean((mpa_SAR_clean %>% filter(iucn_cat != "Unprotected"))$unmatched_number)))
  
}