

data_intersection <- function(year,SAR_data, mpa_wdpa){
  
  #Intersecting SAR_data with MPAs
  mpa_intersection_loop <- pbmclapply(1:nrow(mpa_wdpa), function(i) {
    
    tryCatch({
    
    temp = mpa_wdpa[i,]
    
    temp_id <- temp$id
    
    intersection = st_intersection(SAR_data, temp)
    
    if (nrow(intersection) > 0) {
      
      save(intersection, file = paste0("output/",year,"/","SAR_intersections/SAR_intersection_",temp_id,"_",split_number,".Rdata"))
      print(paste(temp_id,": Intersection"))
      
    } else {print(paste(temp_id,": No intersection"))}
    
    }, error = function(e) {
      cat("An error occurred: ", conditionMessage(e), "\n")
      # Additional error handling or logging can be added here
    })
    
  },mc.cores=4)
  
}