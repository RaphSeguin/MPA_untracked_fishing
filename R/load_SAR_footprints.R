load_SAR_footprints <- function(){
  
  #Downloaded from global fishing watch
  path <- "data/SAR_footprints/"
  
  #Load csv file names
  csv_names <- list.files(path = path,
                          pattern = "*.csv",
                          full.names = TRUE)
  
  # Append all SAR data into dataframe
  SAR_footprints <- lapply(csv_names, read_csv) %>% 
    bind_rows() 
  
  save(SAR_footprints, file = "data/SAR_footprints.Rdata")
  return(SAR_footprints)
  
}