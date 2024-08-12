create_SAR_data <- function(){
  
  # Specify the base directory containing the year folders
  path <- "data/SAR_data/"
  
  #Load csv file names
  csv_names <- list.files(path = path,
                          pattern = "*.csv",
                          full.names = TRUE)
  
  # Append all SAR data into dataframe
  SAR_data <- lapply(csv_names, read_csv) %>% 
    bind_rows() %>%
    mutate(matched_category = as.factor(matched_category)) %>%
    #Keep only unmatched and fishing
    filter(matched_category %in% c("unmatched","fishing")) 
  
  SAR_data <- SAR_data %>%
    filter(grepl("2023", timestamp))
  
  SAR_data <- SAR_data %>%
    mutate(unique_id = paste0("id_",1:nrow(SAR_data)))
  
  save(SAR_data, file = "data/SAR_data.Rdata")

}