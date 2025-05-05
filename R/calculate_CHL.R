download_chlorophyll <- function() {
  
  # Load required packages
  library(rerddap)
  library(lubridate)
  
  # Set up the download directory
  download_dir <- "chlorophyll_data"
  dir.create(download_dir, showWarnings = FALSE)
  
  # ERDDAP dataset ID for global chlorophyll (MODIS-Aqua)
  chl_dataset <- "erdMH1chlamday"
  
  # Set up time period (monthly from 2017 to 2024)
  start_date <- ymd("2017-01-01")
  end_date <- ymd("2024-12-31")
  
  # Generate sequence of months
  months_seq <- seq(start_date, end_date, by = "month")
  
  # Loop through each month and download data
  for (i in 1:length(months_seq)) {
    current_date <- months_seq[i]
    
    # Format dates for ERDDAP query
    date_str <- format(current_date, "%Y-%m-%d")
    end_of_month <- ceiling_date(current_date, "month") - days(1)
    end_date_str <- format(end_of_month, "%Y-%m-%d")
    
    # Format filename with year and month
    filename <- paste0(download_dir, "/chlorophyll_", format(current_date, "%Y_%m"), ".nc")
    
    # Skip if file already exists
    if (file.exists(filename)) {
      cat("File", filename, "already exists. Skipping.\n")
      next
    }
    
    cat("Downloading data for", format(current_date, "%Y-%m"), "\n")
    
    # Try to download the data
    tryCatch({
      # Set up the ERDDAP query with 1° resolution
      chl_data <- griddap(chl_dataset,
                          latitude = c(-90, 90),
                          longitude = c(-180, 180),
                          time = c(date_str, end_date_str),
                          fields = "chlorophyll",
                          stride = c(1, 1, 1),  # Adjust stride for approx 1° resolution
                          fmt = "nc",
                          url = "https://coastwatch.pfeg.noaa.gov/erddap/")
      
      # Copy the downloaded file to our desired filename
      file.copy(chl_data$summary$filename, filename)
      
      cat("Successfully downloaded", filename, "\n")
      
      # Add a small delay to avoid overwhelming the server
      Sys.sleep(2)
      
    }, error = function(e) {
      cat("Error downloading data for", format(current_date, "%Y-%m"), ":", e$message, "\n")
    })
  }
  
  cat("Download process completed.\n")
}
