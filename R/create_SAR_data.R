create_SAR_data <- function(){
  
  # Downloaded from GFW
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
    filter(matched_category %in% c("unmatched","fishing")) %>%
    filter(length_m >= 15)
  
  SAR_data <- SAR_data %>%
    mutate(unique_id = paste0("id_",1:nrow(SAR_data)))
  
  #Timer series check
  SAR_time <- SAR_data %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      year_month = format(timestamp, "%Y-%m")  # Extract Year and Month
    )

  # # Create a monthly time series by summarizing the data (e.g., count entries per month)
  monthly_time_series <- SAR_time %>%
    group_by(year_month) %>%
    summarize(count = n()) %>%
    mutate(date = as.Date(paste0(year_month, "-01")))
  #
  # # Plot the time series with monthly breaks
  ggplot(monthly_time_series, aes(x = date, y = count)) +
    geom_line(color = "blue", size = 1) +       # Line plot
    geom_point(color = "red", size = 2) +      # Points on the line
    labs(
      title = "Monthly Time Series",
      x = "Date",
      y = "Count",
      caption = "Source: SAR_data"
    ) +
    scale_x_date(
      date_breaks = "1 month",                # Breaks every month
      date_labels = "%b %Y"                  # Labels as 'Month Year'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
    ) +
    ylim(0,150000)

  save(SAR_data, file = "data/SAR_data.Rdata")
  
  return(SAR_data)

}