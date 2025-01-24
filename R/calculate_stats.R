#' Calculate Fishing Activity Statistics for a Selected Year
#'
#' Computes key fishing activity metrics within Marine Protected Areas (MPAs) for a given year, 
#' including the number of unmatched fishing vessels, fishing density, and the ratio of 
#' matched to unmatched detections.
#'
#' @param SAR_mpa_final A dataframe containing SAR detection data for MPAs.
#' @param year_selected The year for which statistics should be calculated.
#'
#' @return A dataframe (`SAR_stats_final`) with the following metrics for each MPA:
#' - `id_iucn`: Unique MPA identifier.
#' - `sum_all`: Total fishing detections (matched + unmatched).
#' - `relative_sum_all`: Fishing detections per km².
#' - `unmatched_ratio`: Proportion of unmatched fishing activity.
#' - `unmatched_relative`: Unmatched fishing per km².
#' - `iucn_cat`: IUCN protection category, with "Ia" and "Ib" merged into "I".
#'
#' @details
#' 1. Filters detections with at least 20 SAR images for the selected year.
#' 2. Identifies unmatched fishing boats with `fishing_score >= 0.9`.
#' 3. Removes outliers based on vessel length (top 5%).
#' 4. Computes total and relative fishing activity statistics.
#' 5. Computes the ratio of unmatched to total fishing.
#' 6. Merges results into a final dataset.
#'
#' @examples
#' SAR_stats_2023 <- calculate_stats(SAR_mpa_final, 2023)
#'
#' @export

calculate_stats <- function(SAR_mpa_final, year_selected){
  
  # Create dynamic column names based on the year_selected
  image_count_col <- paste0("image_count_", year_selected)
  normalized_detection_col <- paste0("normalized_detection_", year_selected)
  
  #For each MPA, calculating the number of unmatched fishing boats, the number of unmatched fishing boats/km^2 and the ratio between matched and unmatched
  SAR_stats <- SAR_mpa_final %>%
    st_drop_geometry() %>%
    # Only keep detections where at least 20 images were taken
    filter((!is.na(.data[[image_count_col]]) & .data[[image_count_col]] >= 20)) %>%
    filter(year == year_selected) %>%
    # Cleaning data 
    # Converting country name
    mutate(country = countrycode(iso3, origin="iso3c", destination="country.name")) %>%
    # Drop all factor levels except two
    mutate(matched_category = droplevels(factor(matched_category, levels = c("unmatched", "fishing"))),
           matched_category = as.character(matched_category)) %>%
    # If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = as.factor(ifelse(matched_category == "unmatched" & fishing_score >= 0.9, "unmatched_fishing",
                                       matched_category))) %>%
    # Keep only fishing and unmatched_fishing
    filter(category %in% c("fishing", "unmatched_fishing")) %>%
    # Also if unmatched_fishing and length higher than 90% quantile then delete it 
    filter(length_m < 145) %>%
    filter(length_m < quantile(SAR_mpa_final$length_m, 0.95, na.rm = T)) %>%
    # Calculate, using dynamic column names
    group_by(id_iucn, category) %>%
    mutate(match_count = sum(.data[[normalized_detection_col]], na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate stats
    pivot_wider(names_from = "category", values_from = "match_count") 
  
  # Function to coalesce columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  # Calculating ratio for relative values only
  SAR_stats_ratio <- SAR_stats %>%
    group_by(id_iucn) %>%
    # Keeping only distinct values of columns of interest for each MPA
    distinct(id_iucn, unmatched_fishing, fishing, area_correct) %>%
    # Coalescing columns
    summarise_all(coalesce_by_column) %>%
    # NAs are 0 
    replace(is.na(.), 0) %>%
    # Illegal ratio
    mutate(sum_all = unmatched_fishing + fishing,
           relative_sum_all = (sum_all / area_correct),
           unmatched_ratio = unmatched_fishing / (fishing + unmatched_fishing),
           unmatched_relative = (unmatched_fishing / area_correct)) %>%
    # Keeping only the ratio
    dplyr::select(c(id_iucn, fishing, unmatched_fishing, sum_all, relative_sum_all, unmatched_ratio, unmatched_relative))
  
  SAR_stats_final <- SAR_stats %>%
    dplyr::select(-c(fishing, unmatched_fishing)) %>%
    left_join(SAR_stats_ratio, by = "id_iucn") %>%
    mutate(iucn_cat = ifelse(iucn_cat %in% c("Ia", "Ib"), "I", iucn_cat))
  
  return(SAR_stats_final)
  
  }