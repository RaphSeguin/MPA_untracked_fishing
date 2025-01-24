#' Calculate Fishing Statistics for EEZs Outside MPAs
#'
#' This function calculates various fishing-related statistics for Exclusive Economic Zones (EEZs) 
#' outside Marine Protected Areas (MPAs). It includes metrics such as the count of unmatched fishing 
#' detections, total fishing detections, normalized detection densities, and ratios.
#'
#' @param SAR_eez_final An `sf` object containing SAR detections outside MPAs, with normalized 
#' detection values and associated EEZ attributes.
#'
#' @return A data frame with aggregated statistics for each EEZ (`ISO_SOV1`), including:
#' - `fishing`: Count of matched fishing detections.
#' - `unmatched_fishing`: Count of unmatched fishing detections.
#' - `sum_all`: Total fishing detections (matched and unmatched).
#' - `relative_sum_all`: Total fishing detections per unit EEZ area.
#' - `unmatched_ratio`: Proportion of unmatched fishing detections to total detections.
#' - `unmatched_relative`: Unmatched fishing detections per unit EEZ area.
#'
#' @details
#' The function:
#' 1. Aggregates SAR detection counts by EEZ and detection category.
#' 2. Computes ratios and densities for unmatched and total fishing detections.
#' 3. Ensures proper handling of `NA` values to avoid incorrect calculations.

calculate_stats_outside_mpas <- function(SAR_eez_final){
  
  #For each MPA, calculating the number of unmatched fishing boats, the number of unmatched fishing boats/km^2 and the ratio between matched and unmatched
  SAR_stats_EEZ <- SAR_eez_final %>%
    #Correct area
    st_drop_geometry() %>%
    # Calculate, using dynamic column names
    group_by(ISO_SOV1, category) %>%
    mutate(match_count = sum(normalized_detection_EEZ, na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate stats
    pivot_wider(names_from = "category", values_from = "match_count") 
  
  # Function to coalesce columns
  coalesce_by_column <- function(df) {
    return(coalesce(df[1], df[2]))
  }
  
  # Calculating ratio for relative values only
  SAR_stats_EEZ_ratio <- SAR_stats_EEZ %>%
    group_by(ISO_SOV1) %>%
    # Keeping only distinct values of columns of interest for each MPA
    distinct(ISO_SOV1, unmatched_fishing, fishing, eez_area) %>%
    # Coalescing columns
    summarise_all(coalesce_by_column) %>%
    # NAs are 0 
    replace(is.na(.), 0) %>%
    # Illegal ratio
    mutate(sum_all = unmatched_fishing + fishing,
           relative_sum_all = (sum_all / eez_area),
           unmatched_ratio = unmatched_fishing / (fishing + unmatched_fishing),
           unmatched_relative = (unmatched_fishing / eez_area)) %>%
    # Keeping only the ratio
    dplyr::select(c(ISO_SOV1, fishing, unmatched_fishing, sum_all, relative_sum_all, unmatched_ratio, unmatched_relative))
  
  SAR_stats_eez_final <- SAR_stats_EEZ %>%
    dplyr::select(-c(fishing, unmatched_fishing)) %>%
    left_join(SAR_stats_EEZ_ratio, by = "ISO_SOV1") 
  
  return(SAR_stats_eez_final)
  
  
  
  
}