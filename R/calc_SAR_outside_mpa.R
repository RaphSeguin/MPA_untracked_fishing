#' Calculate SAR Detections Outside MPAs
#'
#' This function identifies and processes Synthetic Aperture Radar (SAR) detections that occur 
#' outside Marine Protected Areas (MPAs) and within unprotected Exclusive Economic Zones (EEZ). 
#' It filters the detections based on fishing activity and vessel length and joins the data with 
#' the EEZ boundaries.
#'
#' @param SAR_data_sf An `sf` object containing SAR detections with attributes such as 
#' `matched_category`, `fishing_score`, and `length_m`, along with spatial coordinates (`lon`, `lat`).
#' @param eez_no_mpa An `sf` object representing the unprotected EEZ boundaries.
#'
#' @return An `sf` object of SAR detections that are classified as "fishing" or "unmatched_fishing," 
#' joined with the unprotected EEZ boundaries. Each detection includes relevant attributes such as 
#' the detection's category, vessel length, and unique identifiers.
#'
#' @details
#' The function performs the following operations:
#' 1. Filters SAR detections to retain only those classified as "fishing" or "unmatched_fishing."
#' 2. Excludes detections with vessel lengths exceeding the 95th percentile of the dataset.
#' 3. Converts SAR detections into an `sf` object for spatial analysis.
#' 4. Joins the processed SAR detections with the unprotected EEZ boundaries.
#' 5. Ensures no duplicate detections by retaining only unique identifiers.

calc_SAR_outside_mpa <- function(SAR_data_sf, eez_no_mpa){
  
  SAR_outside_mpas <- SAR_data_sf %>%
    # Drop all factor levels except two
    mutate(matched_category = droplevels(factor(matched_category, levels = c("unmatched", "fishing"))),
           matched_category = as.character(matched_category)) %>%
    # If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = as.factor(ifelse(matched_category == "unmatched" & fishing_score >= 0.9, "unmatched_fishing",
                                       matched_category))) %>%
    # Keep only fishing and unmatched_fishing
    filter(category %in% c("fishing", "unmatched_fishing")) %>%
    # Also if unmatched_fishing and length higher than 95% quantile then delete it 
    filter(length_m < 145) %>%
    filter(length_m < quantile(SAR_data_sf$length_m, 0.95, na.rm = TRUE)) %>%
    # Convert to sf and join with EEZ
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_join(eez_no_mpa, left = FALSE) %>%
    distinct(unique_id, .keep_all = TRUE)
  
  return(SAR_outside_mpas)
  
}
