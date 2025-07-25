#' Calculate Fishing Activity Statistics for MPAs
#'
#' This function computes key fishing activity metrics within Marine Protected Areas (MPAs), 
#' including the number of unmatched fishing vessels, fishing density, and the ratio of 
#' matched to unmatched detections.
#'
#' @param SAR_mpa_all A dataframe containing SAR detection data for MPAs.
#'
#' @return A dataframe (`SAR_stats_final`) with the following metrics for each MPA:
#' - `id_iucn`: Unique MPA identifier.
#' - `sum_all`: Total fishing detections (matched + unmatched).
#' - `relative_sum_all`: Fishing detections per km².
#' - `unmatched_ratio`: Proportion of unmatched fishing activity.
#' - `unmatched_relative`: Unmatched fishing per km².
#' - `iucn_cat`: IUCN protection category, with categories "Ia" and "Ib" merged into "I".
#'
#' @details
#' 1. Filters detections with at least 20 SAR images.
#' 2. Identifies unmatched fishing boats with `fishing_score >= 0.9`.
#' 3. Removes outliers based on vessel length (top 5%).
#' 4. Calculates total and relative fishing activity statistics.
#' 5. Computes the ratio of unmatched to total fishing.
#' 6. Merges results into a final dataset.
#'
#' @examples
#' SAR_stats <- calculate_stats_all(SAR_mpa_all)
#'
#' @export

calculate_stats_all <- function(SAR_mpa_all){
  
  SAR_stats <- SAR_mpa_all %>%
    st_drop_geometry() %>%
    # Only keep detections where at least 20 images were taken
    filter(image_count >= 20) %>%
    # Drop all factor levels except two
    mutate(matched_category = droplevels(factor(matched_category, levels = c("unmatched", "fishing"))),
           matched_category = as.character(matched_category)) %>%
    # If 90% sure that boat is fishing, it is unmatched and fishing
    mutate(category = as.factor(ifelse(matched_category == "unmatched" & fishing_score >= 0.9, "unmatched_fishing",
                                       matched_category))) %>%
    # Keep only fishing and unmatched_fishing
    filter(category %in% c("fishing", "unmatched_fishing")) %>%
    # Filter out very large vessels
    filter(length_m < 145) %>%
    filter(length_m < quantile(SAR_mpa_all$length_m, 0.95, na.rm = TRUE)) %>%
    # Normalize detection
    mutate(normalized_detection = 1/image_count) %>%
    # Calculate match count per MPA and category
    group_by(id_iucn, category) %>%
    mutate(match_count = sum(normalized_detection, na.rm = TRUE)) %>%
    ungroup() %>%
    # Reshape for summary
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