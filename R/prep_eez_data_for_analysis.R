#' Prepare EEZ Data for Analysis
#'
#' This function processes the Exclusive Economic Zone (EEZ) dataset by integrating 
#' information on fishing activity, SAR detections, and relevant covariates. The resulting 
#' dataset includes EEZ regions that are outside MPAs and coastal buffers, with 
#' calculated fishing effort and SAR detection metrics.
#'
#' @param SAR_eez_stats A dataframe containing SAR detection statistics for EEZ regions, 
#' including fishing effort and unmatched detections.
#' @param eez_no_mpa An `sf` object representing EEZ areas outside MPAs and coastal buffers.
#'
#' @return A dataframe (`EEZ_final_vars`) with the following variables:
#' - `ISO_SOV1`: Sovereign country code.
#' - `SOVEREIGN1`: Country name.
#' - `eez_area`: Area of the unprotected EEZ in square kilometers.
#' - `average_length`: Average vessel length in each EEZ.
#' - `fishing`: Total detected fishing activity.
#' - `unmatched_fishing`: Fishing activity not matched to AIS. 
#' - `sum_all`: Total SAR detections.
#' - `unmatched_relative`: Proportion of unmatched fishing activity.
#' - `SAR_presence`: Factor variable indicating presence or absence of SAR detections.
#' - `matched_density`: Fishing activity per unit area.
#' - `unmatched_density`: Unmatched fishing activity per unit area.
#' - `SAR_density`: Total SAR detections per unit area.
#' - `country`: Country name derived from `ISO_SOV1`.
#'
#' @details
#' The function follows these steps:
#' 1. Aggregates SAR detection statistics at the EEZ level.
#' 2. Merges EEZ statistics with the unprotected EEZ shapefile
#' 3. Replaces missing values in fishing and detection metrics with zero.
#' 4. Computes SAR presence/absence and density measures.
#' 5. Converts country codes to country names and assigns EEZ classification.
#'
#' @examples
#' EEZ_data <- prep_eez_data_for_analysis(SAR_eez_stats, eez_no_mpa)
#'
#' 

prep_eez_data_for_analysis <- function(SAR_eez_stats, eez_no_mpa){
  
  SAR_stats_eez <- SAR_eez_stats %>%
    #Add average length per mpa
    group_by(ISO_SOV1) %>%
    mutate(average_length = mean(length_m)) %>%
    ungroup() %>%
    #Select mpas
    distinct(ISO_SOV1, .keep_all = T) %>%
    dplyr::select(ISO_SOV1, average_length, fishing:unmatched_relative)
  
  EEZ_final_vars <- eez_no_mpa %>%
    st_drop_geometry() %>%
    dplyr::select(ISO_SOV1, SOVEREIGN1, eez_area) %>%
    #Join sar data
    left_join(SAR_stats_eez, by = "ISO_SOV1") %>%
    mutate(across(c(fishing, unmatched_fishing,
                    sum_all, unmatched_relative, relative_sum_all), ~ replace_na(., 0))) %>%
    #Metrics for both years
    mutate(SAR_presence = as.factor(ifelse(sum_all == 0, "No_SAR","SAR")),
           SAR_presence= fct_relevel(SAR_presence, c("No_SAR","SAR")),
           matched_density = fishing/eez_area,
           unmatched_density = unmatched_fishing/eez_area,
           SAR_density = sum_all/eez_area) %>%
    #Distinct MPAs
    distinct(ISO_SOV1, .keep_all = T) %>%
    filter(ISO_SOV1 != "SYC") %>%
    mutate(country = countrycode(ISO_SOV1,origin="iso3c",destination="country.name")) %>%
    mutate(iucn_cat = "EEZ")
  
  return(EEZ_final_vars)
  
  
}