#' Prepare MPA Data for Analysis
#'
#' This function processes Marine Protected Area (MPA) data by integrating 
#' fishing activity statistics and relevant attributes to create a dataset for analysis.
#'
#' @param SAR_stats A dataframe containing SAR-based fishing statistics for MPAs.
#' @param mpa_wdpa An `sf` object representing the MPA dataset.
#'
#' @return A dataframe (`MPA_final_vars`) with the following processed variables:
#' - `id_iucn`: Unique MPA identifier.
#' - `iucn_cat`: IUCN protection category (categories "Ia" and "Ib" merged into "I").
#' - `status_yr`: Year of MPA establishment.
#' - `area_correct`: Corrected area of the MPA.
#' - `management_plan`: Factor variable indicating presence or absence of a management plan.
#' - `parent_iso`: Country ISO code.
#' - `country`: Country name.
#' - `average_length`: Mean vessel length per MPA.
#' - `fishing`: Total detected fishing activity.
#' - `unmatched_fishing`: Fishing activity not matched to known vessels.
#' - `sum_all`: Total SAR detections.
#' - `unmatched_relative`: Proportion of unmatched fishing activity.
#' - `relative_sum_all`: Fishing detections per unit area.
#' - `SAR_presence`: Factor indicating SAR detection presence (`SAR` or `No_SAR`).
#' - `matched_density`: Fishing activity per km².
#' - `unmatched_density`: Unmatched fishing activity per km².
#' - `SAR_density`: Total SAR detections per km².
#'
#' @details
#' 1. Aggregates SAR fishing statistics for each MPA.
#' 2. Merges fishing data with the MPA dataset.
#' 3. Fills missing fishing detection values with zero.
#' 4. Computes density metrics for fishing and SAR detections.
#' 5. Standardizes IUCN categories, merging "Ia" and "Ib" into "I".
#' 6. Assigns a management plan category (`Management plan` or `No management plan`).
#' 7. Converts country ISO codes to country names.
#'
#' @examples
#' MPA_data <- prep_data_for_analysis(SAR_stats, mpa_wdpa)
#'

prep_data_for_analysis <- function(SAR_stats, mpa_wdpa){
  
  SAR_stats_mpa <- SAR_stats %>%
    #Add average length per mpa
    group_by(id_iucn) %>%
    mutate(average_length = mean(length_m)) %>%
    ungroup() %>%
    #Select mpas
    distinct(id_iucn, .keep_all = T) %>%
    dplyr::select(id_iucn, average_length, fishing:unmatched_relative)
  
  MPA_final_vars <- mpa_wdpa %>%
    st_drop_geometry() %>%
    dplyr::select(id_iucn, iucn_cat, status_yr, area_correct, mang_plan, parent_iso) %>%
    dplyr::filter(id_iucn %in% all_mpas_SAR$id_iucn) %>%
    #Join sar data
    left_join(SAR_stats_mpa, by = "id_iucn") %>%
    mutate(across(c(fishing, unmatched_fishing,
                  sum_all, unmatched_relative, relative_sum_all), ~ replace_na(., 0))) %>%
    #Metrics for both years
    mutate(SAR_presence = as.factor(ifelse(sum_all == 0, "No_SAR","SAR")),
           SAR_presence= fct_relevel(SAR_presence, c("No_SAR","SAR")),
           matched_density = fishing/area_correct,
           unmatched_density = unmatched_fishing/area_correct,
           SAR_density = sum_all/area_correct) %>%
    # Convert iucn_cat, keeping original categories
    mutate(iucn_cat = as.factor(iucn_cat),
           iucn_cat = ifelse(iucn_cat %in% c("Ia", "Ib"), "I", as.character(iucn_cat))) %>%
    mutate(iucn_cat = factor(iucn_cat, levels = unique(c("I", levels(mpa_wdpa$iucn_cat))))) %>%
    # Distinct MPAs
    #Distinct MPAs
    distinct(id_iucn, .keep_all = T) %>%
    #Management plan info
    mutate(management_plan = as.factor(ifelse(mang_plan %in% c(" Management plan is not implemented and not available",
                                                     "No","Management plan is not implented and not available",
                                                     "Not Reported"),"No management plan","Management plan"))) %>%
    mutate(country = countrycode(parent_iso,origin="iso3c",destination="country.name"))
  
  return(MPA_final_vars)
  
}