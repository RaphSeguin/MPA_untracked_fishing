#' Plot Coefficient Estimates for Regression Models
#'
#' This function fits **linear mixed-effects regression models** for fishing effort 
#' across three years (`2022, 2023, 2024`) and generates a **coefficient plot** 
#' comparing their statistical significance.
#'
#' @param mpa_model_regression A dataframe containing MPA regression data.
#' @param mpa_model_regression_2022 A dataframe for training the regression model for **2022**.
#' @param mpa_model_regression_2023 A dataframe for training the regression model for **2023**.
#' @param mpa_model_regression_2024 A dataframe for training the regression model for **2024**.
#' @param formula_regression_2022 A formula specifying the regression model for **2022**.
#' @param formula_regression_2023 A formula specifying the regression model for **2023**.
#' @param formula_regression_2024 A formula specifying the regression model for **2024**.
#'
#' @return Saves the following outputs:
#' - `figures/supp/mod_regression_full.csv`: A CSV file with model coefficients across all years.
#' - `figures/supp/coefficient_plot_regression.jpg`: A plot visualizing the coefficient estimates.
#'
#' @details
#' 1. **Fits a linear mixed-effects regression model** (`lmer()`) for each year.
#' 2. **Extracts model coefficients** and renames variables for clarity.
#' 3. **Combines results across years** into a single dataframe.
#' 4. **Generates a coefficient plot** (`ggplot2`):
#'    - Displays t-statistics for each predictor.
#'    - Color codes based on model year (`2022, 2023, 2024`).
#'    - Uses `scale_color_hp_d(option = "Ravenclaw")` for color styling.
#' 5. **Saves the plot and coefficient table** for further analysis.
#'
#' @examples
#' plot_figures_regression_model(mpa_model_regression, mpa_model_regression_2022, 
#'                               mpa_model_regression_2023, mpa_model_regression_2024, 
#'                               log(fishing_2022) ~ ., log(fishing_2023) ~ ., log(fishing_2024) ~ .)
#'


plot_figures_regression_model <- function(mpa_model_regression, 
                                          mpa_model_regression_2022, mpa_model_regression_2023, mpa_model_regression_2024,
                                          formula_regression_2022, formula_regression_2023, formula_regression_2024){
  
  # Train full model 2022
  mod_regression_2022 <- lmer(formula_regression_2022,
                              data = mpa_model_regression_2022)
  
  # Make t table figure
  mod_regression_2022_effects <- broom.mixed::tidy(mod_regression_2022, effects = "fixed", conf.int = T) %>%
    clean_names() %>%
    mutate(model = "Fishing effort - 2022") %>%
    mutate(across(where(is.numeric), round, 2))
  
  # Train full model 2023
  mod_regression_2023 <- lmer(formula_regression_2023,
                              data = mpa_model_regression_2023)
  
  # Make t table figure
  mod_regression_2023_effects <- broom.mixed::tidy(mod_regression_2023, effects = "fixed", conf.int = T) %>%
    clean_names() %>%
    mutate(model = "Fishing effort - 2023") %>%
    mutate(across(where(is.numeric), round, 2))
  
  # Train full model 2024
  mod_regression_2024 <- lmer(formula_regression_2024,
                              data = mpa_model_regression_2024)
  
  # Make t table figure
  mod_regression_2024_effects <- broom.mixed::tidy(mod_regression_2024, effects = "fixed", conf.int = T) %>%
    clean_names() %>%
    mutate(model = "Fishing effort - 2024") %>%
    mutate(across(where(is.numeric), round, 2))
  
  # Combine all years
  mod_regression_full <- bind_rows(mod_regression_2022_effects, mod_regression_2023_effects, mod_regression_2024_effects) %>%
    mutate(term = recode(term,
                         mean_chl = "Primary productivity",
                         intercept = "Intercept",
                         mean_sst = "Sea surface temperature",
                         ais_reception_positions_per_day_class_A = "AIS reception: Type A transponders (pings/day)",
                         depth = "Depth",
                         ais_reception_positions_per_day_class_B = "AIS reception: Type B transponders (pings/day)",
                         dist_to_shore = "Distance to the coast",
                         fishing_2022_log = "Number of vessel detections in 2022",
                         fishing_2023_log = "Number of vessel detections in 2023",
                         fishing_2024_log = "Number of vessel detections in 2024",
                         AIS_fishing_2021_log = "AIS fishing effort in 2021 (log(hours))",
                         AIS_fishing_2022_log = "AIS fishing effort in 2022 (log(hours))",
                         AIS_fishing_2023_log = "AIS fishing effort in 2023 (log(hours))",
                         AIS_fishing_2024_log = "AIS fishing effort in 2024 (log(hours))"
    ))
  
  write.csv(mod_regression_full, file = "figures/supp/mod_regression_full.csv")
  
  # Table and plot
  coefficient_plot <- mod_regression_full %>%
    ggplot(aes(x = reorder(term, -statistic), y = statistic)) +
    geom_point(size = 3, aes(color = model)) +
    scale_color_hp_d(option = "Ravenclaw") +
    labs(title = "Statistics of all models",
         color = "Model type",
         shape = "Significance (t < -1.96 or > 1.96)",
         x = "",
         y = "Statistic") +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(legend.title.align = 0.5) +
    scale_x_discrete(labels = c(
      mean_chl = "Primary productivity",
      intercept = "Intercept",
      mean_sst = "Sea surface temperature",
      ais_reception_positions_per_day_class_A = "AIS reception: Type A transponders (pings/day)",
      depth = "Depth",
      ais_reception_positions_per_day_class_B = "AIS reception: Type B transponders (pings/day)",
      dist_to_shore = "Distance to the coast",
      fishing_2022_log = "Number of vessel detections in 2022",
      fishing_2023_log = "Number of vessel detections in 2023",
      fishing_2024_log = "Number of vessel detections in 2024",
      AIS_fishing_2021_log = "AIS fishing effort in 2021 (log(hours))",
      AIS_fishing_2022_log = "AIS fishing effort in 2022 (log(hours))",
      AIS_fishing_2023_log = "AIS fishing effort in 2023 (log(hours))",
      AIS_fishing_2024_log = "AIS fishing effort in 2024 (log(hours))"
    )) +
    theme(legend.position = "bottom", legend.box = "vertical") +
    guides(shape = guide_legend(order = 1),
           colour = guide_legend(order = 2))
  
  ggsave(coefficient_plot, file = "figures/supp/coefficient_plot_regression.jpg", width = 297, height = 210, units = "mm")
}
