#' Plot Performance Distribution of Binomial and Regression Models
#'
#' This function generates **performance evaluation plots** for binomial classification (stage 1) 
#' and regression models (stage 2) across the years **2022, 2023, and 2024**.
#'
#' @return Saves the following output:
#' - `figures/supp/full_performance.jpg`: A combined figure showing performance metrics for both models.
#'
#' @details
#' 1. **Processes binomial model performance**:
#'    - Merges performance metrics (`F1 Score`, `Precision`, `Recall`, `ROC AUC`) from **2022, 2023, and 2024**.
#'    - **Creates violin and jitter plots** to visualize performance distribution across years.
#' 2. **Processes regression model performance**:
#'    - Merges performance metrics (`RMSE`, `MAE`, `MedAE`, `R²`) from **2022, 2023, and 2024**.
#'    - **Creates violin and jitter plots** for distribution visualization.
#' 3. **Combines performance plots** for binomial and regression models.
#' 4. **Saves the final figure** as a `.jpg` file with high resolution.
#'
#' @examples
#' plot_performance_distribution()
#'


plot_performance_distribution <- function(){
  
  # Performance of stage 1 model
  binomial_performance <- bind_rows(
    binomial_performance_2022 %>% mutate(model = "2022"),
    binomial_performance_2023 %>% mutate(model = "2023"),
    binomial_performance_2024 %>% mutate(model = "2024")
  )
  
  f1_score <- ggplot(binomial_performance, aes(model, F1_Score)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    ylim(0,1) +
    labs(x = " ", y = "F1 score")
  
  precision <- ggplot(binomial_performance, aes(model, Precision)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    ylim(0,1) +
    labs(x = " ", y = "Precision")
  
  recall <- ggplot(binomial_performance, aes(model, Recall)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    ylim(0,1) +
    labs(x = " ", y = "Recall")
  
  roc_auc <- ggplot(binomial_performance, aes(model, ROC_AUC)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    ylim(0,1) +
    labs(x = " ", y = "ROC AUC")
  
  binomial_performance_plot <- ggarrange(f1_score, precision, recall, roc_auc, nrow = 4)
  
  # Performance of stage 2 model
  regression_performance <- bind_rows(
    regression_performance_2022 %>% mutate(model = "2022"),
    regression_performance_2023 %>% mutate(model = "2023"),
    regression_performance_2024 %>% mutate(model = "2024")
  )
  
  rmse <- ggplot(regression_performance, aes(model, RMSE)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    labs(x = " ", y = "Root mean square deviation")
  
  mae <- ggplot(regression_performance, aes(model, MAE)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    labs(x = " ", y = "Mean absolute error")
  
  medae <- ggplot(regression_performance, aes(model, MedAE)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    labs(x = " ", y = "Median absolute error")
  
  r2 <- ggplot(regression_performance, aes(model, R2)) + 
    geom_jitter() + 
    geom_violin(alpha = 0.2) + 
    my_custom_theme() + 
    ylim(0,1) +
    labs(x = " ", y = "R-squared")
  
  regression_performance_plot <- ggarrange(rmse, mae, medae, r2, nrow = 4)
  
  full_performance <- ggarrange(binomial_performance_plot, regression_performance_plot, ncol = 2)
  
  ggsave(full_performance, 
         file = "figures/supp/full_performance.jpg",
         width = 190 * 1.5,
         height = 220 * 1.5,
         units = "mm",
         dpi = 300)
}
