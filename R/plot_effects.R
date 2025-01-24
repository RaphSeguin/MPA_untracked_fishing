#' Generate and Save Effect Plots for Fishing Models
#'
#' This function visualizes the effects of **IUCN category and other covariates** 
#' on vessel detections and fishing effort using **binomial and regression models**.
#'
#' @return Saves the following outputs:
#' - `figures/fig_3_A.jpg`: Boxplot of predicted vessel detection probability by IUCN category.
#' - `figures/coefficient_plot.jpg`: Coefficient plot showing **t-values** for vessel detection models.
#' - `figures/iucn_cats.svg`: Effect of **IUCN category and travel time** on vessel detections.
#'
#' @details
#' 1. **Loads trained models**:
#'    - `mod_spamm_binomial` (binomial model for vessel presence).
#'    - `mod_spamm` (regression model for vessel detections).
#'    - `mod_spamm_unmatched` (regression model for untracked detections).
#' 2. **Figure A**: **Predicted probability of vessel detections by IUCN category**.
#'    - Uses `visreg()` to extract partial effects.
#'    - Overlays boxplots and **predicted probability curves**.
#' 3. **Figure B**: **Coefficient plot of model t-values**.
#'    - Extracts **t-values** from both regression models.
#'    - Highlights **significant** predictors.
#' 4. **Figure C**: **Effect of IUCN category and travel time** on vessel detections.
#'    - Uses `visreg()` to visualize **travel time effects** by IUCN category.
#' 5. **Saves all plots** in `.jpg` and `.svg` formats.
#'

plot_effects <- function(){
  
  #Plot binomial
  # Extract partial effect of IUCN category (predicted probabilities)
  load("output/mod_spamm_binomial.Rdata")
  load("output/mod_spamm.Rdata")
  load("output/mod_spamm_unmatched.Rdata")
  vis_iucn <- visreg(mod_spamm_binomial, "iucn_cat", scale = "response", plot = FALSE)
  
  # Convert to a dataframe
  vis_iucn_df <- vis_iucn$fit %>%
    rename(iucn_cat = vis_iucn$meta$x, predicted_prob = visregFit, lower = visregLwr, upper = visregUpr) %>%
    mutate(predicted_prob = predicted_prob * 100)
  
  #Predicted probabilities of model
  probs_presence <- predict(mod_spamm_binomial, mpa_vessel_model)
  mpa_vessel_model$probs <- probs_presence * 100
  # 
  # mpa_vessel_model %>%
  #   group_by(iucn_cat) %>%
  #   summarize(mean = mean(probs),
  #             sd = sd(probs))
  
  (fig_3_A <- ggplot(mpa_vessel_model, aes(iucn_cat, probs, fill = iucn_cat)) + 
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(values = legend, labels = level_order, name = "IUCN Category") +
    # Transform predicted probabilities to match primary y-axis (-5 to 5)
    geom_line(data = vis_iucn_df, aes(x = iucn_cat, y = predicted_prob), 
              color = "red", group = 1, linewidth = 1) + 
    my_custom_theme() +
    labs(
      x = " ", 
      y = "Predicted probability of vessel presence (%)", 
      title = "Effect of IUCN Category on fishing vessel detections"
    ))
  
  ggsave(fig_3_A, file = "figures/fig_3_A.jpg", width = 210 *2 , height = 92 *2 , units = "mm", dpi = 300)
  
  ggsave(fig_3_A, file = "figures/fig_3_A.svg", width = 18.3*2 , height =  8.6 * 2 , units = "cm")
  
  #T values of both models
  
  mod_spamm_effects <- summary(mod_spamm,  details = list(p_value = TRUE))$beta_table
  
  mod_spamm_effects <- mod_spamm_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 & p_value <0.01 |t_value > 1.96 & p_value < 0.01,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Vessel detections")
  
  mod_spamm_unmatched_effects <- summary(mod_spamm_unmatched, details = list(p_value = TRUE))$beta_table
  
  mod_spamm_unmatched_effects <- mod_spamm_unmatched_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 & p_value < 0.01 |t_value > 1.96 & p_value < 0.01,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Untracked vessel detections")
  
  mod_spamm_full = bind_rows(mod_spamm_effects,mod_spamm_unmatched_effects )
  
  (coefficient_plot = mod_spamm_full %>%
      ggplot(aes(x = reorder(term, -t_value), y = t_value)) +
      geom_point(size = 3, aes(color = model, shape = significance)) +
      geom_errorbar(aes(ymin = t_value - cond_se, ymax = t_value + cond_se, color = model), width = 0.2,alpha = 0.8) +  # Add error bars for confidence intervals
      scale_color_hp_d(option = "Ravenclaw") +
      labs(title = "t-values of both models",
           color = "Model type",
           shape = "Significance (t < -1.96 or > 1.96 and p < 0.01)",
           x = "",
           y = "t-value") +
      coord_flip() +
      theme_minimal(base_size = 20) +
      theme(
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)
      ) + 
      theme(legend.title.align = 0.5) +
      scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
      scale_x_discrete(labels = c(area_correct = "MPA area",
                                  depth_dist_to_shore = "Depth/Distance to shore",
                                  depth = "Depth",
                                  travel_time = "Travel time",
                                  intercept = "Intercept",
                                  mean_sst = "Sea surface temperature",
                                  sd_sst = "Sea surface temperature (SD)",
                                  hf = "Human footprint",
                                  conflicts = "Conflicts",
                                  iucn_cat_i = "IUCN category I",
                                  iucn_cat_ii = "IUCN category II",
                                  gdp = "Gross domestic product",
                                  hdi = "Human development index",
                                  iucn_cat_iv = "IUCN category IV",
                                  marine_ecosystem_dependency = "Marine ecosystem dependency",
                                  salinity = "Salinity",
                                  iucn_cat_v = "IUCN category V",
                                  iucn_cat_vi = "IUCN category VI",
                                  dist_to_shore = "Distance to shore",
                                  sd_chl = "Primary productivy (SD)",
                                  mean_chl = "Primary productivity",
                                  marine2 = "Fully marine MPA")) +
      theme(legend.position = "bottom", legend.box = "vertical") +
      guides(shape = guide_legend(order = 1), 
             colour = guide_legend(order = 2)))
  
  #Iucn cat travel time
  partial_iucn_cat <-  visreg(mod_spamm,"travel_time",type="conditional",by="iucn_cat")$fit
  
  partial_iucn_cat_plot <- ggplot(partial_iucn_cat) +
    geom_line(aes(travel_time,visregFit,color = iucn_cat), linewidth = 1) +
    scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
    my_custom_theme() + 
    theme(legend.position = "bottom") +
    labs(x = "Travel time to the nearest city - log scale",
         y = "Partial effect on number of tracked detections",
         color = "IUCN Category",
         title = "Response: Vessel detections")
  
  # partial_iucn_cat_unmatched <-  visreg(mod_spamm_unmatched,"travel_time",type="conditional",by="iucn_cat")$fit 
  # 
  # partial_iucn_cat_unmatched_plot <- ggplot(partial_iucn_cat_unmatched) +
  #   geom_line(aes(travel_time,visregFit,color = iucn_cat)) +
  #   scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
  #   theme_minimal(base_size = 14) +
  #   theme(legend.position = "bottom") +
  #   labs(x = "Travel time to the nearest city - log scale",
  #        y = "Fitted coefficients",
  #        color = "IUCN Category",
  #        title = "Response: Untracked vessel detections")
  
  # iucn_cats <- ggarrange(partial_iucn_cat_plot,partial_iucn_cat_unmatched_plot+ rremove("ylab"),nrow = 1, common.legend=T,legend="bottom")
  # iucn_cats <-  annotate_figure(iucn_cats, top = text_grob("Effect of IUCN category on the density of vessel detections by travel time", face = "bold", size = 16))
  # 
  # Save coefficient_plot to fill half of an A4 page
  ggsave(coefficient_plot, 
         file = "figures/coefficient_plot.jpg", 
         width = 210 *2 , height = 92 *2 ,
         units = "mm", 
         dpi = 300)
  
  ggsave(coefficient_plot, 
         file = "figures/coefficient_plot.svg",  width = 18.3*2 , height =  8.6 * 2 , units = "cm")
  
  # Save iucn_cats to fill the other half of an A4 page
  ggsave(partial_iucn_cat_plot, 
         file = "figures/iucn_cats.svg",  width = 18.3*2 , height =  8.6 * 2 , units = "cm")
  
}
