plot_effects <- function(){
  
  #T values of both models
  
  mod_spamm_effects <- summary(mod_spamm)$beta_table
  
  mod_spamm_effects <- mod_spamm_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 |t_value > 1.96,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Density of vessl detections")
  
  mod_spamm_unmatched_effects <- summary(mod_spamm_unmatched)$beta_table
  
  mod_spamm_unmatched_effects <- mod_spamm_unmatched_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 |t_value > 1.96,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Density of unmatched vessel detections")
  
  mod_spamm_full = bind_rows(mod_spamm_effects,mod_spamm_unmatched_effects )
  
  (coefficient_plot = mod_spamm_full %>%
      ggplot(aes(x = reorder(term, -t_value), y = t_value)) +
      geom_point(size = 3, aes(color = model, shape = significance)) +
      geom_errorbar(aes(ymin = t_value - cond_se, ymax = t_value + cond_se, color = model), width = 0.2,alpha = 0.8) +  # Add error bars for confidence intervals
      scale_color_hp_d(option = "Ravenclaw") +
      labs(title = "t-values of both models",
           color = "Model type",
           shape = "Significance (t < -1.96 or > 1.96)",
           x = "",
           y = "t-value") +
      coord_flip() +
      theme_minimal(base_size = 14) +
      theme(legend.title.align = 0.5) +
      scale_y_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
      scale_x_discrete(labels = c(area_correct = "MPA area",
                                  depth_dist_to_shore = "Depth/Distance to shore",
                                  depth = "Depth",
                                  travel_time = "Travel time",
                                  intercept = "Intercept",
                                  mean_sst = "Sea surface temperature",
                                  sd_sst = "Sea surface temperature (standard deviation)",
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
                                  sd_chl = "Primary productivy (standard deviation)",
                                  mean_chl = "Primary productivity",
                                  marine2 = "Fully marine MPA")) +
      theme(legend.position = "bottom", legend.box = "vertical") +
      guides(shape = guide_legend(order = 1), 
             colour = guide_legend(order = 2)))
  
  #Iucn cat travel time
  
  #
  partial_iucn_cat <-  visreg(mod_spamm,"travel_time",type="conditional",by="iucn_cat")$fit
  
  partial_iucn_cat_plot <- ggplot(partial_iucn_cat) +
    geom_line(aes(travel_time,visregFit,color = iucn_cat)) +
    scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    labs(x = "Travel time to the nearest city - log scale",
         y = "Fitted coefficients",
         color = "IUCN Category",
         title = "Response: Density of vessel detections")
  
  partial_iucn_cat_unmatched <-  visreg(mod_spamm_unmatched,"travel_time",type="conditional",by="iucn_cat")$fit 
  
  partial_iucn_cat_unmatched_plot <- ggplot(partial_iucn_cat_unmatched) +
    geom_line(aes(travel_time,visregFit,color = iucn_cat)) +
    scale_color_manual(values = legend,breaks =c('I','II', 'III',"IV","V","VI","Not Applicable","Not Assigned","Not Reported","EEZ"))  + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    labs(x = "Travel time to the nearest city - log scale",
         y = "Fitted coefficients",
         color = "IUCN Category",
         title = "Response: Density of unmatched vessel detections")
  
  iucn_cats <- ggarrange(partial_iucn_cat_plot,partial_iucn_cat_unmatched_plot+ rremove("ylab"),nrow = 1, common.legend=T,legend="bottom")
  iucn_cats <-  annotate_figure(iucn_cats, top = text_grob("Effect of IUCN category on the density of vessel detections by travel time", face = "bold", size = 16))
  
  # Save coefficient_plot to fill half of an A4 page
  ggsave(coefficient_plot, 
         file = "figures/coefficient_plot.jpg", 
         width = 210 * 1.5,  # A4 width in mm
         height = 148.5  *1.5,  # Half of A4 height in mm
         units = "mm", 
         dpi = 300)
  
  # Save iucn_cats to fill the other half of an A4 page
  ggsave(iucn_cats, 
         file = "figures/iucn_cats.jpg", 
         width = 210 * 1.5,  # A4 width in mm
         height = 148.5 *1.5,  # Half of A4 height in mm
         units = "mm", 
         dpi = 300)
  
}
