plot_effects <- function(){
  
  mod_spamm_effects <- summary(mod_spamm)$beta_table
  
  mod_spamm_effects <- mod_spamm_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 |t_value > 1.96,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Density of all fishing vessels")
  
  mod_spamm_unmatched_effects <- summary(mod_spamm_unmatched)$beta_table
  
  mod_spamm_unmatched_effects <- mod_spamm_unmatched_effects %>%
    clean_names() %>%
    as.data.frame() %>%
    mutate(significance = ifelse(t_value < -1.96 |t_value > 1.96,"Significant","Not Significant")) %>%
    rownames_to_column("term") %>%
    mutate(model = "Density of unmatched fishing vessels")
  
  mod_spamm_full = bind_rows(mod_spamm_effects,mod_spamm_unmatched_effects )
  
  coefficient_plot = mod_spamm_full %>%
    ggplot(aes(x=reorder(term, - t_value), y=t_value)) +
    geom_point(size = 3, aes(color = model,shape=significance)) +
    scale_color_hp_d(option = "Ravenclaw") +
    # geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.2)+
    labs(title = "t-values of both models",
         color = "Model type",
         shape =  "Significance (t < -1.96 or > 1.96)",
         x = "",
         y = "t-value") +
    coord_flip()+
    theme_minimal(base_size = 14)+
    theme(legend.title.align = 0.5) +
    scale_x_discrete(labels = c(gis_m_area = "MPA area",
                                bathymetry = "Depth",
                                travel_time= "Travel time",
                                intercept = "Intercept",
                                sst = "Sea surface temperature",
                                hf = "Human footprint",
                                conflicts = "Conflicts",
                                iucn_cat_ii = "IUCN category II",
                                gdp = "Gross domestic product",
                                hdi = "Human development index",
                                iucn_cat_iv = "IUCN category IV",
                                marine_ecosystem_dependency = "Marine ecosystem dependency",
                                salinity = "Salinity",
                                iucn_cat_v = "IUCN category V",
                                iucn_cat_vi = "IUCN category VI",
                                dist_to_coast = "Distance to coast",
                                chla = "Primary productivity",
                                marine2 = "Fully marine MPA")) +
    theme(legend.position = "bottom",legend.box = "vertical") +
    guides(shape = guide_legend(order = 1), 
           colour = guide_legend(order = 2))
  
  ggsave(coefficient_plot, file = "figures/coefficient_plot.jpg", width = 297, height = 210, units = "mm", dpi =300)
  
  
}