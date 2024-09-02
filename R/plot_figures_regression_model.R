plot_figures_regression_model <- function(mpa_model_regression, 
                                          mpa_model_regression_2022, mpa_model_regression_2023,
                                          formula_regression_2022,formula_regression_2023){
  
  #Train full model 2022
  mod_regression_2022 <- lmer(formula_regression_2022,
                             data = mpa_model_regression_2022)
  
  #Make t table figure
  mod_regression_2022_effects <- broom.mixed::tidy(mod_regression_2022,effects = "fixed",conf.int = T)
  
  mod_regression_2022_effects <- mod_regression_2022_effects %>%
    clean_names() %>%
    mutate(model = "Fishing effort - 2022")
  
  #Train full model 2023
  mod_regression_2023 <- lmer(formula_regression_2023,
                               data = mpa_model_regression_2023)
  
  #Make t table figure
  mod_regression_2023_effects <- broom.mixed::tidy(mod_regression_2023,effects = "fixed",conf.int = T)
  
  mod_regression_2023_effects <- mod_regression_2023_effects %>%
    clean_names() %>%
    mutate(model = "Fishing effort - 2023")
  
  mod_regression_full = bind_rows(mod_regression_2022_effects,mod_regression_2023_effects)
  
  #Table
  
  (coefficient_plot = mod_regression_full %>%
      ggplot(aes(x=reorder(term, - statistic), y=statistic)) +
      geom_point(size = 3, aes(color = model)) +
      scale_color_hp_d(option = "Ravenclaw") +
      # geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=0.2)+
      labs(title = "Statistics of both models",
           color = "Model type",
           shape =  "Significance (t < -1.96 or > 1.96)",
           x = "",
           y = "Statistic") +
      coord_flip()+
      theme_minimal(base_size = 14)+
      theme(legend.title.align = 0.5) +
      # scale_y_continuous(breaks = c(-10,-5,0,5,10,15,20,25,30)) +
      scale_x_discrete(labels = c(mean_chl = "Primary productivity",
                                  intercept = "Intercept",
                                  mean_sst = "Sea surface temperature",
                                  ais_reception_positions_per_day_class_A = "AIS reception: Type A transponders (pings/day)",
                                  depth = "Depth",
                                  ais_reception_positions_per_day_class_B = "AIS reception: Type B transponders (pings/day)",
                                  dist_to_shore = "Distance to the coast",
                                  fishing_2023_log = "Number of vessel detections in 2023",
                                  fishing_2022_log = "Number of vessel detections in 2022",
                                  AIS_fishing_2021_log = "AIS fishing effort in 2021 (log(hours))",
                                  AIS_fishing_2022_log = "AIS fishing effort in 2022 (log(hours))")) +
      theme(legend.position = "bottom",legend.box = "vertical") +
      guides(shape = guide_legend(order = 1), 
             colour = guide_legend(order = 2)))
  
  ggsave(coefficient_plot, file = "figures/supp/coefficient_plot_regression.jpg", width = 297, height = 210, units = "mm")
  
}