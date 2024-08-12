plot_percentage_threats <- function(){
  
  #For each activity, percentage of cells with no activity
  mpa_percentage <- mpa_pressures %>%
    #Group by activity and protection status
    #For ports
    group_by(protected, port_presence) %>%
    mutate(cells_with_ports = n(),
           cells_with_ports_percentage = cells_with_ports/cell_number*100) %>%
    ungroup() %>%
    #For shipping
    group_by(protected, shipping_presence) %>%
    mutate(cells_with_shipping = n(),
           cells_with_shipping_percentage = cells_with_shipping/cell_number*100) %>%
    ungroup() %>%
    #For desalination
    group_by(protected, desalination_presence) %>%
    mutate(cells_with_desalination = n(),
           cells_with_desalination_percentage = cells_with_desalination/cell_number*100) %>%
    ungroup() %>%
    #For offshore
    group_by(protected, offshore_presence) %>%
    mutate(cells_with_offshore = n(),
           cells_with_offshore_percentage = cells_with_offshore/cell_number*100) %>%
    ungroup() %>%
    #For fishing
    group_by(protected, fishing_presence) %>%
    mutate(cells_with_fishing = n(),
           cells_with_fishing_percentage = cells_with_fishing/cell_number*100) %>%
    ungroup() %>%
    #For dredge
    group_by(protected, dredge_presence) %>%
    mutate(cells_with_dredge = n(),
           cells_with_dredge_percentage = cells_with_dredge/cell_number*100) %>%
    ungroup() %>%
    #For aggregate
    group_by(protected, aggregate_presence) %>%
    mutate(cells_with_aggregate = n(),
           cells_with_aggregate_percentage = cells_with_aggregate/cell_number*100) %>%
    ungroup() %>%
    #For unmatched
    group_by(protected, unmatched_presence) %>%
    mutate(cells_with_unmatched = n(),
           cells_with_unmatched_percentage = cells_with_unmatched/cell_number*100) %>%
    ungroup() 
  
  #Percentage of cells with each activity
  
  #ports
  plot_ports_percentage <- mpa_percentage %>%
    filter(port_presence == 1) %>%
    mutate(activity = "ports") %>%
    distinct(protected,cells_with_ports_percentage,activity) %>%
    ggplot(aes(protected, cells_with_ports_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Ports",
         x = " ",
         y = "Percentage of cells with activity") +
    guides(fill = "none")
  
  #shipping
  plot_shipping_percentage <- mpa_percentage %>%
    filter(shipping_presence == 1) %>%
    mutate(activity = "shipping") %>%
    distinct(protected,cells_with_shipping_percentage,activity) %>%
    ggplot(aes(protected, cells_with_shipping_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Shipping",
         x = " ",
         y = "Percentage of cells with activity") +
    guides(fill = "none")
  
  #Desalination
  plot_desalination_percentage <- mpa_percentage %>%
    filter(desalination_presence == 1) %>%
    mutate(activity = "desalination") %>%
    distinct(protected,cells_with_desalination_percentage,activity) %>%
    ggplot(aes(protected, cells_with_desalination_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Desalination",
         x = " ",
         y = "Percentage of cells with activity") +
    guides(fill = "none")
  
  #Offshore
  plot_offshore_percentage <- mpa_percentage %>%
    filter(offshore_presence == 1) %>%
    mutate(activity = "offshore") %>%
    distinct(protected,cells_with_offshore_percentage,activity) %>%
    ggplot(aes(protected, cells_with_offshore_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) +
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Offshore",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none")
  
  #Fishing
  plot_fishing_percentage <- mpa_percentage %>%
    filter(fishing_presence == 1) %>%
    mutate(activity = "fishing") %>%
    distinct(protected,cells_with_fishing_percentage,activity) %>%
    ggplot(aes(protected, cells_with_fishing_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Fishing",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none")
    
  #Dredge
  plot_dredge_percentage <- mpa_percentage %>%
    filter(dredge_presence == 1) %>%
    mutate(activity = "dredge") %>%
    distinct(protected,cells_with_dredge_percentage,activity) %>%
    ggplot(aes(protected, cells_with_dredge_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Dredge",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none")
  
  #Unmatched
  plot_unmatched_percentage <- mpa_percentage %>%
    filter(unmatched_presence == 1) %>%
    mutate(activity = "unmatched") %>%
    distinct(protected,cells_with_unmatched_percentage,activity) %>%
    ggplot(aes(protected, cells_with_unmatched_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Unmatched",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none")
  
  #Aggregate
  plot_aggregate_percentage <- mpa_percentage %>%
    filter(aggregate_presence == 1) %>%
    mutate(activity = "aggregate") %>%
    distinct(protected,cells_with_aggregate_percentage,activity) %>%
    ggplot(aes(protected, cells_with_aggregate_percentage,fill=activity)) +
    scale_x_discrete(labels = c("Unprotected","Protected")) + 
    scale_fill_manual(values = legend_activities) + 
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Aggregate",
         x = " ",
         y = "Percentage of cells with activity")+
    guides(fill = "none")
  
  ggarrange(plot_ports_percentage, plot_shipping_percentage + rremove("ylab"),
            plot_desalination_percentage+ rremove("ylab"),plot_offshore_percentage+ rremove("ylab"), 
            plot_fishing_percentage, plot_dredge_percentage+ rremove("ylab"),
             plot_unmatched_percentage+ rremove("ylab"), plot_aggregate_percentage+ rremove("ylab"), 
            nrow = 2, ncol = 4)
  
  ggsave("figures/percentage_activities.jpg", width = 297, height = 210, units = "mm", dpi = 300)
  
}
