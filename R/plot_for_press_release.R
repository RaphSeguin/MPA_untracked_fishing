plot_press <- function(){
  
#Prep plot data
full_SAR_data <- SAR_stats %>%
  bind_rows(SAR_eez_stats %>% dplyr::rename(parent_iso = "ISO_SOV1")) %>%
  distinct(unique_id, .keep_all = T) %>%
  left_join(SAR_data_sf %>% dplyr::select(unique_id), by = "unique_id") %>%
  st_as_sf()

world <- ne_countries(scale = "large")

##Now create inset maps
world_4326 <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") 

plot_region_map <- function(full_SAR_data, mpa_wdpa, world_4326, bbox, region_name, point_size) {
  
  # Crop the data
  cropped_data <- list(
    SAR = st_crop(full_SAR_data, bbox),
    MPA = st_crop(mpa_wdpa, bbox),
    World = st_crop(world_4326, bbox)
  )
  
  # Create label points centered on land
  label_points <- st_point_on_surface(cropped_data$World)
  
  matched_category_colors <- c("fishing" = "#6DAEDB", "unmatched" = "#E3B778")
  
  # Function to generate map with a given SAR dataset
  generate_map <- function(sar_data, filename_suffix) {
    map <- ggplot() +
      geom_sf(data = cropped_data$World, fill = "#0A0E10", size = 0.03, color = "grey") +
      geom_sf(data = barrier_reef, fill = NA, color = "white", linewidth = 0.5, alpha = 0.7) +
      geom_sf(data = sar_data, aes(color = matched_category), size = point_size, stroke = 0, alpha = 0.5) +
      # geom_sf_text(
      #   data = label_points,
      #   aes(label = name),
      #   color = "white",
      #   size = 5,  # Increased font size
      #   fontface = "bold",
      #   check_overlap = TRUE
      # ) +
      scale_color_manual(
        values = matched_category_colors,
        name = "Fishing vessels",
        labels = c("fishing" = "Publicly tracked", "unmatched" = "Not publicly tracked")
      ) +
      theme_map() +
      theme(
        legend.position = "bottom",
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_text(color = "white", size = 12, hjust = 0.5),
        legend.text = element_text(color = "white", size = 10),
        legend.box.just = "center",
        legend.justification = "center",
        legend.direction = "horizontal",
        legend.key.size = unit(1.5, "lines"),
        panel.background = element_rect(fill = "#10161B", color = NA),
        plot.background = element_rect(fill = "#10161B", color = NA),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0)
      ) +
      annotate("rect", 
               xmin = bbox["xmin"], xmax = bbox["xmax"], 
               ymin = bbox["ymin"], ymax = bbox["ymax"], 
               color = "black", fill = NA, linewidth = 0.5) +
      guides(color = guide_legend(
        override.aes = list(shape = 16, size = 5, alpha = 1),
        title.position = "top"
      ))
    
    # Save image
    plot_path <- paste0("figures/", region_name, "_", filename_suffix, ".jpg")
    ggsave(map, file = plot_path, width = 1920*6, height = 1080*6, units = "px", dpi = 600)
    return(plot_path)
  }
  
  # Filtered datasets
  tracked_only <- cropped_data$SAR[cropped_data$SAR$matched_category == "fishing", ]
  both_categories <- cropped_data$SAR
  
  # Generate maps
  tracked_path <- generate_map(tracked_only, "tracked")
  full_path <- generate_map(both_categories, "full")
  
  # # Create a GIF with smooth transition
  # img_tracked <- image_read(tracked_path)
  # img_full <- image_read(full_path)
  # 
  # # Generate intermediate frames for smooth transition
  # morph_frames <- image_morph(c(img_tracked, img_full), frames = 10)
  # 
  # # Add a pause on the last frame
  # pause_duration <- 10  # Number of times to repeat the last frame
  # last_frame <- morph_frames[length(morph_frames)]
  # pause_frames <- rep(last_frame, pause_duration)
  # 
  # # Combine frames
  # final_frames <- c(morph_frames, pause_frames)
  # 
  # # Create the animation
  # gif <- image_animate(final_frames, fps = 10, dispose = "previous")
  # 
  # # Save the gif
  # gif_path <- paste0("figures/", region_name, "_transition.gif")
  # image_write(gif, gif_path)
}


#BBOX
bbox_north_sea <- st_bbox(c(xmin = -6, ymin = 47, xmax = 5, ymax = 53), crs = st_crs(full_SAR_data))
bbox_senegal <- st_bbox(c(xmin = -20, ymin = 10,xmax = -10, ymax = 17), crs = st_crs(full_SAR_data))
bbox_gambie <- st_bbox(c(xmin = -18, ymin = 12.2,xmax = -16, ymax = 13.2), crs = st_crs(full_SAR_data))
bbox_southamerica <- st_bbox(c(xmin = -70, ymin = -57,xmax = -50, ymax = -29), crs = st_crs(full_SAR_data))
bbox_china <- st_bbox(c(xmin = 100, ymin = 15,xmax = 135, ymax = 54), crs = st_crs(full_SAR_data))

bbox_barrierreef <- st_bbox(c(xmin = 141, ymin = -25,xmax = 155, ymax = -9), crs = st_crs(full_SAR_data))

# Plot and save the maps
plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_north_sea, "north_sea", 0.5)
plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_senegal, "Senegal", 0.8)
plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_gambie, "Gambia", 0.7)
plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_southamerica, "South America", 0.5)
plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_china, "China", 0.2)

plot_region_map(full_SAR_data, mpa_wdpa, world_4326, bbox_barrierreef, "Barrier_reef", 1.9)

# Crop the data
cropped_data <- list(
  SAR = st_crop(full_SAR_data, bbox_barrierreef),
  MPA = st_crop(mpa_wdpa, bbox_barrierreef),
  World = st_crop(world_4326, bbox_barrierreef)
)

fishing_barrier <- st_join(fishing_effort_clean, barrier_reef, left = F)
coords <- st_coordinates(fishing_barrier)
fishing_barrier_final <- fishing_barrier %>%
  cbind(coords) %>%
  group_by(X,Y) %>%
  reframe(fishing_hours = sum(apparent_fishing_hours)) %>%
  ungroup() %>%
  filter(fishing_hours > 0) %>%
  st_as_sf(coords = c("X","Y"), crs = 4326)

plot_ais_barrier <- ggplot() +
  geom_sf(data = cropped_data$World, fill = "#0A0E10", size = 0.03, color = "grey") +
  geom_sf(data = barrier_reef, fill = NA, color = "white", linewidth = 0.5, alpha = 0.7) +
  geom_sf(data = fishing_barrier_final, aes(color = log(fishing_hours + 1)), stroke = 0, size = 0.5) +
  scale_color_gradientn(
    colors = c("#ecda9a", "#f66356", "#ee4d5a"),
    name = "Fishing effort (log scale)"
  ) +
  theme_map() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_text(color = "white", size = 12, hjust = 0.5),
    legend.text = element_text(color = "white", size = 10),
    legend.box.just = "center",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.key.size = unit(1.5, "lines"),
    panel.background = element_rect(fill = "#10161B", color = NA),
    plot.background = element_rect(fill = "#10161B", color = NA),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0)
  ) +
  annotate("rect", 
           xmin = bbox_barrierreef["xmin"], xmax = bbox_barrierreef["xmax"], 
           ymin = bbox_barrierreef["ymin"], ymax = bbox_barrierreef["ymax"], 
           color = "black", fill = NA, linewidth = 0.5) +
  guides(color = guide_legend(
    override.aes = list(shape = 16, size = 5, alpha = 1),
    title.position = "top"
  ))

ggsave(plot_ais_barrier, file = "figures/plot_ais_barrier.jpg", width = 1920 * 4, height = 1080 *4 , units = "px", dpi = 600)

}
