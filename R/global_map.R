# 
# # Load World Map Data
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# 
# # Simplify Global Map by thinning vessel detections
# all_sar_thinned <- all_sar %>%
#   sample_frac(0.1)  # Randomly sample 10% of the data for better global readability
# 
# # Base Global Map
# global_map <- ggplot() +
#   geom_sf(data = world, fill = "lightgrey", color = NA) +
#   geom_sf(data = MPA_sf, fill = "lightblue", color = NA, alpha = 0.4) +
#   geom_sf(data = all_sar_thinned, aes(color = matched_category), shape = ".", alpha = 0.3) +
#   scale_color_manual(values = c("fishing" = "darkorange", "unmatched_fishing" = "deepskyblue")) +
#   theme_map() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
#   labs(title = "Global Overview of Vessel Detections in MPAs")
# 
# # Function to Create Insets
# create_inset <- function(region_bbox, title) {
#   ggplot() +
#     geom_sf(data = st_crop(world, region_bbox), fill = "lightgrey", color = NA) +
#     geom_sf(data = st_crop(MPA_sf, region_bbox), fill = "lightblue", color = NA, alpha = 0.4) +
#     geom_sf(data = st_crop(all_sar, region_bbox), aes(color = matched_category), shape = ".", alpha = 0.6) +
#     scale_color_manual(values = c("fishing" = "darkorange", "unmatched_fishing" = "deepskyblue")) +
#     theme_map() +
#     theme_void() +
#     labs(title = title)
# }
# 
# # Define bounding boxes for each region
# bbox_med <- st_bbox(c(xmin = -10, xmax = 40, ymin = 30, ymax = 46), crs = st_crs(world))
# bbox_hokkaido <- st_bbox(c(xmin = 140, xmax = 150, ymin = 40, ymax = 46), crs = st_crs(world))
# bbox_gbr <- st_bbox(c(xmin = 142, xmax = 154, ymin = -24, ymax = -10), crs = st_crs(world))
# bbox_ca <- st_bbox(c(xmin = -90, xmax = -60, ymin = 5, ymax = 20), crs = st_crs(world))
# 
# # Create Insets
# mediterranean_map <- create_inset(bbox_med, "North-West Mediterranean")
# hokkaido_map <- create_inset(bbox_hokkaido, "Hokkaidō (Japan)")
# gbr_map <- create_inset(bbox_gbr, "Great Barrier Reef (Australia)")
# ca_map <- create_inset(bbox_ca, "Central America")
# 
# # Arrange the Global Map with Insets
# final_plot <- ggdraw() +
#   draw_plot(global_map, 0, 0, 1, 1) +
#   draw_plot(mediterranean_map, 0.05, 0.55, 0.4, 0.4) +
#   draw_plot(hokkaido_map, 0.55, 0.55, 0.4, 0.4) +
#   draw_plot(gbr_map, 0.55, 0.05, 0.4, 0.4) +
#   draw_plot(ca_map, 0.05, 0.05, 0.4, 0.4)
# 
# # Save the Final Plot
# ggsave(final_plot, file = "figures/global_with_insets_final.jpg", 
#        width = 297, 
#        height = 210, 
#        units = "mm", 
#        dpi = 300)
