# https://github.com/weecology/NeonTreeEvaluation_package
library(dplyr)
library(devtools)
# devtools::install_github("Weecology/NeonTreeEvaluation_package")
library(NeonTreeEvaluation)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))

# NeonTreeEvaluation::download()

df <- submission %>% filter(plot_name=="NIWO_017_2019")
results <- evaluate_field_stems(predictions = df, project = F, show = T, summarize = T)

rgb_path <- get_data(plot_name = "NIWO_017_2019", type="rgb")
rgb <- terra::rast(rgb_path)

res <- terra::res(rgb)
e <- sf::st_bbox(terra::ext(rgb))
full_tos_plot <- sf::st_as_sfc(e) %>% sf::st_set_crs(local_utm)

rgb1 <- terra::rast(get_data(plot_name = "NIWO_017_2019", type = "rgb"))
rgb2 <- terra::rast(get_data(plot_name = "NIWO_017_2018", type = "rgb"))


par(mfrow = c(1, 2))
terra::plotRGB(rgb1)
terra::plotRGB(rgb2)

xmls <- get_data("NIWO_017_2018", type = "annotations")
annotations2 <- xml_parse(xmls)

boxes2 <- NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = annotations2, raster_object = raster::stack(rgb2))
dev.off()
# boxes1 <- NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = crowns, raster_object = raster::stack(rgb2))

terra::plotRGB(rgb2)
plot(sf::st_geometry(boxes2), col = "red", add = TRUE)
# plot(sf::st_geometry(boxes1), col = "blue", add = TRUE)


crowns <- 
  sf::st_read(cropped_crowns_fname) %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_intersects(x = ., y = full_tos_plot, sparse = FALSE)) %>%
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(xmin_proj = NA, ymin_proj = NA, xmax_proj = NA, ymax_proj = NA)

for(i in 1:nrow(crowns)) {
  bbox <- st_bbox(crowns[i, "geometry"])
  crowns[i, "xmin_proj"] <- bbox$xmin
  crowns[i, "ymin_proj"] <- bbox$ymin
  crowns[i, "xmax_proj"] <- bbox$xmax
  crowns[i, "ymax_proj"] <- bbox$ymax
}


crowns <-
  crowns %>% 
  dplyr::mutate(xmin = (xmin_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymin = (ymin_proj - e$ymin) / (e$ymax - e$ymin) * nrow(rgb),
                xmax = (xmax_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymax = (ymax_proj - e$ymin) / (e$ymax - e$ymin) * nrow(rgb),
                height = height,
                score = NA,
                label = "Tree",
                plot_name = "NIWO_017_2019") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(xmin, ymin, xmax, ymax, plot_name) %>% 
  as_tibble()

# tos_trees <-
  # NeonTreeEvaluation:::clean_field_data(NeonTreeEvaluation::field) %>%
  # NeonTreeEvaluation::field %>% 
  # dplyr::filter(plotID == "NIWO_017") %>% 
  # as_tibble()
# 
# plot(st_geometry(tos_trees), col = "red", pch = 19)

crowns %>% filter(plot_name == "NIWO_017_2019")
NeonTreeEvaluation:::process_plot
NeonTreeEvaluation:::process_plot(predictions = df, 
             plot_name = "NIWO_017", show = TRUE, image_name = "NIWO_017_2019")

df %>% as_tibble
crowns
spatial_boxes <- crowns %>% NeonTreeEvaluation::boxes_to_spatial_polygons(., raster::raster(rgb)) %>% sf::st_as_sf() %>% mutate(height = predictions$height, 
                                                                                                                score = predictions$score)

results3 <- evaluate_field_stems(predictions = as_tibble(df), project = FALSE, show = TRUE, summarize = TRUE)

results3 <- evaluate_field_stems(predictions = crowns, project = FALSE, show = TRUE, summarize = TRUE)
results <- evaluate_image_crowns(predictions = df, project = FALSE, show = T, summarize = TRUE)


df<-submission %>% filter(plot_name %in% c("NIWO_017_2018"))

#Compute total recall and precision for the overlap data
results <- evaluate_image_crowns(predictions = df, project = T, show=TRUE, summarize = T)
drone_results <- evaluate_image_crowns(predictions = crowns, project = T, show = TRUE, summarize = T)


results<-evaluate_image_crowns(predictions = df,project = T, show=F, summarize = T)
?NeonTreeEvaluation::compute_precision_recall()
?evaluate_field_stems
