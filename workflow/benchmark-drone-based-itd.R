# https://github.com/weecology/NeonTreeEvaluation_package
library(dplyr)
library(devtools)
# devtools::install_github("Weecology/NeonTreeEvaluation_package")
library(NeonTreeEvaluation)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")

gcp <- sf::st_read(gcp_locations_fname)
crowns <- sf::st_read(cropped_crowns_fname)

gcp <- sf::st_transform(gcp, sf::st_crs(crowns))

# trees are only mapped on the ground within the innermost 20 x 20m plot
interior_20m_plot <-
  gcp %>% 
  dplyr::filter(pointID %in% c(49, 51, 33, 31)) %>% 
  sf::st_geometry() %>%
  sf::st_union() %>% 
  sf::st_cast("POLYGON")

target_trees <-
  crowns %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crowns)) %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_20m_plot, sparse = FALSE))

crowns <-
  crowns %>% 
  # dplyr::filter(sf::st_intersects(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_intersects(x = ., y = full_tos_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_within(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  # dplyr::filter(treeID %in% target_trees$treeID) %>%
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(xmin_proj = NA, ymin_proj = NA, xmax_proj = NA, ymax_proj = NA)

for(i in 1:nrow(crowns)) {
  bbox <- st_bbox(crowns[i, "geometry"])
  crowns[i, "xmin_proj"] <- bbox$xmin
  crowns[i, "ymin_proj"] <- bbox$ymin
  crowns[i, "xmax_proj"] <- bbox$xmax
  crowns[i, "ymax_proj"] <- bbox$ymax
}

# note that the required "image position" for the NeonTreeEvaluation package
# starts at 0 at the *top* of the image, then *increases* as it goes down
# which is the opposite of how Northing works in geographic space
# we have to account for this when setting the ymin and ymax variables
crowns <-
  crowns %>% 
  dplyr::mutate(xmin = (xmin_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymin = -(ymax_proj - e$ymax) / (e$ymax - e$ymin) * nrow(rgb),
                xmax = (xmax_proj - e$xmin) / (e$xmax - e$xmin) * ncol(rgb),
                ymax = -(ymin_proj - e$ymax) / (e$ymax - e$ymin) * nrow(rgb),
                height = height,
                score = NA,
                label = "Tree",
                plot_name = "NIWO_017_2019") %>% 
  # sf::st_drop_geometry() %>%
  dplyr::select(xmin, ymin, xmax, ymax, plot_name)


# NeonTreeEvaluation::download()

rgb_path <- get_data(plot_name = "NIWO_017_2019", type="rgb")
rgb <- terra::rast(rgb_path)
res <- terra::res(rgb)
e <- sf::st_bbox(terra::ext(rgb))

boxes_for_20m_plot <- 
  NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = crowns, raster_object = raster::raster(rgb)) %>% 
  dplyr::filter(sf::st_within(x = ., y = interior_20m_plot, sparse = FALSE))
  

terra::plotRGB(rgb)
plot(sf::st_geometry(boxes_for_20m_plot), col = "blue", add = TRUE)
plot(sf::st_geometry(crowns), col = "red", add = TRUE)

full_40m_plot <- sf::st_as_sfc(e) %>% sf::st_set_crs(sf::st_crs(crowns))

tos_trees <-
  NeonTreeEvaluation::field %>% 
  NeonTreeEvaluation:::clean_field_data() %>%
  dplyr::filter(plotID == "NIWO_017") %>%
  sf::st_as_sf(coords = c("itcEasting","itcNorthing"),
               crs = sf::st_crs(crowns))

crowns_for_40m_plot <-
  crowns %>% 
  dplyr::filter(sf::st_within(x = ., y = full_40m_plot, sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(plot_name = "NIWO_017_2018")

boxes_for_40m_plot <- 
  NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = crowns, raster_object = raster::raster(rgb)) %>% 
  dplyr::filter(sf::st_within(x = ., y = full_40m_plot, sparse = FALSE))

results <- evaluate_image_crowns(predictions = crowns_for_40m_plot, 
                                 project = TRUE, show = TRUE, summarize = TRUE)

deepforest_boxes <- 
  NeonTreeEvaluation::get_data(plot_name = "NIWO_017_2018", type = "annotations") %>% 
  NeonTreeEvaluation::xml_parse() %>% 
  NeonTreeEvaluation::boxes_to_spatial_polygons(raster_object = raster::raster(rgb))

rownames(boxes_for_40m_plot) <- 1:nrow(boxes_for_40m_plot)
boxes_for_40m_plot$crown_id <- 1:nrow(boxes_for_40m_plot)

low_thresh_results <- NeonTreeEvaluation::compute_precision_recall(ground_truth = deepforest_boxes, predictions = boxes_for_40m_plot, threshold = 0.1)

2 * (low_thresh_results["precision"] * low_thresh_results["recall"]) / (low_thresh_results["precision"] + low_thresh_results["recall"])

terra::plotRGB(rgb)
plot(st_geometry(deepforest_boxes), col = "blue", add = TRUE)
plot(st_geometry(boxes_for_40m_plot), col = "red", add = TRUE)
