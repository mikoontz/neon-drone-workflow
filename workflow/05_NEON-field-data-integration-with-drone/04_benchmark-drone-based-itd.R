# https://github.com/weecology/NeonTreeEvaluation_package
library(dplyr)
library(devtools)
# devtools::install_github("Weecology/NeonTreeEvaluation_package")
library(NeonTreeEvaluation)
library(data.table)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")

gcp <- sf::st_read(gcp_locations_fname)
crowns_raw <- sf::st_read(cropped_crowns_fname)

gcp <- sf::st_transform(gcp, sf::st_crs(crowns))

# Get the data from the NeonTreeEvaluation package (it's big! Almost 4 GB)
# NeonTreeEvaluation::download()

rgb_path <- get_data(plot_name = "NIWO_017_2019", type="rgb")
rgb <- terra::rast(rgb_path)
res <- terra::res(rgb)

# This is the extent of the RGB image represented as an sf polygon
# We can use this to represent the 40 x 40 m plot, as it is clipped for
# the DeepForest uses
e <- sf::st_bbox(terra::ext(rgb))

# trees are only mapped on the ground within the innermost 20 x 20m plot
interior_20m_plot <-
  gcp %>% 
  dplyr::filter(pointID %in% c(49, 51, 33, 31)) %>% 
  sf::st_geometry() %>%
  sf::st_union() %>% 
  sf::st_cast("POLYGON") %>% 
  sf::st_transform(sf::st_crs(crowns_raw))

target_trees <-
  crowns_raw %>% 
  sf::st_drop_geometry() %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crowns_raw)) %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_20m_plot, sparse = FALSE))

crowns <-
  crowns_raw %>% 
  # dplyr::filter(sf::st_intersects(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_intersects(x = ., y = full_tos_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_within(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  # dplyr::filter(treeID %in% target_trees$treeID) %>%
  dplyr::rename(geometry = geom) %>% 
  dplyr::mutate(xmin_proj = NA, ymin_proj = NA, xmax_proj = NA, ymax_proj = NA)

for(i in 1:nrow(crowns)) {
  bbox <- sf::st_bbox(crowns[i, "geometry"])
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
                crown_id = 1:nrow(.),
                height = height,
                score = NA,
                label = "Tree",
                plot_name = "NIWO_017_2019") %>% 
  # sf::st_drop_geometry() %>%
  dplyr::select(treeID, crown_id, xmin, ymin, xmax, ymax, plot_name)

# We can turn our crown polygons into boxes and overlay them relative to the 
# raster object of interest (in this case the RGB image).
# We also filter out all the boxes to display just the ones within the smaller
# plot (where the tree stems are mapped-- the 20 x 20 m plot
boxes_for_20m_plot <- 
  NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = crowns, raster_object = raster::raster(rgb)) %>% 
  dplyr::filter(sf::st_within(x = ., y = interior_20m_plot, sparse = FALSE))

crowns_for_20m_plot <-
  crowns %>% 
  # dplyr::filter(sf::st_intersects(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_intersects(x = ., y = full_tos_plot, sparse = FALSE)) %>%
  # dplyr::filter(sf::st_within(x = ., y = interior_20m_plot, sparse = FALSE)) %>%
  dplyr::filter(treeID %in% target_trees$treeID) %>%
  # dplyr::filter(crown_id %in% boxes_for_20m_plot$crown_id) %>% 
  sf::st_drop_geometry()

terra::plotRGB(rgb)
plot(sf::st_geometry(boxes_for_20m_plot), col = "blue", add = TRUE)
plot(sf::st_geometry(crowns), col = "red", add = TRUE)

# Evaluate our drone-predicted tree boxes based on the NEON field-measured
# tree locations
field_stem_results_20m <- NeonTreeEvaluation::evaluate_field_stems(predictions = crowns_for_20m_plot,
                                                                  project = TRUE,
                                                                  show = TRUE,
                                                                  summarize = TRUE)

# The 40 x 40 m plot is the full TOS plot. This is the extent of the annotations
# from the DeepForest project, and it's the extent that we want to use to
# compare our segmented crown boxes to the DeepForest annotated ones
full_40m_plot <- sf::st_as_sfc(e) %>% sf::st_set_crs(sf::st_crs(crowns))

# These are the trees as measured in the field by NEON
# There is some discrepancy between the stems reported here, and those 
# reported in the official NEON product (code to retrieve that below)
tos_trees <-
  NeonTreeEvaluation::field %>% 
  # NeonTreeEvaluation:::clean_field_data() %>%
  dplyr::filter(plotID == "NIWO_017") %>%
  sf::st_as_sf(coords = c("itcEasting","itcNorthing"),
               crs = sf::st_crs(crowns))

# The NEON product to get the tree stem locations (and othre attributes) caan
# be downloaded using this code from the neonUtilities package
# We want the "Woody Plant Vegetation Structure" data (DP1.10098.001)
# Get it for all years so that we can make sure we have a complete stem map (not all trees are measured every year)
woody_plant_veg_structure <- neonUtilities::loadByProduct(dpID = "DP1.10098.001", 
                                                          site = "NIWO", 
                                                          package = "basic",
                                                          check.size = FALSE)

# Then we can use the geoNEON package to convert the data product downloaded
# above into a spatial points object
# Reiterating the note above-- there appears to be some discrepancy between
# the tree locations as determined with the {geoNEON} package versus those
# determined from the {NeonTreeEvaluation} package.
neon_trees <- 
  woody_plant_veg_structure$vst_mappingandtagging %>% 
  dplyr::filter(plotID == "NIWO_017") %>% 
  geoNEON::getLocTOS(dataProd = "vst_mappingandtagging") %>% 
  dplyr::rename(easting = adjEasting,
                northing = adjNorthing) %>% 
  dplyr::filter(!is.na(easting)) %>% 
  sf::st_as_sf(coords = c("easting", "northing"), crs = sf::st_crs(crowns))

# Showing the discrepancy between the two datasets
terra::plotRGB(rgb)
plot(st_geometry(tos_trees), col = "red", pch = 19, add = TRUE)
plot(st_geometry(neon_trees), col = "blue", pch = 19, add = TRUE)

# these represent the drone-derived crown segments that fall within the 
# 40 x 40 m plots
# There weren't crown annotations for NIWO_017_2019, so we'll use the 
# annotations from the year before (NIWO_017_2018) for illustrative purposes
# which appear to be based on the same RGB image
crowns_for_40m_plot <-
  crowns %>% 
  dplyr::filter(sf::st_within(x = ., y = full_40m_plot, sparse = FALSE)) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(plot_name = "NIWO_017_2018")

# As before, we can convert the crowns data frame into polygons (essentially 
# representing each crown polygon as its bounding box and then filter to only
# crown boxes that fall entirely within the 40 x 40 m plot. This provides the
# best match to the crown annotations which we can use to compare our results
boxes_for_40m_plot <- 
  NeonTreeEvaluation::boxes_to_spatial_polygons(boxes = crowns, raster_object = raster::raster(rgb)) %>% 
  dplyr::filter(sf::st_within(x = ., y = full_40m_plot, sparse = FALSE))

# Compare our drone-segmented crowns with the DeepForest annotated crowns to
# assess the recall and precision
results <- evaluate_image_crowns(predictions = crowns_for_40m_plot, 
                                 project = TRUE, show = TRUE, summarize = TRUE)

# We can also break down the assessment process more finely by building the
# DeepForest crown boxes 
deepforest_boxes <- 
  NeonTreeEvaluation::get_data(plot_name = "NIWO_017_2018", type = "annotations") %>% 
  NeonTreeEvaluation::xml_parse() %>% 
  NeonTreeEvaluation::boxes_to_spatial_polygons(raster_object = raster::raster(rgb))

# Some ideosyncracies in the {NeonTreeEvaluation} package means that we should
# re-determine the crown_id number for each crown so they go 1 through the
# number of rows
rownames(boxes_for_40m_plot) <- 1:nrow(boxes_for_40m_plot)
boxes_for_40m_plot$crown_id <- 1:nrow(boxes_for_40m_plot)

# Here we are also able to change the "intersection-over-union" threshold 
# to determinee whether a matched tree is a true positive (i.e., how well does
# prediction overlap with a ground truth tree)
low_thresh_drone_results <- NeonTreeEvaluation::compute_precision_recall(ground_truth = deepforest_boxes, predictions = boxes_for_40m_plot, threshold = 0.1)

# We can calculate the F-Score (which incorporates both recall and precision)
# as a single assessment value. It's the harmonic mean between the two values.
drone_f_score <- 2 * (low_thresh_drone_results["precision"] * low_thresh_drone_results["recall"]) / (low_thresh_drone_results["precision"] + low_thresh_drone_results["recall"])

names(drone_f_score) <- "f_score"

terra::plotRGB(rgb)
plot(st_geometry(deepforest_boxes), col = "blue", add = TRUE)
plot(st_geometry(boxes_for_40m_plot), col = "red", add = TRUE)

# Note that DeepForest predicted tree crowns can also be found here:
# Ben Weinstein, Sergio Marconi, Alina Zare, Stephanie Bohlman, Sarah Graves, Aditya Singh, & Ethan White. (2020). NEON Tree Crowns Dataset (0.0.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3765872
# https://doi.org/10.5281/zenodo.3765871
idtrees <- data.table::fread(file.path("data", "raw", "idtrees", "NIWO_2019.csv"))

# From the .h5 file we downloaded to get the hyperspectral data
niwo_017_geo_index <- "451000_4432000"
# 106837 trees predicted in this NEON AOP tile
idtrees <- idtrees[geo_index == niwo_017_geo_index, ]

# Determine the centroid of each tree based on its bounding box
# also determine the image positions relative to the RGB image extent
# note (as above when we did this with our drone-derived tree crowns
# that the required "image position" for the NeonTreeEvaluation package
# starts at 0 at the *top* of the image, then *increases* as it goes down
# which is the opposite of how Northing works in geographic space
# we have to account for this when setting the ymin and ymax variables

idtrees[, easting := (left + right) / 2]
idtrees[, northing := (top + bottom) / 2]
idtrees[, `:=`(xmin = (left - e$xmin) / (e$xmax - e$xmin) * ncol(rgb), 
               ymin = -(top - e$ymax) / (e$ymax - e$ymin) * nrow(rgb), 
               xmax = (right - e$xmin) / (e$xmax - e$xmin) * ncol(rgb), 
               ymax = -(bottom - e$ymax) / (e$ymax - e$ymin) * nrow(rgb))]

# Convert trees to a spatial object
idtrees_sf <- 
  sf::st_as_sf(idtrees, 
               coords = c("easting", "northing"), 
               crs = sf::st_crs(crowns), 
               remove = FALSE)

idtrees_niwo_017_sf <-
  idtrees_sf %>% 
  dplyr::filter(sf::st_intersects(x = ., y = full_40m_plot, sparse = FALSE)) %>% 
  dplyr::select(xmin, ymin, xmax, ymax)

deepforest_predicted_boxes <- 
  idtrees_niwo_017_sf %>% 
  sf::st_drop_geometry() %>% 
  NeonTreeEvaluation::boxes_to_spatial_polygons(raster_object = raster::raster(rgb))

low_thresh_deepforest_results <- NeonTreeEvaluation::compute_precision_recall(ground_truth = deepforest_boxes, predictions = deepforest_predicted_boxes, threshold = 0.1)

# We can calculate the F-Score (which incorporates both recall and precision)
# as a single assessment value. It's the harmonic mean between the two values.
deepforest_f_score <- 2 * (low_thresh_deepforest_results["precision"] * low_thresh_deepforest_results["recall"]) / (low_thresh_deepforest_results["precision"] + low_thresh_deepforest_results["recall"])

names(deepforest_f_score) <- "f_score"

terra::plotRGB(rgb)
plot(st_geometry(deepforest_boxes), col = "blue", add = TRUE)
plot(st_geometry(deepforest_predicted_boxes), col = "red", add = TRUE)
