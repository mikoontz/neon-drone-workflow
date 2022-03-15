library(dplyr)
library(sf)
library(raster)
library(tmap)
library(viridis)

# files to be read in this script
multispec_photo_overlap_count_fname <- file.path("data", "out", paste0(site_name, "_", flight_datetime, "_", "multispec-photo-overlap-count.tif"))
# multispec_photo_overlap_pct_fname <- file.path("data", "out", paste0(site_name, "_", flight_datetime, "_", "multispec-photo-overlap-percent.tif"))
multispec_photos_metadata_fname <- paste0("data/drone/L0/", site_name, "_", flight_datetime, "_multispec-photos_metadata.csv")
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")

# files to be written in this script
figure_fname <- file.path("figs", "photo-points-overlap-and-gcps.png")

# read data to be used to build plots
multispec_meta <- 
  read.csv(multispec_photos_metadata_fname) %>% 
  dplyr::filter(band_name == "blue") %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[, "X"],
                lat = sf::st_coordinates(.)[, "Y"],
                # https://gis.stackexchange.com/questions/190198/how-to-get-appropriate-crs-for-a-position-specified-in-lat-lon-coordinates/190209#190209
                epsg = 32700 - round((45 + lat) / 90) * 100 + round((183 + lon) / 6))

# For now, assume that all the photos are taken in the same UTM zone. If that's not the case, the next processing
# step can be split up by grouping by the epsg column (use base::split then lapply())
multispec_meta <-
  multispec_meta %>% 
  st_transform(unique(.$epsg))

neon_plot_anchors <- 
  sf::st_read(gcp_locations_fname) %>% 
  sf::st_transform(st_crs(multispec_meta))

photo_count <- raster::raster(multispec_photo_overlap_count_fname)

# didn't end up using the bbox trick, but keeping it around for future reference
# https://stackoverflow.com/questions/60892033/how-do-you-position-the-title-and-legend-in-tmap
bbox_new <- sf::st_bbox(photo_count)
xrange <- bbox_new["xmax"] - bbox_new["xmin"] # range of x values
yrange <- bbox_new["ymax"] - bbox_new["ymin"] # range of y values

bbox_new["xmin"] <- bbox_new["xmin"] - (0.25 * xrange) # xmin - left
# bbox_new["xmax"] <- bbox_new["xmax"] + (0.25 * xrange) # xmax - right
# bbox_new["ymin"] <- bbox_new["ymin"] - (0.25 * yrange) # ymin - bottom
# bbox_new["ymax"] <- bbox_new["ymax"] + (0.2 * yrange) # ymax - top


combined_plot <-
  # tmap::tm_shape(photo_count, bbox = bbox_new) +
  tmap::tm_shape(photo_count) +
  tmap::tm_raster(palette = viridis::viridis(100), style = "cont", title = "Number of\noverlapping photos") +
  tmap::tm_grid(alpha = 0.25) +
  tmap::tm_xlab(text = "Easting (m)") +
  tmap::tm_ylab(text = "Northing (m)") +
  tmap::tm_shape(multispec_meta) +
  tmap::tm_symbols(col = "black", shape = 19, size = 0.1) +
  tmap::tm_shape(neon_plot_anchors) +
  tmap::tm_symbols(col = "red", shape = 19, size = 0.25) +
  tmap::tm_add_legend(type = "symbol",
                      labels = c("Photo points", "Ground control points"), 
                      col = c("black", "red"), 
                      shape = 19) +
  # tmap::tm_layout(legend.position = c("left", "bottom"))
tmap::tm_layout(legend.position = c(1.01, 0.65),
                outer.margins = c(0.01, 0.01, 0.01, 0.2))

combined_plot

tmap::tmap_save(tm = combined_plot, filename = figure_fname, width = 180, height = 135, dpi = 300, units = "mm")
