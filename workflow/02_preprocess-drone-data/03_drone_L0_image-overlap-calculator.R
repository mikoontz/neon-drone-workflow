# Purpose: We want to assess the degree of overlap in photos from a mission.
# The challenge is that the flight planning app uses camera characteristics
# from the DJI camera to plan missions, but the camera characteristics from
# the multispectral camera are different and produce different overlap than
# the DJI camera. Further, even the overlap for the DJI camera is different
# than how it was planned if a double grid mission were flown or if an "offset"
# was used (where overlap is planned using an altitude X, but the drone is
# flown at an altitude of X + offset. This produces greater overlap on the
# ground than the planned overlap at X).

# the variables named with "planned" refer to the overlap/footprint length/etc.
# at X meters below the camera lens (reflecting the altitude of X used to plan
# the mission). The variables named with "actual" refer to the overlap/footprint
# length/etc. on the ground (reflecting the altitude of X + offset at which
# the drone actually flew.)

# Load dependencies

library(raster)
library(fasterize)
library(sf)
library(tidyverse)
library(viridis)
library(rasterVis)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read within this script
rgb_photo_meta_fname <- paste0("data/drone/L0/", site_name, "_", flight_datetime, "_rgb-photos_metadata.csv")
multispec_photos_metadata_fname <- paste0("data/drone/L0/", site_name, "_", flight_datetime, "_multispec-photos_metadata.csv")

# files to be written from this script
multispec_photo_overlap_count_fname <- file.path("data", "out", paste0(site_name, "_", flight_datetime, "_", "multispec-photo-overlap-count.tif"))
multispec_photo_overlap_pct_fname <- file.path("data", "out", paste0(site_name, "_", flight_datetime, "_", "multispec-photo-overlap-percent.tif"))

# function to create image footprint around point locations of each image ---------------------
# based on AGL, camera properties, and yaw (angles the footprint)
image_footprint <- function(obj, rad = TRUE) {
  
  # the coordinates of each image center
  pts <- st_coordinates(obj)
  
  # When rad = TRUE, the Yaw column of obj is assumed to be in radians
  # If rad = FALSE, then convert degrees to radians
  if(!rad) obj$Yaw <- obj$Yaw * pi / 180
  
  # Define the boundaries of the image footprint rectangle (as though camera is
  # facing due north)
  # horizontal and vertical footprints have already been calculated
  xmin <- pts[, "X"] - (pull(obj, h_footprint) / 2)
  xmax <- pts[, "X"] + (pull(obj, h_footprint) / 2)
  ymin <- pts[, "Y"] - (pull(obj, v_footprint) / 2)
  ymax <- pts[, "Y"] + (pull(obj, v_footprint) / 2)
  
  properties <- tibble(x = pts[, "X"], y = pts[, "Y"], agl = obj$agl, yaw = obj$Yaw, xmin, xmax, ymin, ymax)
  
  rotated_rect_polys <- 
    properties %>% 
    pmap(.f = function(x, y, agl, yaw, xmin, xmax, ymin, ymax) {
      rect_poly <- st_polygon(list(matrix(c(xmin, ymax, 
                                            xmax, ymax, 
                                            xmax, ymin, 
                                            xmin, ymin, 
                                            xmin, ymax), 
                                          byrow = TRUE, 
                                          ncol = 2)))
      
      # Get the center point
      center <- st_point(c(x, y))
      
      # rotate the polygon using an affine transform
      rotator <- matrix(c(cos(yaw), sin(yaw), -sin(yaw), cos(yaw)), nrow = 2, ncol = 2)
      
      rotated_rect_poly <-
        (rect_poly - center) * rotator + center
      
      return(rotated_rect_poly)
    })
  
  
  new_obj <-
    obj %>%
    st_drop_geometry() %>% 
    dplyr::mutate(geometry = st_sfc(rotated_rect_polys, crs = st_crs(obj))) %>% 
    st_as_sf()
  
  return(new_obj) 
}

# flight characteristics --------------------------------------------------

x3_planned_forward_overlap <- 0.95
x3_planned_side_overlap <- 0.9
agl_m <- 100 
offset_m <- 0


# RGB camera --------------------------------------------------------------


# zenmuse x3 sensor characteristics ---------------------------------------
# https://www.helicomicro.com/wp-content/uploads/2015/09/x5-x5r.pdf

x3_sensor_width_px <- 4000  
x3_sensor_height_px <- 3000

x3_focal_length_mm <- 3.6 # could be 4.8, 12, or 70
x3_sensor_width_mm <- 6.17 # Fixed for Rededge3; sensor width to focal length proportion is equal to swath width to altitude proportion
x3_sensor_height_mm <- 4.55

# Note this is set in the mission planner app and is limited by the write speed 
# of the DJI X3 camera to the SD card. 0.5 is the fastest possible speed that 
# MapPilot can tell the DJI X3 camera to write, and that will only work with a fast 
# SD card
x3_shutter_speed <- 0.5 # 

# Expected (non-double-gridded) overlap of RGB imagery based on flight and camera characteristics
x3_h_footprint_planned_agl_m <- (x3_sensor_width_mm / x3_focal_length_mm) * agl_m
x3_v_footprint_planned_agl_m <- (x3_sensor_height_mm / x3_focal_length_mm) * agl_m

transect_spacing <- (1 - x3_planned_side_overlap) * x3_h_footprint_planned_agl_m
flight_speed <- (1 - x3_planned_forward_overlap) * x3_v_footprint_planned_agl_m * x3_shutter_speed

x3_h_footprint_actual_agl_m <- (x3_sensor_width_mm / x3_focal_length_mm) * (agl_m + offset_m)
x3_v_footprint_actual_agl_m <- (x3_sensor_height_mm / x3_focal_length_mm) * (agl_m + offset_m)

x3_actual_side_overlap_agl <- 1 - (transect_spacing / x3_h_footprint_actual_agl_m)
x3_actual_forward_overlap_agl <- 1 - (flight_speed / x3_shutter_speed / x3_v_footprint_actual_agl_m)

# Actual overlap based on image locations and their footprints
x3_meta <- 
  read_csv(rgb_photo_meta_fname) %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) %>% 
  dplyr::mutate(h_footprint = (x3_sensor_width_mm / x3_focal_length_mm) * (agl),
                v_footprint = (x3_sensor_height_mm / x3_focal_length_mm) * (agl)) %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[, "X"],
                lat = sf::st_coordinates(.)[, "Y"],
                # https://gis.stackexchange.com/questions/190198/how-to-get-appropriate-crs-for-a-position-specified-in-lat-lon-coordinates/190209#190209
                epsg = 32700 - round((45 + lat) / 90) * 100 + round((183 + lon) / 6))

# For now, assume that all the photos are taken in the same UTM zone. If that's not the case, the next processing
# step can be split up by grouping by the epsg column (use base::split() then lapply() or dplyr::group_by() then dplyr::group_split() then purrr::map())
x3_meta <-
  x3_meta %>% 
  st_transform(unique(.$epsg))

x3_footprints <- 
  x3_meta %>% 
  image_footprint(rad = FALSE)

plot(x3_footprints$geometry)

photo_count <- fasterize(sf = x3_footprints, raster = raster(x3_footprints, res = 0.5), fun = "count")
photo_overlap <- 1 - (1 / photo_count)

photo_overlap_threshold <- photo_overlap
photo_overlap_threshold[photo_overlap_threshold[] < 0.95] <- NA

plot(photo_count, col = viridis(100))
plot(photo_overlap_threshold, col = viridis(100))


# Multispectral camera ----------------------------------------------------


# rededge sensor characteristics --------------------------------------------------
# From: https://www.usgs.gov/media/images/micasense-rededge-3-specifications

re_sensor_width_mm <- 4.8 # Fixed for Rededge3; sensor width to focal length proportion is equal to swath width to altitude proportion
re_sensor_height_mm <- 3.6
re_sensor_width_px <- 1280
re_sensor_height_px <- 960
re_focal_length_mm <- 5.5

# Note that this shutter speed is set in the web browser interface to the RedEdge
# camera. A shutter speed of 1 image per second is the fastest that the RedEdge
# camera can write.
re_shutter_speed <- 1

re_h_footprint_planned_agl_m <- (re_sensor_width_mm / re_focal_length_mm) * agl_m
re_v_footprint_planned_agl_m <- (re_sensor_height_mm / re_focal_length_mm) * agl_m

re_h_footprint_actual_agl_m <- (re_sensor_width_mm / re_focal_length_mm) * (agl_m + offset_m)
re_v_footprint_actual_agl_m <- (re_sensor_height_mm / re_focal_length_mm) * (agl_m + offset_m)

re_planned_side_overlap_agl <- 1 - (transect_spacing / re_h_footprint_planned_agl_m)
re_planned_forward_overlap_agl <- 1 - (flight_speed / re_shutter_speed / re_v_footprint_planned_agl_m)

re_actual_side_overlap_agl <- 1 - (transect_spacing / re_h_footprint_actual_agl_m)
re_actual_forward_overlap_agl <- 1 - (flight_speed / re_shutter_speed / re_v_footprint_actual_agl_m)

multispec_meta <- 
  read.csv(multispec_photos_metadata_fname) %>% 
  dplyr::filter(band_name == "blue") %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) %>% 
  dplyr::mutate(h_footprint = (re_sensor_width_mm / re_focal_length_mm) * (agl),
                v_footprint = (re_sensor_height_mm / re_focal_length_mm) * (agl)) %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[, "X"],
                lat = sf::st_coordinates(.)[, "Y"],
                # https://gis.stackexchange.com/questions/190198/how-to-get-appropriate-crs-for-a-position-specified-in-lat-lon-coordinates/190209#190209
                epsg = 32700 - round((45 + lat) / 90) * 100 + round((183 + lon) / 6))

# For now, assume that all the photos are taken in the same UTM zone. If that's not the case, the next processing
# step can be split up by grouping by the epsg column (use base::split then lapply())
multispec_meta <-
  multispec_meta %>% 
  st_transform(unique(.$epsg))

footprints <- 
  multispec_meta %>% 
  image_footprint()

photo_count <- 
  fasterize(sf = footprints, raster = raster(footprints, res = 0.5), fun = "count") %>% 
  terra::rast()

photo_overlap <- 1 - (1 / photo_count)

photo_overlap_threshold <- terra::classify(x = photo_overlap,
                                           rcl = matrix(c(0, 0.95, NA), byrow = TRUE, nrow = 1),
                                           include.lowest = TRUE)


terra::writeRaster(x = photo_count, 
                    filename = multispec_photo_overlap_count_fname,
                    overwrite = TRUE)
terra::writeRaster(x = photo_overlap_threshold, 
                    filename = multispec_photo_overlap_pct_fname,
                    overwrite = TRUE)
