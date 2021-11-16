# Purpose: crop the outputs from Metashape to more closely adhere to the ground 
# data (and to make .tif and .las files smaller and easier to work with)

library(tidyverse)
library(sf)
library(lidR)
library(raster)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

mission_footprint_dir <- file.path("data", "drone", "L0", "mission-footprint", site_name, flight_datetime)

# files to be read in this script
constrained_site_bounds_fname <- file.path(mission_footprint_dir, paste(site_name, flight_datetime, "constrained-site-bounds.gpkg", sep = "_"))

# GCP data used for getting local coordinate reference system, which can be used to transform EPSG:4326 coordinate
# reference system into something more amenable to buffering by distances in meters (rather than degrees)
gcp_fname <- file.path("data", "out", paste(site_name, "gcp-locations.gpkg", sep = "_"))

# Used for cropping to slightly wider area if desired
# site_bounds_fname <- file.path(mission_footprint_dir, paste(site_name, flight_datetime, "site-bounds.gpkg", sep = "_"))

ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho.tif"))

# if RGB camera was also used in addition to Micasense Rededge sensor
# ortho_rgb_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_rgb.tif"))


dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm.tif"))

# if RGB camera was also used in addition to Micasense Rededge sensor and you want
# the DSM derived from RGB imagery
dsm_rgb_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_rgb.tif"))

dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud.las"))
sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud.las"))

# files to be written using this script
cropped_ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))

#  if RGB camera was also used in addition to Micasense Rededge sensor
# cropped_rgb_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_rgb_cropped.tif"))

cropped_dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif"))

#  if RGB camera was also used in addition to Micasense Rededge sensor
cropped_dsm_rgb_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_rgb_cropped.tif"))

cropped_dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las"))
cropped_sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las"))

# GCP file
gcp <- st_read(gcp_fname)
local_crs <- unique(gcp$local_crs)

# site boundary just overlapping the field data
constrained_site_bounds <- st_read(constrained_site_bounds_fname)

# larger boundary based on flight path, rather than field plot
# needs to be buffered in a little bit to avoid edge effects
# buffering in 35 meters (~2 flight transects) following Koontz et al. 2021
# could likely buffer in less, because a cross hatch flight pattern was used, which
# increases photo overlap, but we'll be conservative
site_bounds <- 
  st_read(site_bounds_fname) %>% 
  st_transform(local_crs) %>% 
  st_buffer(-35) %>% 
  st_transform(4326)

# Orthomosaic outputs from Metashape derived using Micasense Rededge imagery need to be divided by 32768 to make 1 correspond to 100% reflectance (.tif files are 16 bit images and half that value [2^16 / 2] corresponds
# to 100% reflectance. See https://support.micasense.com/hc/en-us/articles/215460518-What-are-the-units-of-the-Atlas-GeoTIFF-output-
# August 16, 2019 14:08
# What are the units of the Atlas GeoTIFF output?
# GeoTIFF: This is a 5-layer, 16-bit ortho-rectified GeoTIFF file. A GIS application is needed to open this type of file. The pixel values are proportional to % reflectance, with a pixel value of 32768 being equal to 100% reflectance (65535 is equal to 200% reflectance). In order to extract the reflectance values you will need to divide by 32768.

# Then, we multiply through by 256 to put the 0 to 1 values back on a standard 256 color ramp
ortho <- raster::brick(ortho_fname) / 32768
cropped_ortho <- raster::crop(x = ortho, y = constrained_site_bounds)

# Digital surface model
dsm <- raster::raster(dsm_fname)
cropped_dsm <- raster::crop(x = dsm, y = constrained_site_bounds)

# RGB orthomosaic (if RGB camera was also used in addition to Micasense Rededge sensor)
# rgb <- raster::brick(ortho_rgb_fname)
# cropped_rgb <- raster::crop(x = rgb, y = site_bounds)

# RGB DSM (if RGB camera was also used and you want the DSM derived from RGB photos)
dsm_rgb <- raster::raster(dsm_rgb_fname)
cropped_dsm_rgb <- raster::crop(x = dsm_rgb, y = site_bounds)

# Point cloud is exported from Metashape in a local coordinate reference system (CRS) in order to properly 
# meet some .las file standards (like the scaling for the X and Y dimension)
# Because of this, we need to crop the point cloud using a site boundary polygon that is in the same CRS

dense_point_cloud_catalog <- lidR::catalog(folder = dense_point_cloud_fname)
cropped_dense_point_cloud <- lidR::lasclip(dense_point_cloud_catalog, sf::st_transform(constrained_site_bounds, dense_point_cloud_catalog@proj4string))

sparse_point_cloud_catalog <- lidR::catalog(folder = sparse_point_cloud_fname)
cropped_sparse_point_cloud <- lidR::lasclip(sparse_point_cloud_catalog, sf::st_transform(constrained_site_bounds, sparse_point_cloud_catalog@proj4string))

# Write the cropped geospatial data to files for rapid recall and use in segmentation algorithm validation against ground data
# ortho
writeRaster(x = cropped_ortho, filename = cropped_ortho_fname, overwrite = TRUE)

# dsm
writeRaster(x = cropped_dsm, filename = cropped_dsm_fname, overwrite = TRUE)

# RGB orthomosaic
# writeRaster(x = cropped_rgb, filename = cropped_rgb_fname, overwrite = TRUE)

# RGB DSM
writeRaster(x = cropped_dsm_rgb, filename = cropped_dsm_rgb_fname, overwrite = TRUE)

# dense point cloud
writeLAS(las = cropped_dense_point_cloud, file = cropped_dense_point_cloud_fname)

# sparse point cloud
writeLAS(las = cropped_sparse_point_cloud, file = cropped_sparse_point_cloud_fname)
