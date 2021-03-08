# Purpose: crop the outputs from Metashape to more closely adhere to the ground 
# data (and to make .tif and .las files smaller and easier to work with)

library(tidyverse)
library(sf)
library(lidR)
library(raster)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

mission_footprint_dir <- paste0("data/drone/L0/mission-footprint/", site_name, "/", flight_datetime)

# files to be read in this script
constrained_site_bounds_fname <- paste0(mission_footprint_dir, "/", site_name, "_", flight_datetime, "_constrained-site-bounds.gpkg")
ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho.tif"))
dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm.tif"))
dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud.las"))
sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud.las"))

# files to be written using this script
cropped_ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
cropped_dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif"))
cropped_dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las"))
cropped_sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las"))

# site boundary just overlapping the field data
constrained_site_bounds <- st_read(constrained_site_bounds_fname)

# Orthomosaic outputs from Metashape derived using Micasense Rededge imagery need to be divided by 32768 to make 1 correspond to 100% reflectance
# Then, we multiply through by 256 to put the 0 to 1 values back on a standard 256 color ramp
ortho <- raster::brick(ortho_fname) / 32768
cropped_ortho <- raster::crop(x = ortho, y = constrained_site_bounds)

dsm <- raster::raster(dsm_fname)
cropped_dsm <- raster::crop(x = dsm, y = constrained_site_bounds)

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

# dense point cloud
writeLAS(las = cropped_dense_point_cloud, file = cropped_dense_point_cloud_fname)

# sparse point cloud
writeLAS(las = cropped_sparse_point_cloud, file = cropped_sparse_point_cloud_fname)
