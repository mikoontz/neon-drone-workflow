# create digital terrain model and canopy height model

library(tidyverse)
library(lidR)
library(raster)
# install.packages("RCSF")

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# directories to be created in this script
L2_geo_dir <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime)

# files to be read in this script
cropped_dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif"))
cropped_dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las"))
cropped_sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las"))

# files to be written in this script
cropped_classified_dense_pc_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_classified-dense-point-cloud_cropped.las"))
cropped_dtm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dtm_cropped.tif"))
cropped_chm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif"))

if(!dir.exists(L2_geo_dir)) {
  dir.create(L2_geo_dir, recursive = TRUE)
}

sparse_point_cloud <- lidR::readLAS(files = cropped_sparse_point_cloud_fname)
dense_point_cloud <- lidR::readLAS(files = cropped_dense_point_cloud_fname)

local_utm <- sf::st_crs(sparse_point_cloud)

# Use the cloth simulation filter by Zhang et al. (2016) [http://www.mdpi.com/2072-4292/8/6/501/htm]
# implemented in the lidR package to classify points in the point cloud as ground or non-ground

# cloth resolution set to be ~5 times the average spacing between points
# when point cloud density is ~30 pts per m^2

classified_dense_point_cloud <- lidR::classify_ground(las = dense_point_cloud, 
                                                      algorithm = csf(sloop_smooth = TRUE, 
                                                                      class_threshold = 0.25, 
                                                                      cloth_resolution = 0.5,  
                                                                      rigidness = 1, 
                                                                      iterations = 500, 
                                                                      time_step = 0.65))


if(!file.exists(cropped_classified_dense_pc_fname)) {
  lidR::writeLAS(las = classified_dense_point_cloud, file = cropped_classified_dense_pc_fname)
}

# Plot the classification of the point cloud for inspection
# plot(classified_dense_point_cloud, color = "Classification")

classified_sparse_point_cloud <- lidR::classify_ground(las = sparse_point_cloud, 
                                                       algorithm = csf(sloop_smooth = TRUE, 
                                                                       class_threshold = 0.25, 
                                                                       cloth_resolution = 0.5,  
                                                                       rigidness = 1, 
                                                                       iterations = 500, 
                                                                       time_step = 0.65))

# Plot the raw point cloud for inspection
plot(sparse_point_cloud)

# Plot the classification of the point cloud for inspection
lidR::plot(classified_sparse_point_cloud, color = "Classification", pal = c("darkgreen", "white"))

# Create a 1m resolution digital terrain model using the classified ground points
# and interpolation between those ground points using the tin() function from lidR which 
# implements a Delaunay triangulation method
dtm <- 
  lidR::grid_terrain(las = classified_sparse_point_cloud,
                          res = 0.5,
                          algorithm = tin()) |>
  terra::rast()

# dtm_4326 <- raster::projectRaster(from = dtm, crs = sp::CRS("+init=epsg:4326"))
dtm_4326 <- terra::project(x = dtm, y = "epsg:4326")

# Write the dtm file to disk
# raster::writeRaster(x = dtm, filename = cropped_dtm_fname, overwrite = TRUE)
terra::writeRaster(x = dtm, filename = cropped_dtm_fname, overwrite = TRUE)

# calculate a canopy height model -----------------------------------------

# dsm <- raster::raster(cropped_dsm_fname)
dsm <- terra::rast(cropped_dsm_fname)
# dsm <- terra::project(x = dsm, y = paste0("epsg:", local_utm), method = "bilinear")
dsm <- terra::project(x = dsm, y = local_utm$wkt, method = "bilinear")

# Using bilinear interpolation to downsample the 1m resolution DTM to have the
# same resolution as the dsm (~5cm, but slightly different for each site)
# dtm_resamp <- raster::resample(x = dtm, y = dsm, method = "bilinear")
dtm_resamp <- terra::resample(x = dtm, y = dsm, method = "bilinear")

# The Canopy Height Model (chm) is the dsm (vegetation + ground) minus the dtm (ground)
# to give just the height of the vegetation.
chm <- dsm - dtm_resamp

plot(dsm, col = viridis::viridis(100))
plot(dtm_resamp, col = viridis::viridis(100))
plot(chm, col = viridis::viridis(100))

# Write the chm file to disk so we can use it later
# Note that all of these outputs generated using R get written to the same place, regardless of whether the
# output is derived from merged X3+RedEdge imagery versus just being derived from RedEdge imagery
# raster::writeRaster(x = chm_smooth, filename = cropped_chm_fname, overwrite = TRUE)
terra::writeRaster(x = chm, filename = cropped_chm_fname, overwrite = TRUE)

