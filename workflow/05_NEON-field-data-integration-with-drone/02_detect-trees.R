# Detect trees in the scene

library(tidyverse)
library(sf)
# devtools::install_github("cran/lidR@fd23e40058363f2e1ea29ade94fd06504422ed1f")
# devtools::install_github("Jean-Romain/lidRplugins@a06022664534778aa7c10e4681f64f61c89aad24")
library(lidR)
library(terra)
library(raster)
library(ForestTools)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# directories to be created
L3a_geo_dir <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime)

# files to be read in this script
cropped_chm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif"))
cropped_dtm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dtm_cropped.tif"))
cropped_classified_dense_pc_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_classified-dense-point-cloud_cropped.las"))

# files to be written in this script
cropped_ttops_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg"))

# read in the necessary data products
chm <- terra::rast(cropped_chm_fname)
dtm <- terra::rast(cropped_dtm_fname)
pc <- lidR::readLAS(cropped_classified_dense_pc_fname)

normalized_pc <- lidR::normalize_height(las = pc, algorithm = raster::raster(dtm))
plot(normalized_pc)

# From Young et al., 2021
# VWF algorithm with a = 0, b = 0.04, smooth = 2
a <- 0
b <- 0.04
c <- 0

# Smooth the chm 
chm_res <- mean(terra::res(chm))
pixels_smooth_2 <- round(((1/chm_res)-1)/2)*2 + 1

weights <- matrix(1, nrow = pixels_smooth_2, ncol = pixels_smooth_2)
chm_smooth <- terra::focal(x = chm, weights, fun = mean)
chm_smooth <- terra::classify(chm_smooth, rcl = matrix(data = c(-Inf, 0, 0), byrow = TRUE, nrow = 1))

# use ForestTools vwf() function to detect trees
lin <- function(x){x^2*c + x*b + a} # window filter function to use in next step
ttops <- ForestTools::vwf(CHM = raster::raster(chm), 
                          winFun = lin, 
                          minHeight = 5, 
                          maxWinDiameter = 199) %>% 
  sf::st_as_sf()

# ttops <- lidR::find_trees(las = normalized_pc, algorithm = lmf(ws = 1.5))

plot(chm, col = viridis::viridis(100))
plot(ttops %>% st_transform(4326) %>% st_geometry(), add = TRUE, pch = 19, col = "red")  
  
if(!dir.exists(L3a_geo_dir)) {
  dir.create(L3a_geo_dir, recursive = TRUE)
}

if(!file.exists(cropped_ttops_fname)) {
  
  sf::st_write(obj = ttops, dsn = cropped_ttops_fname, delete_dsn = TRUE)
}
