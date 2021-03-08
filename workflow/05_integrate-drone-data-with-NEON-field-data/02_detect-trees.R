# Detect trees in the scene

library(tidyverse)
library(sf)
# devtools::install_github("cran/lidR@fd23e40058363f2e1ea29ade94fd06504422ed1f")
# devtools::install_github("Jean-Romain/lidRplugins@a06022664534778aa7c10e4681f64f61c89aad24")
library(lidR)

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
chm <- raster::raster(cropped_chm_fname)
dtm <- raster::raster(cropped_dtm_fname)
pc <- lidR::readLAS(cropped_classified_dense_pc_fname)

normalized_pc <- lidR::lasnormalize(las = pc, algorithm = dtm)

ttops <- lidR::tree_detection(las = normalized_pc, algorithm = lmf(ws = 1.5))
ttops <- sf::st_as_sf(ttops)  

plot(chm, col = viridis::viridis(100))
plot(ttops %>% st_transform(4326) %>% st_geometry(), add = TRUE, pch = 19, col = "red")  
  
if(!dir.exists(L3a_geo_dir)) {
  dir.create(L3a_geo_dir, recursive = TRUE)
}

if(!file.exists(cropped_ttops_fname)) {
  
  sf::st_write(obj = ttops %>% st_transform(4326), dsn = cropped_ttops_fname)
}
