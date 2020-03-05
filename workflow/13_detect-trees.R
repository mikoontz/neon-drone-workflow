# Detect trees in the scene

library(tidyverse)
library(sf)
# devtools::install_github("cran/lidR@fd23e40058363f2e1ea29ade94fd06504422ed1f")
# devtools::install_github("Jean-Romain/lidRplugins@a06022664534778aa7c10e4681f64f61c89aad24")
library(lidR)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

chm <- raster::raster(file.path("data", "data_drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif")))
dtm <- raster::raster(file.path("data", "data_drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dtm_cropped.tif")))

pc <- lidR::readLAS(file.path("data", "data_drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_classified-dense-point-cloud_cropped.las")))

normalized_pc <- lidR::lasnormalize(las = pc, algorithm = dtm)

ttops <- lidR::tree_detection(las = normalized_pc, algorithm = lmf(ws = 1.5))
ttops <- sf::st_as_sf(ttops)  

plot(chm, col = viridis::viridis(100))
plot(ttops %>% st_transform(4326) %>% st_geometry(), add = TRUE, pch = 19, col = "red")  
  
if(!dir.exists(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime))) {
  dir.create(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime), recursive = TRUE)
}

if(!file.exists(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg")))) {
  
  sf::st_write(obj = ttops %>% st_transform(4326), dsn = file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg")))
}
