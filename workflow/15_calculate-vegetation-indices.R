# calculate veg indices

library(tidyverse)
library(raster)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

ortho <- raster::brick( file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif")))

ndvi <- (ortho[[4]] - ortho[[3]]) / (ortho[[4]] + ortho[[3]])

plot(ndvi, col = viridis::viridis(100))

if(!dir.exists(file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime))) {
  
  dir.create(file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime), recursive = TRUE)
}

if(!file.exists(file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif")))) {
  raster::writeRaster(x = ndvi, filename = file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif")))
}
