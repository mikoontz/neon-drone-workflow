# calculate veg indices

library(tidyverse)
library(raster)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# directories to be created in this script
L3a_spectral_dir <- file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime)

# files to be read in this script
cropped_ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
cropped_crowns_fname <- file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))

# files to be written in this script
ndvi_fname <- file.path("data", "data_drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif"))

# read in necessary data products
ortho <- raster::brick(cropped_ortho_fname)
ndvi <- (ortho[[4]] - ortho[[3]]) / (ortho[[4]] + ortho[[3]])

if(!dir.exists(L3a_spectral_dir)) {
  dir.create(L3a_spectral_dir, recursive = TRUE)
}

if(!file.exists(ndvi_fname)) {
  raster::writeRaster(x = ndvi, filename = ndvi_fname)
}

crowns <- sf::st_read(cropped_crowns_fname)

plot(ndvi, col = viridis::viridis(100))
plot(crowns %>% st_geometry(), add = TRUE)

raster::plotRGB(ortho * 256)
plot(crowns %>% st_geometry(), add = TRUE, border = "red")
