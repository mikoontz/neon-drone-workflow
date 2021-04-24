# calculate veg indices

library(tidyverse)
library(raster)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read in this script
ndvi_drone_fname <- file.path("data", "drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif"))
ndvi_neon_fname <- file.path("data", "out", "AOP", "ndvi_spectral-resampled.tif")

# cropped_ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))

# read in necessary data products
# ortho <- raster::brick(cropped_ortho_fname)
ndvi_drone <- raster::raster(ndvi_drone_fname)
ndvi_neon <- raster::raster(ndvi_neon_fname)
crowns <- sf::st_read(cropped_crowns_fname)
crowns_neon_proj <- crowns %>% st_transform(st_crs(ndvi_neon))

plot(ndvi_drone, col = viridis::viridis(100))
plot(crowns %>% st_geometry(), add = TRUE)

plot(ndvi_neon, col = viridis::viridis(100))
plot(crowns_neon_proj %>% st_geometry(), add = TRUE)