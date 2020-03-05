# segment trees


library(tidyverse)
library(sf)
# devtools::install_github("cran/lidR@fd23e40058363f2e1ea29ade94fd06504422ed1f")
# devtools::install_github("Jean-Romain/lidRplugins@a06022664534778aa7c10e4681f64f61c89aad24")
library(lidR)
library(stars)
library(ForestTools)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

ttops <- sf::st_read(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg")))

chm <- raster::raster(file.path("data", "data_drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif")))

non_spatial_ttops <-
  ttops %>%
  dplyr::mutate(x = st_coordinates(.)[, 1],
                y = st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry()

crowns <-
  ttops %>%
  ForestTools::mcws(CHM = chm, minHeight = 1.5, format = "raster") %>%
  setNames(nm = "treeID") %>%
  st_as_stars() %>%
  st_as_sf(merge = TRUE) %>% 
  st_set_crs(4326) %>%
  dplyr::left_join(non_spatial_ttops, by = "treeID")

plot(crowns$geometry)

if(!dir.exists(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime))) {
  dir.create(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime), recursive = TRUE)
}

if(!file.exists(file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg")))) {
  sf::st_write(obj = crowns, dsn = file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg")))
}
