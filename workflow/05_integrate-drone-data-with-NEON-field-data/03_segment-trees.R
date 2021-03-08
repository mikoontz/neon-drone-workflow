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

# files to be read in this script
cropped_ttops_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg"))
cropped_chm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif"))

# files to be written in this script
cropped_crowns_fname <- file.path("data", "data_drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))

# read necessary data products
ttops <- sf::st_read(cropped_ttops_fname)
chm <- raster::raster(cropped_chm_fname)

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

if(!file.exists(cropped_crowns_fname)) {
  sf::st_write(obj = crowns, dsn = cropped_crowns_fname)
}
