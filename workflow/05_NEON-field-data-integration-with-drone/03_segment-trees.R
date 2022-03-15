# segment trees

library(tidyverse)
library(sf)
# devtools::install_github("cran/lidR@fd23e40058363f2e1ea29ade94fd06504422ed1f")
# devtools::install_github("Jean-Romain/lidRplugins@a06022664534778aa7c10e4681f64f61c89aad24")
library(lidR)
library(stars)
library(ForestTools)
library(terra)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read in this script
cropped_ttops_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ttops_cropped.gpkg"))
cropped_chm_fname <- file.path("data", "drone", "L2", "geometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_chm_cropped.tif"))

# files to be written in this script
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))

# read necessary data products
ttops <- sf::st_read(cropped_ttops_fname)
# the {terra} package will be replacing {raster}, so here's how this is done in {terra}
chm <- terra::rast(cropped_chm_fname)

# smooth the CHM the same way we did with the tree detection step
chm_res <- mean(terra::res(chm))
pixels_smooth_2 <- round(((1/chm_res)-1)/2)*2 + 1

weights <- matrix(1, nrow = pixels_smooth_2, ncol = pixels_smooth_2)
chm_smooth <- terra::focal(x = chm, weights, fun = mean)
chm_smooth <- terra::classify(chm_smooth, rcl = matrix(data = c(-Inf, 0, 0), byrow = TRUE, nrow = 1))

non_spatial_ttops <-
  ttops %>%
  dplyr::mutate(x = st_coordinates(.)[, 1],
                y = st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry()

crowns <-
  ttops %>%
  ForestTools::mcws(CHM = raster::raster(chm), minHeight = 5, format = "raster") %>%
  setNames(nm = "treeID") %>%
  st_as_stars() %>%
  st_as_sf(merge = TRUE) %>%
  dplyr::left_join(non_spatial_ttops, by = "treeID") %>% 
  sf::st_make_valid()

for (i in 1:nrow(crowns)) {
  this_point <- sf::st_drop_geometry(crowns[i, ]) %>% sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crowns))
  crowns[i, "point_in_crown"] <- as.vector(sf::st_intersects(x = this_point, y = crowns[i, ], sparse = FALSE))
}

crowns <-
  crowns %>% 
  dplyr::filter(point_in_crown) %>% 
  dplyr::select(-point_in_crown)

if(!file.exists(cropped_crowns_fname)) {
  sf::st_write(obj = crowns, dsn = cropped_crowns_fname, delete_dsn = TRUE)
}
