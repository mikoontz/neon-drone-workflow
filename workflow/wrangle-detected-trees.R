# Get the NEON tree data for NIWO_017
library(dplyr)
library(sf)
library(neonUtilities)

# We will use the geoNEON package to help us get precise geolocations for each tree
# for more info, see https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON
# library(devtools)
# devtools::install_github('NEONScience/NEON-geolocation/geoNEON', dependencies = TRUE)
library(geoNEON)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"


# directories to be created by this script
dir.create("data/raw/TOS", recursive = TRUE, showWarnings = FALSE)

# files to be read by this script
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")

gcp <- sf::st_read(gcp_locations_fname)

local_utm <-
  paste0("32", 
         ifelse(stringr::str_detect(string = unique(gcp$utmZone), pattern = "N"), yes = "6", no = "7"), 
         stringr::str_extract(unique(gcp$utmZone), pattern = "[0-9]+")) %>% 
  as.numeric()

gcp <- sf::st_transform(gcp, local_utm)

# trees are only mapped on the ground within the innermost 20 x 20m plot
interior_plot <-
  gcp %>% 
  dplyr::filter(pointID %in% c(49, 51, 33, 31)) %>% 
  sf::st_geometry() %>%
  sf::st_union() %>% 
  sf::st_cast("POLYGON")

# We want the "Woody Plant Vegetation Structure" data (DP1.10098.001)
# Get it for all years so that we can make sure we have a complete stem map (not all trees are measured every year)
woody_plant_veg_structure <- neonUtilities::loadByProduct(dpID = "DP1.10098.001", 
                                                          site = "NIWO", 
                                                          package = "basic",
                                                          check.size = FALSE)

neon_trees <- 
  woody_plant_veg_structure$vst_mappingandtagging %>% 
  dplyr::filter(plotID == "NIWO_017") %>% 
  geoNEON::getLocTOS(dataProd = "vst_mappingandtagging") %>% 
  dplyr::rename(easting = adjEasting,
                northing = adjNorthing) %>% 
  dplyr::filter(!is.na(easting)) %>% 
  sf::st_as_sf(coords = c("easting", "northing"), crs = local_utm)

drone_trees <- 
  sf::st_read("data/drone/L3a/geometric/niwo_017/2019-10-09/niwo_017_2019-10-09_ttops.gpkg") %>% 
  sf::st_transform(local_utm) %>%
  dplyr::filter(sf::st_intersects(x = ., y = interior_plot, sparse = FALSE))

drone_crowns <-
  sf::st_read("data/drone/L3a/geometric/niwo_017/2019-10-09/niwo_017_2019-10-09_crowns.gpkg") %>% 
  sf::st_transform(local_utm) %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_plot, sparse = FALSE))

idtrees_niwo_017 <-
  idtrees_niwo_017 %>% 
  dplyr::filter(sf::st_intersects(x = ., y = interior_plot, sparse = FALSE))

plot(interior_plot)
plot(st_geometry(drone_crowns), add = TRUE, col = "darkgreen")
plot(st_geometry(neon_trees), add = TRUE, pch = 19)
plot(st_geometry(idtrees_niwo_017), add = TRUE, pch = 19, col = "blue")
plot(st_geometry(drone_trees), add = TRUE, col = "red", pch = 19)

