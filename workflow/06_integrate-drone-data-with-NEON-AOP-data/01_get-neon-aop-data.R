library(neonUtilities)
library(sf)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# directories to be created by this script
dir.create("data/raw/AOP", recursive = TRUE, showWarnings = FALSE)

# files to be read by this script
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")

gcp <- sf::read_sf(gcp_locations_fname)

local_utm <-
  paste0("32", 
         ifelse(stringr::str_detect(string = unique(gcp$utmZone), pattern = "N"), yes = "6", no = "7"), 
         stringr::str_extract(unique(gcp$utmZone), pattern = "[0-9]+")) %>% 
  as.numeric()

# We'll use the centroid of the vegetation plot that we flew over to spatially
# restrict the amount of NEON spectrometer data that we download
# Note this could be a point location from anywhere within the footprint of
# the AOP

site_centroid <- 
  gcp %>% 
  sf::st_transform(local_utm) %>% 
  dplyr::summarize() %>% 
  sf::st_centroid() %>% 
  sf::st_coordinates()

# NIWO 2019 Imaging spectroscopy orthomosaic (DP3.30006.001)
# NEON (National Ecological Observatory Network). Spectrometer orthorectified surface directional reflectance - mosaic, RELEASE-2021 (DP3.30006.001). https://doi.org/10.48443/qeae-3x15. Dataset accessed from https://data.neonscience.org on February 3, 2021
# https://data.neonscience.org/data-products/DP3.30006.001

neonUtilities::byTileAOP(dpID = "DP3.30006.001", 
                         site = "NIWO", 
                         year = 2019, 
                         easting = site_centroid[1, "X"], 
                         northing = site_centroid[1, "Y"],
                         buffer = 20, # per help docs, "Defaults to 0. If easting and northing coordinates are the centroids of NEON TOS plots, use buffer=20"
                         savepath = "data/raw/AOP")

## Note that this download mechanism is sometimes unreliable and it might
## require going to the NEON website to use the GUI for downloads
## (https://data.neonscience.org/data-products/DP3.30006.001)