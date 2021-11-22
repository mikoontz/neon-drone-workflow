library(data.table)
library(dplyr)
library(sf)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# Get the bounds of the 40 x 40 m plot itself (the polygon)
all_neon_tos_polys <- read_sf("data/raw/All_NEON_TOS_Plots_V5/All_Neon_TOS_Polygons_V5.shp")

my_site_poly <-
  all_neon_tos_polys %>% 
  filter(plotID == toupper(site_name)) %>% 
  filter(crdSource == "Geo 7X (H-Star)") %>% 
  filter(subtype == "basePlot")

# The geometry is in EPSG4326, but we may want it in the local UTM coordinate reference system
my_site_local_crs <-
  paste0("32", 
         ifelse(stringr::str_detect(string = unique(my_site_poly$utmZone), pattern = "N"), yes = "6", no = "7"), 
         stringr::str_extract(unique(my_site_poly$utmZone), pattern = "[0-9]+")) %>% 
  as.numeric()

my_site_poly <- sf::st_transform(my_site_poly, my_site_local_crs)

# Ben Weinstein, Sergio Marconi, Alina Zare, Stephanie Bohlman, Sarah Graves, Aditya Singh, & Ethan White. (2020). NEON Tree Crowns Dataset (0.0.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3765872
# https://doi.org/10.5281/zenodo.3765871
idtrees <- fread(file.path("data", "raw", "idtrees", "NIWO_2019.csv"))

# From the .h5 file we downloaded to get the hyperspectral data
niwo_017_geo_index <- "451000_4432000"
idtrees <- idtrees[geo_index == niwo_017_geo_index, ]

idtrees[, easting := (left + right) / 2]
idtrees[, northing := (top + bottom) / 2]
idtrees <- sf::st_as_sf(idtrees, coords = c("easting", "northing"), crs = my_site_local_crs, remove = FALSE)

idtrees_niwo_017 <-
  idtrees %>% 
  dplyr::filter(sf::st_intersects(x = ., y = my_site_poly, sparse = FALSE))

plot(st_geometry(my_site_poly))
plot(st_geometry(idtrees_niwo_017), add = TRUE)
