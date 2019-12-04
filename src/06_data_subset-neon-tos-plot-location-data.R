# Purpose: subset the NEON TOS plot location data into just the points from NIWO_017

library(tidyverse)
library(sf)

tos_points <- st_read("data/data_raw/All_NEON_TOS_Plots_V5/All_Neon_TOS_Points_V5.shp")

niwo_017 <- 
  tos_points %>% 
  dplyr::filter(plotID == "NIWO_017") %>% 
  dplyr::filter(subtype == "basePlot") %>% 
  dplyr::filter(crdSource == "Geo 7X (H-Star)")

plot(st_geometry(niwo_017))

dir.create("data/data_output", recursive = TRUE)

st_write(niwo_017, "data/data_output/niwo_017_gcp-locations.geoJSON")
