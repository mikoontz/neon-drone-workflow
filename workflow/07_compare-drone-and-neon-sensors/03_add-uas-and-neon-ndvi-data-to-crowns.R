# Extract NDVI from within each tree crown segment

library(dplyr)
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# files to be read
cropped_crowns_fname <- file.path("data", "drone", "L3a", "geometric", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_crowns_cropped.gpkg"))
gcp_locations_fname <- paste0("data/out/", site_name, "_gcp-locations.gpkg")
ndvi_drone_fname <- file.path("data", "drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif"))
ndvi_neon_fname <- file.path("data", "out", "AOP", "ndvi_spectral-resampled.tif")

# files to write
augmented_crowns_fname <- file.path("data", "drone", "L3b", paste0(site_name, "_", flight_datetime, "_", "crowns-with-reflectance_cropped.gpkg"))
fig_fname <- file.path("figs", "uas-vs-neon-ndvi-per-crown.png")

gcp <- sf::st_read(gcp_locations_fname)
crowns_raw <- sf::st_read(cropped_crowns_fname) %>% dplyr::rename(geometry = geom)
drone_ndvi <- terra::rast(ndvi_drone_fname)
neon_resampled_ndvi <- terra::rast(ndvi_neon_fname)

# Extract the NDVI values from the NEON AOP raster (the one that has been resampled)
# from within each tree crown
crowns_with_neon_ndvi <- 
  exactextractr::exact_extract(x = neon_resampled_ndvi, y = crowns_raw,
                               fun = c("mean", "stdev"),
                               append_cols = "treeID") %>% 
  dplyr::rename(ndvi_mean_neon = mean,
                ndvi_stdev_neon = stdev)

# Extract the NDVI values from the UAS-derived raster (the one from the SfM
# processing of the MicaSense RedEdge 3 imagery) from within each tree crown
crowns_with_uas_ndvi <- 
  exactextractr::exact_extract(x = drone_ndvi, y = crowns_raw, 
                               fun = c("mean", "stdev"),
                               append_cols = "treeID") %>% 
  dplyr::rename(ndvi_mean_uas = mean, ndvi_stdev_uas = stdev)

# join the two dataframes together along with all the original attributes of the
# tree crowns, then convert back to a spatial object. Now we have each crown along
# with some spectral data from two different sensors
crowns <- 
  dplyr::left_join(x = crowns_with_neon_ndvi, y = crowns_with_uas_ndvi, by = "treeID") %>% 
  dplyr::left_join(crowns_raw, by = "treeID") %>% 
  sf::st_as_sf()

sf::st_write(obj = crowns, dsn = augmented_crowns_fname)

crowns <-
  crowns %>% 
  dplyr::mutate(diff_ndvi = ndvi_mean_uas - ndvi_mean_neon)

ndvi_two_source_gg <-
  ggplot(crowns, aes(x = ndvi_mean_uas, y = ndvi_mean_neon)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Mean UAS-derived NDVI",
       y = "Mean NEON-derived NDVI")

ggplot(crowns, aes(x = ndvi_mean_uas, y = diff_ndvi)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Mean UAS-derived NDVI",
       y = "Mean NEON-derived NDVI")

ggsave(plot = ndvi_two_source_gg, filename = fig_fname, width = 180, height = 180, units = "mm", dpi = 300)
