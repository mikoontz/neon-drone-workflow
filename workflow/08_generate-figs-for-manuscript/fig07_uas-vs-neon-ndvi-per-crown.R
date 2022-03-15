library(dplyr)
library(ggplot2)
library(sf)

augmented_crowns_fname <- file.path("data", "drone", "L3b", paste0(site_name, "_", flight_datetime, "_", "crowns-with-reflectance_cropped.gpkg"))

# this figure source data created in the workflow/07_compare-drone-andneon-sensors/03_add-uas-and-neon-ndvi-data-to-crowns.R script
crowns <- sf::st_read(augmented_crowns_fname)

ndvi_two_source_gg <-
  ggplot(crowns, aes(x = ndvi_mean_uas, y = ndvi_mean_neon)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_smooth() +
  theme_bw() +
  labs(x = "Mean UAS-derived NDVI",
       y = "Mean NEON-derived NDVI")

ggsave(plot = ndvi_two_source_gg, filename = file.path("figs", "uas-vs-neon-ndvi-per-crown.png"), width = 180, height = 180, units = "mm", dpi = 300)
