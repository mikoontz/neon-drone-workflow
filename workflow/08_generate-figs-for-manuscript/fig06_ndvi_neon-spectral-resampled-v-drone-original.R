library(dplyr)
library(terra)

## Read input raster data
# figure source data created in the workflow/06_NEON-AOP-data-integration-with-drone/02_spectrally-resample-aop-to-rededge.R
ndvi_drone_fname <- file.path("data", "drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif"))
ndvi_neon_fname <- file.path("data", "out", "AOP", "ndvi_spectral-resampled.tif")

figure_fname <- file.path("figs", "fig06_ndvi_neon-spectral-resampled-v-drone-original.pdf")

drone_ndvi <- terra::rast(ndvi_drone_fname)
neon_resampled_ndvi <- terra::rast(ndvi_neon_fname)

neon_ndvi_tmap <-
  tmap::tm_shape(neon_resampled_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_grid(alpha = 0.2, n.x = 4, n.y = 4, col = "black", labels.col = "black") +
  tmap::tm_xlab(text = "                                                                                                    Easting (m)") +
  tmap::tm_ylab(text = "Northing (m)", space = 0.1) +
  tmap::tm_layout(legend.show = FALSE, asp = 1, main.title = "a", main.title.size = 0.9)

neon_ndvi_tmap

drone_ndvi_tmap <-
  tmap::tm_shape(drone_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_grid(alpha = 0.2, labels.show = c(TRUE, TRUE), n.x = 4, n.y = 4, col = "black", labels.col = "black") +
  tmap::tm_xlab(text = "Easting (m)                                                         ") +
  tmap::tm_ylab(text = "") +
  tmap::tm_layout(legend.show = FALSE, asp = 1, main.title = "b", main.title.size = 0.9)

drone_ndvi_tmap

legend_ndvi_tmap <-
  tmap::tm_shape(drone_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_layout(legend.only = TRUE, legend.position = c("right", "center"))

two_panel <- 
  tmap::tmap_arrange(neon_ndvi_tmap, drone_ndvi_tmap, legend_ndvi_tmap, 
                     nrow = 1, ncol = 3, widths = c(0.45, 0.45, 0.1), 
                     outer.margins = c(0.01, 0.01, 0, 0))

tmap::tmap_save(tm = two_panel, filename = figure_fname, width = 180, height = 70, units = "mm")
