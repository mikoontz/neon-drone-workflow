# devtools::install_github('earthlab/neonhs')
library(neonhs)
library(dplyr)
library(hdf5r)
library(purrr)
library(hsdar)
library(ggplot2)
library(sf)
library(tidyr)
library(viridis)
# note some packages are still using data types from the {raster} package
# so we include it in this script, but try to convert to {terra} data
# types when possible (so we include that package too)
library(raster)
library(terra)
library(tmap)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# create necessary directories
dir.create("data/out/AOP", recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("data", "drone", "L3a", "spectral", site_name, flight_datetime), 
           showWarnings = FALSE)

# files to be read using this script
mission_footprint_fname <- "data/drone/L0/mission-footprint/niwo_017/2019-10-09/niwo_017_2019-10-09_constrained-site-bounds.gpkg"
cropped_ortho_fname <- file.path("data", "drone", "L2", "radiometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
micasense_rededge3_characteristics_fname <-  "data/out/micasense-rededge3_sensor-characteristics.csv"

# files to be written using this script
neon_aop_spectral_crop <- file.path("data", "out", "AOP", "neon-aop-spectral-crop-for-micasense-rededge.tif")
ndvi_drone_fname <- file.path("data", "drone", "L3a", "spectral", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ndvi.tif"))
ndvi_neon_fname <- file.path("data", "out", "AOP", "ndvi_spectral-resampled.tif")

if(!file.exists(ndvi_drone_fname) | !file.exists(ndvi_neon_fname)) {
  
  # Get the mission footprint and transform it to the same coordinate reference 
  # system as the NEON AOP data
  # This enables us to further spatially constrain the NEON AOP data that we 
  # want to read into memory. We can also read the whole NEON AOP tile, if we
  # wish, by excluding the `crop = mission_footprint` piece in the call to 
  # neonhs::hs_read()
  mission_footprint <- 
    sf::st_read(mission_footprint_fname)
  
  # Get the Micasense Rededge3 relative spectral response data into SpecLib
  # format so that we can readily use the spectralResampling() function from
  # the {hsdar} package
  # This also tells us the range of wavelengths we can use from the NEON AOP
  # spectrometer data
  micasense_rededge3_speclib_matrix <- read.csv("data/out/micasense-rededge3_speclib-matrix.csv")
  rededge_speclib <- speclib(spectra = as.matrix(micasense_rededge3_speclib_matrix[, -1]),
                             wavelength = micasense_rededge3_speclib_matrix$wavelength_nm)
  
  micasense_wavelength_range <- range(hsdar::wavelength(rededge_speclib))
  
  # Also get the basic sensor characteristics for easier matching to the finished
  # product
  micasense_rededge3_characteristics <- 
    read.csv(micasense_rededge3_characteristics_fname) %>% 
    dplyr::arrange(channel)
  
  # Get NEON AOP spectrometer file(s) that were downloaded in previous script
  fname_h5 <- list.files("data/raw/AOP", recursive = TRUE, full.names = TRUE, pattern = ".h5$")
  file_h5 <- lapply(fname_h5, FUN = hdf5r::H5File$new, mode = "r+")
  
  # This script is currently set up to show how to work with a single one of the
  # NEON AOP spectrometer files, so we'll just use the first list item representing
  # the first file read by the line above using the hdf5r::H5File$new call above
  file_h5 <- file_h5[[1]]
  
  # The NEON AOP data themselves are buried in the .h5 files and we can access 
  # them by navigating to the right part of the file and then using the $read() 
  # functionality from the {hdf5r} package
  
  # Get some details from the NEON AOP data
  site <- file_h5$names
  crs_epsg <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/EPSG Code"]]$read()
  
  # Get Gaussian wave parameters of NEON AOP reflectance data
  # including the band centers and the full width half maximum values for each
  # band. These data are used to construct the Gaussian curve that represents
  # the spectral response at each narrow band of the NEON AOP instrument
  # We filter to just the wavelengths that are valid for the Micasense Rededge3
  # sensor
  neon_hs_params <- 
    data.frame(band_center = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/Wavelength"]]$read(),
               fwhm = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/FWHM"]]$read()) %>% 
    mutate(channel = 1:nrow(.)) %>% 
    dplyr::select(channel, everything()) %>% 
    dplyr::filter(band_center >= micasense_wavelength_range[1] & band_center <= micasense_wavelength_range[2])
  
  # The {neonhs} package lets us extract the reflectance data from the AOP
  # spectrometer product in a more intuitive format-- a raster brick from the 
  # {raster} package. The rows are the northing dimension, the columns are the 
  # easting dimension, and the z-dimension (i.e., layers) are the spectral 
  # channels (the number of layers corresponds to the number of bands requested 
  # using the bands = argument. Rather than getting all 426 bands, which can take
  # up a lot of time/memory, we focus just on the bands that will end up being
  # usable because they overlap with the usable bands on the Micasense Rededge3
  # sensor.
  hs <- neonhs::hs_read(filename = fname_h5[1], 
                        bands = neon_hs_params$channel,
                        crop = sf::st_transform(mission_footprint, crs = as.numeric(crs_epsg)))
  
  # In order to do spectral resampling, we have to convert the hyperspectral data
  # from NEON into the SpecLib format from the {hsdar} package. We can do that
  # using a raster brick, but only if we write the raster to disk first.
  raster::writeRaster(x = hs, filename = neon_aop_spectral_crop, overwrite = TRUE)
  
  # Now we can read back in the NEON AOP spectrometer data from the .tif file
  # and pass in the important parameters for defining the shape of the spectral
  # response for each band
  neon_speclib <- hsdar::speclib(spectra = neon_aop_spectral_crop, 
                                 wavelength = neon_hs_params$band_center,
                                 fwhm = neon_hs_params$fwhm)
  
  
  # Now that we have both the NEON AOP spectrometer (i.e., hyperspectral) data
  # in a SpecLib format, and the Micasense Rededge3 relative spectral response
  # data in a SpecLib format, it is straightforward to use the {hsdar} package
  # to spectrally resample the NEON AOP data to "look" like what might be sensed
  # from the Micasense Rededge3 sensor
  neon_resampled_to_rededge <- 
    spectralResampling(x = neon_speclib, 
                       response_function = rededge_speclib)
  
  # The result of the spectral resampling is another SpecLib, but we can retrieve
  # the raster version of those results
  neon_resampled_r <- 
    neon_resampled_to_rededge@spectra@spectra_ra %>% 
    terra::rast() %>% 
    setNames(hsdar::idSpeclib(rededge_speclib))
  
  # Calculate NDVI from both products
  neon_resampled_ndvi <-
    (neon_resampled_r[["nir"]] - neon_resampled_r[["red"]]) / (neon_resampled_r[["nir"]] + neon_resampled_r[["red"]])
  
  # read in necessary data products from drone-mounted micasense Rededge3
  # processed data
  # We want the drone ortho and the resampled NEON AOP NDVI rasters to match
  # up in extent, so we project the drone ortho to the right coordinate
  # reference system first, get the resolution in meters, create a template
  # raster with the extent of the neon_resampled_ndvi raster, coerce the 
  # resolution of that raster to be the same as that of the drone ortho (doing
  # so drops the values of the template, making it truly just a template with
  # our desired resolution, extent, and CRS for the drone ortho), then project
  # the drone ortho once more to the newly created template in order to match
  # the extents properly
  ortho <- 
    terra::rast(cropped_ortho_fname) %>% 
    setNames(micasense_rededge3_characteristics$band) %>% 
    terra::project(y = sf::st_crs(as.numeric(crs_epsg))$wkt, 
                   method = "bilinear")
  
  ortho_template <- neon_resampled_ndvi
  terra::res(ortho_template) <- terra::res(ortho)
  
  ortho <- terra::project(x = ortho, y = ortho_template, method = "bilinear")
  
  drone_ndvi <- 
    (ortho[["nir"]] - ortho[["red"]]) / (ortho[["nir"]] + ortho[["red"]])
  
  names(drone_ndvi) <- "ndvi"
  
  # Write NDVI products to disk
  terra::writeRaster(x = drone_ndvi, filename = ndvi_drone_fname, overwrite = TRUE)
  terra::writeRaster(x = neon_resampled_ndvi, filename = ndvi_neon_fname, overwrite = TRUE)
}

drone_ndvi <- terra::rast(ndvi_drone_fname)
neon_resampled_ndvi <- 
  terra::rast(ndvi_neon_fname) %>% 
  terra::resample(y = drone_ndvi, method = "near")

ndvi <- c(drone_ndvi, neon_resampled_ndvi)

two_panel <-
  tmap::tm_shape(ndvi[[1]]) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA) +
  tmap::tm_grid(alpha = 0.2, ticks = TRUE) +
  tmap::tm_xlab(text = "Easting (m)") +
  tmap::tm_ylab(text = "Northing (m)") +
  tmap::tm_layout(panel.labels = c("Drone-derived", "NEON AOP-derived"), 
                  panel.label.bg.color = "white",
                  legend.outside = TRUE,
                  legend.outside.position = 'right')

two_panel

tmap::tmap_save(tm = two_panel, filename = file.path("figs", "ndvi_neon-spectral-resampled-v-drone-original.png"), dpi = 300, width = 180, height = 70, units = "mm")


neon_ndvi_tmap <-
  tmap::tm_shape(neon_resampled_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_grid(alpha = 0.2, n.x = 4, n.y = 4) +
  tmap::tm_xlab(text = "                                                                                                    Easting (m)") +
  tmap::tm_ylab(text = "Northing (m)", space = 0.1) +
  tmap::tm_layout(legend.show = FALSE, asp = 1)

neon_ndvi_tmap

drone_ndvi_tmap <-
  tmap::tm_shape(drone_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_grid(alpha = 0.2, labels.show = c(TRUE, TRUE), n.x = 4, n.y = 4) +
  tmap::tm_xlab(text = "Easting (m)                                                             ") +
  tmap::tm_ylab(text = "") +
  tmap::tm_layout(legend.show = FALSE, asp = 1)

drone_ndvi_tmap

legend_ndvi_tmap <-
  tmap::tm_shape(drone_ndvi) +
  tmap::tm_raster(style = "cont", palette = viridis(100), title = "NDVI", midpoint = NA, breaks = c(-1, -0.5, 0, 0.5, 1.0)) +
  tmap::tm_layout(legend.only = TRUE, legend.position = c("right", "center"))

two_panel <- 
  tmap::tmap_arrange(neon_ndvi_tmap, drone_ndvi_tmap, legend_ndvi_tmap, 
                     nrow = 1, ncol = 3, widths = c(0.45, 0.45, 0.1), 
                     outer.margins = c(0.01, 0.01, 0, 0))

tmap::tmap_save(tm = two_panel, filename = file.path("figs", "ndvi_neon-spectral-resampled-v-drone-original.png"), dpi = 300, width = 180, height = 70, units = "mm")