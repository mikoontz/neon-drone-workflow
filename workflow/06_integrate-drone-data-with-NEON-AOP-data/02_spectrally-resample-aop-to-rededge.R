# remotes::install_github('earthlab/neonhs')
library(neonhs)
library(dplyr)
library(hdf5r)
library(purrr)
library(hsdar)
library(ggplot2)
library(sf)
library(tidyr)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# create necessary directories
dir.create("data/out/AOP", recursive = TRUE, showWarnings = FALSE)

# files to be read using this script
mission_footprint_fname <- "data/drone/L0/mission-footprint/niwo_017/2019-10-09/niwo_017_2019-10-09_site-bounds.gpkg"
cropped_ortho_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
micasense_rededge3_characteristics_fname <-  "data/out/micasense-rededge3_sensor-characteristics.csv"

# files to be written using this script
neon_aop_spectral_crop <- file.path("data", "out", "AOP", "neon-aop-spectral-crop-for-micasense-rededge.tif")

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
fname_h5 <- list.files("data/raw/AOP", recursive = TRUE, full.names = TRUE)
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
neon_resampled_to_rededge <- spectralResampling(x = neon_speclib, 
                                                response_function = rededge_speclib)

# The result of the spectral resampling is another SpecLib, but we can retrieve
# the raster version of those results
neon_resampled_r <- 
  neon_resampled_to_rededge@spectra@spectra_ra %>% 
  setNames(hsdar::idSpeclib(rededge_speclib))

# read in necessary data products from drone-mounted micasense Rededge3
# processed data
ortho <- 
  raster::brick(cropped_ortho_fname) %>% 
  setNames(micasense_rededge3_characteristics$band)

# Calculate NDVI from both products
neon_resampled_ndvi <-
  (neon_resampled_r[["nir"]] - neon_resampled_r[["red"]]) / (neon_resampled_r[["nir"]] + neon_resampled_r[["red"]])

drone_ndvi <- (ortho[["nir"]] - ortho[["red"]]) / (ortho[["nir"]] + ortho[["red"]])

# Plot side by side
par(mfrow = c(1, 2))
plot(neon_resampled_ndvi)
plot(drone_ndvi)


