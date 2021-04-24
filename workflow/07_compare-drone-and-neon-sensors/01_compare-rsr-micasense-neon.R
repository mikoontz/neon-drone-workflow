# Plot spectral response curve for NEON and Micasense Rededge 3 next to 
# each other

library(tidyverse)
library(raster)
library(hdf5r)
library(hsdar)
library(readr)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# Get the Micasense RedEdge 3 relative spectral response data
micasense_rededge3_rsr <- 
  read_csv(file = "data/out/micasense-rededge3-relative-spectral-response.csv") %>% 
  mutate(source = "rededge3",
         channel = paste(source, as.numeric(as.factor(band)), sep = "_")) %>% 
  dplyr::select(channel, band_fullname, wavelength_nm, relative_spectral_response, source)

# Get NEON AOP spectrometer file(s) that were downloaded in previous script
fname_h5 <- list.files("data/raw/AOP", recursive = TRUE, full.names = TRUE, pattern = ".h5$")
file_h5 <- lapply(fname_h5, FUN = hdf5r::H5File$new, mode = "r+")

# This script is currently set up to show how to work with a single one of the
# NEON AOP spectrometer files, so we'll just use the first list item representing
# the first file read by the line above using the hdf5r::H5File$new call above
file_h5 <- file_h5[[1]]

# Get some details from the NEON AOP data
site <- file_h5$names

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

neon_hs_params_long <-
  neon_hs_params %>% 
  mutate(data = purrr::pmap(.l = ., 
                            .f = function(channel, band_center, fwhm) { 
                              wavelength_nm <- seq(band_center - 10,
                                                   band_center + 10, 
                                                   by = 0.2)
                              
                              relative_spectral_response <- dnorm(x = wavelength_nm, 
                                                                 mean = band_center, 
                                                                 sd = fwhm / (2*sqrt(2 * log(2))))
                              
                              relative_spectral_response <- relative_spectral_response / max(relative_spectral_response)
                              
                              return(data.frame(wavelength_nm, relative_spectral_response))
                              
                            }
  )) %>% 
  tidyr::unnest(cols = "data") %>% 
  mutate(source = "neon",
         band_fullname = channel,
         channel = paste(source, channel, sep = "_")) %>% 
  dplyr::select(channel, band_fullname, wavelength_nm, relative_spectral_response, source)

neon_micasense_rsr <- rbind(neon_hs_params_long, micasense_rededge3_rsr)

neon_micasense_rsr_zoom <- 
  neon_micasense_rsr %>% 
  dplyr::filter(wavelength_nm > 650 & wavelength_nm < 750) %>% 
  dplyr::filter(relative_spectral_response > 0.01) %>% 
  mutate(source = ifelse(source == "neon", yes = "NEON", no = "Micasense RedEdge 3"))

neon_micasense_rsr_gg <-
  ggplot(neon_micasense_rsr_zoom, 
       aes(x = wavelength_nm, y = relative_spectral_response, color = source, group = channel)) + 
  geom_line() +
  scale_color_manual(values = c("red", "black")) +
  theme_bw() +
  labs(x = "Wavelength (nm)",
       y = "Relative spectral response",
       color = "Instrument")

ggsave(filename = "figs/relative-spectral-response_micasense-rededge3-vs-neon-aop.png", 
       plot = neon_micasense_rsr_gg)
