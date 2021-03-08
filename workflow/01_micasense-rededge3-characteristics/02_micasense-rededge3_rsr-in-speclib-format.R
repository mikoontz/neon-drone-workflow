# Create a SpecLib from the Micasense Rededge3 relative spectral response
# function for use in spectral resampling of hyperspectral data (e.g., 
# like those from the NEON AOP spectrometer)

# The SpecLib class comes from the {hsdar} package and makes it straightforward
# to take hyperspectral data (like those from the NEON AOP) and resample to 
# match the characteristics of a different multi-band sensor (like the 
# Micasense Rededge3, Landsat, etc.)

library(dplyr)
library(tidyr)

# arbitrary relative spectral response threshold above which there is a 
# "high enough" spectral response to warrant inclusion of that wavelength
# in the later spectral operations
rsr_threshold <- 0.125

# get micasense basic sensor info
micasense_rededge3_characteristics <- read.csv("data/out/micasense-rededge3_sensor-characteristics.csv")

# Get micasense rededge spectral response function
micasense_rededge3_response <- 
  read.csv("data/out/micasense-rededge3-relative-spectral-response.csv")
  
# What is the range of wavelengths for which we can reasonably use the sensor?
rsr_with_threshold <- 
  micasense_rededge3_response %>% 
  dplyr::filter(relative_spectral_response >= rsr_threshold) %>% 
  dplyr::group_by(band) %>% 
  summarize(lb = floor(min(wavelength_nm)),
            ub = ceiling(max(wavelength_nm)))

# join the basic characteristics with info on "usable" wavelengths
micasense_rededge3_characteristics <-
  micasense_rededge3_characteristics %>% 
  dplyr::left_join(rsr_with_threshold)

# The "valid" wavelengths for the Micasense Rededge3 
micasense_wavelength_range <-
  seq(floor(min(micasense_rededge3_characteristics$lb)), ceiling(max(micasense_rededge3_characteristics$ub)))

micasense_rededge3_response <-
  micasense_rededge3_response %>% 
  dplyr::filter(wavelength_nm %in% micasense_wavelength_range) %>% 
  dplyr::select(band, wavelength_nm, relative_spectral_response)

rededge_speclib_mat <-
  micasense_rededge3_response %>% 
  pivot_wider(names_from = "band", values_from = "relative_spectral_response")

write.csv(x = rededge_speclib_mat, 
          file = "data/out/micasense-rededge3_speclib-matrix.csv",
          row.names = FALSE)
