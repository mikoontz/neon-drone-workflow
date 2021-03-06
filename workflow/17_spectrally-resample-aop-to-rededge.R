library(dplyr)
library(hdf5r)
library(purrr)
library(hsdar)
library(ggplot2)
library(sf)
library(tidyr)

# Get NEON AOP file
fname_h5 <- list.files("data/raw/AOP", recursive = TRUE, full.names = TRUE)
file_h5 <- lapply(fname_h5, FUN = hdf5r::H5File$new, mode = "r+")
file_h5 <- file_h5[[1]]
site <- file_h5$names
crs_epsg <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/EPSG Code"]]$read()

# get micasense sensor info
micasense_usable_wavelengths <-
  read.csv("data/out/micasense-rededge3-nominal-sensitivity.csv") %>% 
  dplyr::filter(!is.na(color)) %>% 
  pull(nm) 

micasense_wavelength_range <- c(floor(min(micasense_usable_wavelengths)), 
                                ceiling(max(micasense_usable_wavelengths)))

# get Gaussian wave parameters of NEON AOP reflectance data
neon_hs_params <- 
  data.frame(band_center = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/Wavelength"]]$read(),
             fwhm = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/FWHM"]]$read()) %>% 
  mutate(id = 1:nrow(.)) %>% 
  dplyr::select(id, everything())

mission_footprint <- 
  sf::st_read("data/drone/L0/mission-footprint/niwo_017/2019-10-09/niwo_017_2019-10-09_site-bounds.gpkg") %>% 
  sf::st_transform(mission_footprint, crs = as.numeric(crs_epsg))

neon_usable_bands <- 
  neon_hs_params %>% 
  dplyr::filter(band_center >= micasense_wavelength_range[1] & band_center <= micasense_wavelength_range[2])

hs <- neonhs::hs_read(filename = fname_h5[1], 
                      bands = neon_usable_bands$id,
                      crop = mission_footprint)

raster::writeRaster(x = hs, filename = "data/out/temp-raster.tif", overwrite = TRUE)

neon_speclib <- hsdar::speclib("data/out/temp-raster.tif", 
                               wavelength = neon_usable_bands$band_center,
                               fwhm = neon_usable_bands$fwhm)

# Get micasense rededge spectral response function
target_rsr <- 
  read.csv("data/out/micasense-rededge3-relative-spectral-response.csv") %>% 
  filter(relative_spectral_response >= rsr_threshold)

rededge_speclib_mat <-
  target_rsr %>% 
  pivot_wider(names_from = "band", values_from = "relative_spectral_response")

rededge_speclib <- speclib(as.matrix(rededge_speclib_mat[, -1]), rededge_speclib_mat$wavelength_nm)

neon_resampled_to_rededge <- spectralResampling(x = neon_speclib, response_function = rededge_speclib)

r <- neon_resampled_to_rededge@spectra@spectra_ra

plot(r[[1]])






















# 
# 
# # HS_band_centers <- wavelength$read()
# # HS_FWHM <- fwhm$read()
# 
# # CRS
# # crs_str <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Coordinate_System_String"]]
# 
# # crs_mapinfo <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Map_Info"]]
# # crs_proj4 <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Proj4"]]
# 
# 
# reflectance <- file_h5[[site]][["Reflectance"]][["Reflectance_Data"]]
# 
# # Grab the UTM coordinates of the spatial extent
# xMin <- h5attr(reflectance, "Spatial_Extent_meters")[1]
# xMax <- h5attr(reflectance, "Spatial_Extent_meters")[2]
# yMin <- h5attr(reflectance, "Spatial_Extent_meters")[3]
# yMax <- h5attr(reflectance, "Spatial_Extent_meters")[4]
# 
# # define the extent (left, right, top, bottom)
# rasExt <- extent(xMin,xMax,yMin,yMax)
# 
# # view the extent to make sure that it looks right
# rasExt
# 
# # Finally, define the no data value for later
# myNoDataValue <- as.integer(h5attr(reflectance, "Data_Ignore_Value"))
# myNoDataValue
# 
# 
# 
# micasense_rsr <-
#   read.csv("data/out/micasense-rededge3-relative-spectral-response.csv")
# 
# target_rsr <-
#   micasense_rsr %>%
#   filter(wavelength_nm >= min(micasense_usable_wavelengths) & wavelength_nm <= max(micasense_usable_wavelengths)) %>% 
#   dplyr::select(band, wavelength_nm, relative_spectral_response)
# 
# # spectral resolution for interpolation in nanometers
# ## NOTE: the relative spectral response for the Micasense RedEdge3 is already at
# ## 1 nm precision, so the interpolation described here returns the same values of
# ## relative spectral response that we already have. However, this effort is useful
# ## in cases where we don't have the relative spectral response per nanometer, but
# ## something more spectrally coarse like only every 5 nanometers
# spec_res_interp <- 1
# target_resample_range <- seq(min(target_rsr$wavelength_nm), max(target_rsr$wavelength_nm), by = spec_res_interp)
# 
# ## Interpolate the relative spectral response at the `spec_res_interp` precision 
# ## using the known relative spectral response of the target sensor (in this case the
# ## Micasense Rededge3)
# ## Make sure to do this separately for each band
# ## Reiterating an implication of NOTE above, the `y` values for the Micasense
# ## camera for a given band are going to be identical to the `relative_spectral_response`
# ## values for that band.
# ## target_resample[["blue"]]$y == target_rsr[target_rsr$band == "blue", "relative_spectral_response"]
# target_resample <-
#   target_rsr %>%
#   split(target_rsr$band) %>% 
#   lapply(FUN = function(x) {
#     approx(x = x$wavelength_nm, y = x$relative_spectral_response, xout = target_resample_range)
#   })
# 
# # # example call:
# # # get_fit_wave(NEON_guassian_wave_params, target_bandpass[-1]-target_bandpass[0]+1, 1)
# # 
# # # Get the relative spectral response of the NEON spectral imager at each of the
# # # desired wavelengths of the target sensor using known properties of the
# # # NEON sensor (i.e., the full width half maximum value and the center wavelength)
# # get_fit_wave <- function(params, num_value, num_peak) {
# #   # this callback calculates f(c,x)=exp(-c0*sqr(x0))
# #   # where x is a position on X-axis and c is adjustable parameter
# #   # params are the maximum 
# #   c <- params
# #   
# #   # x = np.arange(1, num_value)
# #   x <- 1:num_value
# #   
# #   if (num_peak == 1) {
# #     wave <- c[1] * exp(-((x-c[2])/c[3])^2)
# #   }
# #   # } else if (num_peak == 2) {
# #   #   wave <- c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)
# #   # } else if (num_peak == 3) {
# #   #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)
# #   # } else if(num_peak == 4) {
# #   #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2) 
# #   # } else if (num_peak == 5) {
# #   #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2) 
# #   # } else if (num_peak == 6) {
# #   #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2) 
# #   # } else if(num_peak == 7) {
# #   #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2)+c[18]*np.exp(-((x-c[19])/c[20])**2) 
# #   # } 
# #   
# #   
# #   return(wave)
# # }
# 
# target_bands <- unique(target_rsr$band)
# out <- vector(mode = "list", length = length(target_bands))
# 
# for(i in seq_along(target_bands)) {
#   this_band <- target_bands[i]
#   
#   target_bandpass <- target_rsr$wavelength_nm[target_rsr$band == this_band]
#   target_band_rsr <- target_rsr$relative_spectral_response[target_rsr$band == this_band]
#   
#   out[[i]] <- 
#     HS_params  %>% 
#     dplyr::rowwise() %>% 
#     mutate(wave = list(dnorm(x = target_bandpass, mean = band_center, sd = sigma)),
#            wave2 = list(1 / (sqrt(2 * pi) * sigma) * exp(-((target_bandpass - band_center) ^ 2 / (2 * sigma^2)))),
#            weights = list(convolve(x = target_band_rsr, y = rev(wave), type = "o")))
# }
# 
# 
# # np.divide(np.sum(np.multiply(Refl[:,:,values],np.flipud(weights)),axis=2),np.sum(weights))
# 
# usable_HS_params <- 
#   HS_params %>% 
#   filter(band_center >= min(micasense_usable_wavelengths) & band_center <= max(micasense_usable_wavelengths))
# 
# 
# convolve(x = target_resample[[bandnames[j]]]$y, y = rev(NEON_gaussian_wave[[i]]), type = "o")
# 
# weights <- vector(mode = "list", length = length(values))
# NEON_gaussian_wave <- vector(mode = "list", length = length(values))
# 
# for (i in 1:length(values)) {
#   
#   # NEON_guassian_wave_params = np.array([[1,HS_band_centers[values[i]]-target_bandpass[0]-(HS_band_centers[values[0]]-target_bandpass[0])+2,1.201*HS_FWHM[values[i]]]],dtype = np.float32)
#   NEON_guassian_wave_params = c(1, HS_band_centers[values[i]] - 
#                                   target_bandpass[1] - 
#                                   (HS_band_centers[values[1]] - target_bandpass[1]) + 2,
#                                 1.201 * HS_FWHM[values[i]])
#   
#   NEON_gaussian_wave[[i]] = get_fit_wave(params = NEON_guassian_wave_params, 
#                                          num_value = target_bandpass[length(target_bandpass)] - target_bandpass[1] + 1, 
#                                          num_peak = 1)
#   
#   weights[[i]] <- convolve(x = target_resample[[bandnames[j]]]$y, y = rev(NEON_gaussian_wave[[i]]), type = "o")
#   
#   # weights[i] = np.convolve(target_resample, NEON_gaussian_wave, mode='valid')
#   
# }
# 
# plot(target_resample_range, NEON_gaussian_wave)
# 
# test = data.frame(nm = rep(target_resample_range, times = length(NEON_gaussian_wave)), neon_wave = unlist(NEON_gaussian_wave), wave_num = rep(1:length(NEON_gaussian_wave), each = length(target_resample_range)))
# 
# ggplot(test %>% filter(nm > 600 & nm < 615) %>% mutate(wave_num = factor(wave_num)), aes(x = nm, y = neon_wave, color = wave_num)) + geom_line()
# 
# 

