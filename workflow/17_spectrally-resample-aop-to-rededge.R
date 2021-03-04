library(dplyr)
library(hdf5r)

rsr_threshold <- 0.125

target_rsr <- 
  read.csv("data/out/micasense-rededge3-relative-spectral-response.csv") %>% 
  filter(relative_spectral_response >= rsr_threshold)

ggplot(target_rsr, aes(x = wavelength_nm, y = relative_spectral_response, color = band)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(y = "Relative spectral response")

fname_h5 <- list.files("data/raw/AOP", recursive = TRUE, full.names = TRUE)
file_h5 <- lapply(fname_h5, FUN = hdf5r::H5File$new, mode = "r+")

file_h5 <- file_h5[[1]]

site <- file_h5$names

# spectral
wavelength <- file_h5[[site]][["Reflectance/Metadata/Spectral_Data/Wavelength"]]
# wavelength$read()
fwhm <- file_h5[[site]][["Reflectance/Metadata/Spectral_Data/FWHM"]]
# fwhm$read()

HS_params <- data.frame(band_center = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/Wavelength"]]$read(),
                        fwhm = file_h5[[site]][["Reflectance/Metadata/Spectral_Data/FWHM"]]$read())

# HS_band_centers <- wavelength$read()
# HS_FWHM <- fwhm$read()

# CRS
# crs_str <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Coordinate_System_String"]]
# crs_epsg <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/EPSG Code"]]
# crs_mapinfo <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Map_Info"]]
# crs_proj4 <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Proj4"]]

# reflectance <- file_h5[[site]][["Reflectance"]][["Reflectance_Data"]]

micasense_usable_wavelengths <-
  read.csv("data/out/micasense-rededge3-nominal-sensitivity.csv") %>% 
  filter(!is.na(color)) %>% 
  pull(nm)

micasense_rsr <-
  read.csv("data/out/micasense-rededge3-relative-spectral-response.csv")

bandnames <- unique(micasense_rsr$band)
j <- 1

# start with blue! Will build this to iterate over all bands properly
# target_bandpass <-
#   target_rsr %>%
#   filter(band == bandnames[j]) %>%
#   filter(wavelength_nm >= min(usable_wavelengths) & wavelength_nm <= max(usable_wavelengths)) %>% 
#   pull(wavelength_nm)
# 
# target_RSR <-
#   target_rsr %>%
#   filter(band == bandnames[j]) %>%
#   filter(wavelength_nm >= min(usable_wavelengths) & wavelength_nm <= max(usable_wavelengths)) %>% 
#   pull(relative_spectral_response)

target_rsr <-
  micasense_rsr %>%
  filter(band == bandnames[j]) %>%
  filter(wavelength_nm >= min(usable_wavelengths) & wavelength_nm <= max(usable_wavelengths)) %>% 
  dplyr::select(wavelength_nm, relative_spectral_response)

# values_lower = np.where(HS_band_centers>np.min(target_bandpass))
# values_upper = np.where(HS_band_centers<np.max(target_bandpass))
# values = np.intersect1d(values_lower,values_upper) # valid band centers relative to target band pass

# values <- HS_band_centers[HS_band_centers >= min(target_bandpass) & HS_band_centers <= max(target_bandpass)]
# values <- which(HS_band_centers >= min(target_bandpass) & HS_band_centers <= max(target_bandpass))

usable_HS_params <- values <- 
  HS_params %>% 
  filter(band_center >= min(target_rsr$wavelength_nm) & band_center <= max(target_rsr$wavelength_nm))

# target_resample_range = np.arange(np.min(target_bandpass),np.max(target_bandpass),1)
# target_resample = np.interp(target_resample_range, target_bandpass, target_RSR)

# spectral resolution for interpolation in nanometers
## NOTE: the relative spectral response for the Micasense RedEdge3 is already at
## 1 nm precision, so the interpolation described here returns the same values of
## relative spectral response that we already have. However, this effort is useful
## in cases where we don't have the relative spectral response per nanometer, but
## something more spectrally coarse
spec_res_interp <- 1
target_resample_range <- seq(min(target_rsr$wavelength_nm), max(target_rsr$wavelength_nm), by = spec_res_interp)

## Interpolate the relative spectral response at the `spec_res_interp` precision 
## using the known relative spectral resonse of the target sensor (in this case the
## Micasense Rededge3)
## Make sure to do this separately for each band
target_resample <-
  target_rsr %>%
  split(target_rsr$band) %>% 
  lapply(FUN = function(x) {
    approx(x = x$wavelength_nm, y = x$relative_spectral_response, xout = target_resample_range)
  })

# example call:
# get_fit_wave(NEON_guassian_wave_params, target_bandpass[-1]-target_bandpass[0]+1, 1)

# Get the relative spectral response of the NEON spectral imager at each of the
# desired wavelengths of the target sensor using known properties of the
# NEON sensor (i.e., the full width half maximum value and the center wavelength)
get_fit_wave <- function(params, num_value, num_peak) {
  # this callback calculates f(c,x)=exp(-c0*sqr(x0))
  # where x is a position on X-axis and c is adjustable parameter
  # params are the maximum 
  c <- params
  
  # x = np.arange(1, num_value)
  x <- 1:num_value
  
  if (num_peak == 1) {
    wave <- c[1] * exp(-((x-c[2])/c[3])^2)
  }
  # } else if (num_peak == 2) {
  #   wave <- c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)
  # } else if (num_peak == 3) {
  #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)
  # } else if(num_peak == 4) {
  #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2) 
  # } else if (num_peak == 5) {
  #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2) 
  # } else if (num_peak == 6) {
  #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2) 
  # } else if(num_peak == 7) {
  #   wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2)+c[18]*np.exp(-((x-c[19])/c[20])**2) 
  # } 
  
  
  return(wave)
}

weights <- vector(mode = "list", length = length(values))
NEON_gaussian_wave <- vector(mode = "list", length = length(values))

for (i in 1:length(values)) {
  
  # NEON_guassian_wave_params = np.array([[1,HS_band_centers[values[i]]-target_bandpass[0]-(HS_band_centers[values[0]]-target_bandpass[0])+2,1.201*HS_FWHM[values[i]]]],dtype = np.float32)
  NEON_guassian_wave_params = c(1, HS_band_centers[values[i]] - 
                                           target_bandpass[1] - 
                                           (HS_band_centers[values[1]] - target_bandpass[1]) + 2,
                                         1.201 * HS_FWHM[values[i]])
  
NEON_gaussian_wave[[i]] = get_fit_wave(params = NEON_guassian_wave_params, 
                                  num_value = target_bandpass[length(target_bandpass)] - target_bandpass[1] + 1, 
                                  num_peak = 1)

weights[[i]] <- convolve(x = target_resample[[bandnames[j]]]$y, y = rev(NEON_gaussian_wave[[i]]), type = "o")

# weights[i] = np.convolve(target_resample, NEON_gaussian_wave, mode='valid')

}

plot(target_resample_range, NEON_gaussian_wave)

test = data.frame(nm = rep(target_resample_range, times = length(NEON_gaussian_wave)), neon_wave = unlist(NEON_gaussian_wave), wave_num = rep(1:length(NEON_gaussian_wave), each = length(target_resample_range)))

ggplot(test %>% filter(nm > 600 & nm < 615) %>% mutate(wave_num = factor(wave_num)), aes(x = nm, y = neon_wave, color = wave_num)) + geom_line()

