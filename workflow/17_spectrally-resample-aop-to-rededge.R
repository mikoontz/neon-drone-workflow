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

# CRS
crs_str <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Coordinate_System_String"]]
crs_epsg <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/EPSG Code"]]
crs_mapinfo <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Map_Info"]]
crs_proj4 <- file_h5[[site]][["Reflectance/Metadata/Coordinate_System/Proj4"]]


reflectance <- file_h5[[site]][["Reflectance"]][["Reflectance_Data"]]

HS_band_centers <- wavelength$read()
HS_FWHM <- fwhm$read()

target_rsr <- 
  read.csv("data/out/micasense-rededge3-relative-spectral-response.csv")

target_bandpass <- 
  target_rsr %>% 
  filter(band == "blue") %>% 
  pull(wavelength_nm)

target_RSR <- 
  target_rsr %>% 
  filter(band == "blue") %>% 
  pull(relative_spectral_response)

# values_lower = np.where(HS_band_centers>np.min(target_bandpass))
# values_upper = np.where(HS_band_centers<np.max(target_bandpass))
# values = np.intersect1d(values_lower,values_upper) # valid band centers relative to target band pass

values <- HS_band_centers[HS_band_centers >= min(target_bandpass) & HS_band_centers <= max(target_bandpass)]

# target_resample_range = np.arange(np.min(target_bandpass),np.max(target_bandpass),1)
# target_resample = np.interp(target_resample_range, target_bandpass, target_RSR)

target_resample_range <- seq(min(target_bandpass), max(target_bandpass), by = 1)

target_resample <-
  target_rsr %>%
  split(target_rsr$band) %>% 
  lapply(FUN = function(x) {
    approx(x = x$wavelength_nm, y = x$relative_spectral_response, xout = target_resample_range)
  })

weights = np.zeros_like(values,dtype=np.float32)

get_fit_wave <- function(params, num_value, num_peak) {
  # this callback calculates f(c,x)=exp(-c0*sqr(x0))
  # where x is a position on X-axis and c is adjustable parameter
  
  #num_peak = initial_params[0,-1]
  #num_peak = 1
  
  c = params[0,0:int(num_peak*3)]
  
  x = np.arange(1,num_value)
  
  if (num_peak == 1) {
    wave <- c[0]*np.exp(-((x-c[1])/c[2])**2)
  } else if (num_peak == 2) {
    wave <- c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)
  } else if (num_peak == 3) {
    wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)
  } else if(num_peak == 4) {
    wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2) 
  } else if (num_peak == 5) {
    wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2) 
  } else if (num_peak == 6) {
    wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2) 
  } else if(num_peak == 7) {
    wave = c[0]*np.exp(-((x-c[1])/c[2])**2)+c[3]*np.exp(-((x-c[4])/c[5])**2)+c[6]*np.exp(-((x-c[7])/c[8])**2)+c[9]*np.exp(-((x-c[10])/c[11])**2)+c[12]*np.exp(-((x-c[13])/c[14])**2)+c[15]*np.exp(-((x-c[16])/c[17])**2)+c[18]*np.exp(-((x-c[19])/c[20])**2) 
  } 
  
  
  return(wave)
}


for i in range(elements):
  
  NEON_guassian_wave_params = np.array([[1,HS_band_centers[values[i]]-target_bandpass[0]-(HS_band_centers[values[0]]-target_bandpass[0])+2,1.201*HS_FWHM[values[i]]]],dtype = np.float32)
NEON_gaussian_wave = get_fit_wave(NEON_guassian_wave_params, target_bandpass[-1]-target_bandpass[0]+1, 1)
weights[i] = np.convolve(target_resample,NEON_gaussian_wave,mode='valid')
plt.plot(target_resample_range,NEON_gaussian_wave)

plt.show()
weights

