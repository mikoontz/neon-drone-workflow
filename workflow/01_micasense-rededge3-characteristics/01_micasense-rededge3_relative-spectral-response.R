# Derive the relative spectral response of the Micasense Rededge3 sensor

library(dplyr)
library(tidyr)
library(ggplot2)

# Create directories
dir.create("data/raw", showWarnings = FALSE)
dir.create("data/out", showWarnings = FALSE)

# Micasense Rededge3 basic sensor characteristics ------------------------------
# Note that the convention for the Micasense sensors is to 
# call the red edge band the 5th channel, even though it
# would be the 4th channel if counting sequentially from
# shortest wavelength to longest wavelength

# These come from the product user's guide 
# (https://support.micasense.com/hc/en-us/articles/215261448-RedEdge-User-Manual-PDF-Download-)

micasense_rededge3_characteristics <- 
  data.frame(channel = c(1:3, 5, 4), 
             band = c("blue", "green", "red", "re", "nir"), 
             fwhm = c(20, 20, 10, 10, 40),
             center = c(475, 560, 668, 717, 840))

write.csv(x = micasense_rededge3_characteristics, 
          file = "data/out/micasense-rededge3_sensor-characteristics.csv",
          row.names = FALSE)

# Micasense Rededge3 relative spectral response --------------------------------
# Note these data came directly from Micasense and we were told there are no
# restrictions on sharing/publishing them
# If you have a different Micasense sensor (or one from a different company),
# reach out to the company to see whether they'll share the response curves
# for the instrument

# quantum efficiency of silicon chip in RedEdge 3 camera
qe <- 
  readr::read_csv("data/raw/MicasenseQEvals_trim.csv", skip = 1) %>% 
  dplyr::mutate(qe = QE_percent * 0.01) %>% 
  setNames(c("wavelength_nm", "qe_pct", "qe"))

# bandpass filter sensitivity
filters <-
  read.csv("data/out/RedEdge_3_Filters_srs.csv") %>%
  setNames(c("wavelength_nm", "blue", "green", "red", "re", "nir")) %>% 
  tidyr::pivot_longer(names_to = "band", values_to = "transmission", cols = -1) %>% 
  dplyr::mutate(band = factor(band, levels = c("blue", "green", "red", "re", "nir")))

# Plot both 
ggplot() +
  geom_line(data = filters, mapping = aes(x = wavelength_nm, y = transmission, col = band)) +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  theme_bw() +
  geom_line(data = qe, mapping = aes(x = wavelength_nm, y = qe)) +
  labs(x = "Wavelength (nm)",
       y = "Bandpass filter transmission (colored lines)\nQuantum efficiency (black line)",
       color = "Band")

# Join the two datasets and combine the two sensitivities (bandpass filter and silicon chip quantum efficiency)
rsr <-
  filters %>% 
  dplyr::left_join(qe, by = "wavelength_nm") %>% 
  dplyr::mutate(spectral_response = transmission * qe)

# Plot the raw combination
ggplot(rsr, aes(x = wavelength_nm, y = spectral_response, color = band)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(y = "Spectral response")

# Peak normalize the combined sensitivity per band
rsr <-
  rsr %>% 
  dplyr::mutate(band = as.character(band)) %>% 
  dplyr::mutate(band_fullname = case_when(band == "re" ~ "red edge",
                                          band == "nir" ~ "near infrared",
                                          TRUE ~ band)) %>% 
  dplyr::mutate(band_fullname = factor(band_fullname, levels = c("blue", "green", "red", "red edge", "near infrared"))) %>% 
  dplyr::group_by(band_fullname) %>% 
  dplyr::mutate(relative_spectral_response = spectral_response / max(spectral_response, na.rm = TRUE)) %>% 
  dplyr::ungroup()

ggplot(rsr, aes(x = wavelength_nm, y = relative_spectral_response, color = band_fullname)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(color = "Band name",
       x = "Wavelength (nm)",
       y = "Relative spectral response") +
  theme_bw()

ggsave("figs/micasense-rededge3-relative-spectral-response.png")

write.csv(x = rsr, 
          file = "data/out/micasense-rededge3-relative-spectral-response.csv", 
          row.names = FALSE)
