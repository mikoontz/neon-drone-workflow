# Convert Rededge imagery to AOP imagery based on characteristics of each sensor

library(tidyverse)


# RedEdge3 sensor characteristics -----------------------------------------

# quantum efficiency of silicon chip in RedEdge 3 camera
qe <- 
  readr::read_csv("data/raw/MicasenseQEvals_trim.csv", skip = 1) %>% 
  dplyr::mutate(qe = QE_percent * 0.01) %>% 
  setNames(c("wavelength_nm", "qe_pct", "qe"))

# bandpass filter sensitivity
filters <-
  readr::read_csv("data/out/RedEdge_3_Filters_srs.csv") %>% 
  dplyr::rename(wavelength_nm = `Wavlength (nm)`, 
                blue = `Band 1`,
                green = `Band 2`,
                red = `Band 3`,
                `re` = `Band 5`,
                `nir` = `Band 4`) %>% 
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
  dplyr::group_by(band) %>% 
  dplyr::mutate(relative_spectral_response = spectral_response / max(spectral_response, na.rm = TRUE)) %>% 
  dplyr::ungroup()

ggplot(rsr, aes(x = wavelength_nm, y = relative_spectral_response, color = band)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(y = "Relative spectral response")

ggsave("figs/micasense-rededge3-relative-spectral-response.png")

write.csv(x = rsr, file = "data/out/micasense-rededge3-relative-spectral-response.csv", row.names = FALSE)
