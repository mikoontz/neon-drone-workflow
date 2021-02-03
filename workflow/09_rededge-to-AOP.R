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
                `red edge` = `Band 5`,
                `near infrared` = `Band 4`) %>% 
  tidyr::pivot_longer(names_to = "band", values_to = "reflectance", cols = -1) %>% 
  dplyr::mutate(band = factor(band, levels = c("blue", "green", "red", "red edge", "near infrared")))

# Plot both 
ggplot() +
  geom_line(data = filters, mapping = aes(x = wavelength_nm, y = reflectance, col = band)) +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  theme_bw() +
  geom_line(data = qe, mapping = aes(x = wavelength_nm, y = qe)) +
  labs(x = "Wavelength (nm)",
       y = "Reflectance (colored lines)\nQuantum efficiency (black line)",
       color = "Band")

# Join the two datasets and combine the two sensitivities (bandpass filter and silicon chip)
sensitivity <-
  filters %>% 
  dplyr::left_join(qe, by = "wavelength_nm") %>% 
  dplyr::mutate(sensitivity = reflectance * qe)

# Plot the raw combination
ggplot(sensitivity, aes(x = wavelength_nm, y = sensitivity, color = band)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred"))

# Peak normalize the combined sensitivity per band
sensitivity <-
  sensitivity %>% 
  dplyr::group_by(band) %>% 
  dplyr::mutate(sensitivity = sensitivity / max(sensitivity, na.rm = TRUE)) %>% 
  dplyr::ungroup()

ggplot(sensitivity, aes(x = wavelength_nm, y = sensitivity, color = band)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred"))

ggsave("figs/rededge3-wavelength-sensitivity.png")
