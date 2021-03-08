# Calibrated reflectance panel reflectance 
# "Reflectance curve for Micasense Calibrated Reflectance Panel RP02-1701230-SC"

library(tidyverse)

# get micasense basic sensor info
micasense_rededge3_characteristics <- 
  read.csv("data/out/micasense-rededge3_sensor-characteristics.csv") %>% 
  dplyr::mutate(lb = center - fwhm/2,
                ub = center + fwhm/2) 

micasense_rededge3_characteristics_long <-
  micasense_rededge3_characteristics %>% 
  tidyr::pivot_longer(cols = c(center, lb, ub), names_to = "type", values_to = "wavelength") %>% 
  dplyr::mutate(band_fullname = case_when(band == "re" ~ "red edge",
                                          band == "nir" ~ "near infrared",
                                          TRUE ~ band)) %>% 
  dplyr::mutate(band_fullname = factor(band_fullname, 
                                       levels = c("blue", "green", "red", "red edge", "near infrared"))) +
  dplyr::mutate(is_center = )


refl <- 
  read.csv(file = "data/raw/RP02-1701230-SC.csv", header = FALSE) %>% 
  setNames(c("wavelength", "reflectance")) %>% 
  dplyr::filter(wavelength > 400 & wavelength < 900)

ggplot(refl, aes(x = wavelength, y = reflectance)) +
  geom_point() +
  geom_vline(data = micasense_rededge3_characteristics_long, 
             mapping = aes(xintercept = wavelength, 
                           color = band_fullname, 
                           lty = type != "center")) +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       color = "Band name") +
  theme_bw() +
  guides(lty = FALSE) # turn off the "center" versus "not center" legend

write.csv(x = refl, 
          file = "data/out/micasense-rededge3-calibrated-reflectance-panel.csv", 
          row.names = FALSE)

ggsave(filename = "figs/micasense-rededge3-calibrated-reflectance-panel.png")



