# Calibrated reflectance panel sensitivity 

library(tidyverse)

lwr_nm <- 465
upr_nm <- 860

refl <- 
  readr::read_csv(file = "data/raw/RP02-1701230-SC.csv", 
           col_names = c("nm", "reflectance")) %>% 
  dplyr::filter((nm >= lwr_nm - 10) & (nm <= upr_nm + 10)) %>% 
  dplyr::mutate(color = case_when(nm >= 465 & nm <= 485 ~ "blue",
                                  nm >= 550 & nm <= 570 ~ "green",
                                  nm >= 663 & nm <= 673 ~ "red",
                                  nm >= 712 & nm <= 722 ~ "re",
                                  nm >= 820 & nm <= 860 ~ "nir")) %>% 
  dplyr::mutate(color = factor(color, levels = c("blue", "green", "red", "re", "nir"))) %>% 
  dplyr::mutate(center = ifelse(nm %in% c(475, 560, 668, 717, 840), yes = TRUE, no = FALSE))
  

ggplot(refl, aes(x = nm, y = reflectance)) +
  geom_point() +
  geom_vline(xintercept = c(465, 475, 485), col = "#0000ff", lty = c(2, 1, 2)) +
  geom_vline(xintercept = c(550, 560, 570), col = "#00ff00", lty = c(2, 1, 2)) +
  geom_vline(xintercept = c(663, 668, 673), col = "#ff0000", lty = c(2, 1, 2)) +
  geom_vline(xintercept = c(712, 717, 722), col = "#ff0033", lty = c(2, 1, 2)) +
  geom_vline(xintercept = c(820, 840, 860), col = "darkred", lty = c(2, 1, 2)) +
  labs(x = "Wavelength (nm)",
       y = "Reflectance",
       title = "Reflectance curve for Micasense Calibrated Reflectance Panel RP02-1701230-SC")

ggsave(filename = "figs/rededge-calibration-curve-reflectance-panel.png")

mean_refl <-
  refl %>% 
  group_by(color) %>% 
  summarize(reflectance = mean(reflectance))
