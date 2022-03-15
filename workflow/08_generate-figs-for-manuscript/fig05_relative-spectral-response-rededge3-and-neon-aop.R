library(dplyr)
library(ggplot2)

# read in the figure's source data
# figure source data generated in workflow/06_NEON-AOP-data-integration-with-drone/03_compare-rsr-micasense-neon.R

neon_micasense_rsr <- read.csv(file = "data/out/relative-spectral-response-rededge3-and-neon-aop.csv")

neon_micasense_rsr_zoom <- 
  neon_micasense_rsr %>% 
  dplyr::filter(wavelength_nm > 650 & wavelength_nm < 750) %>% 
  dplyr::filter(relative_spectral_response > 0.01) %>% 
  mutate(source = ifelse(source == "rededge3", yes = band_fullname, no = source),
         source = factor(source, levels = c("NEON", "red", "red edge")))

### The plots for a 2-panel figure
full_redege_rsr_gg <- 
  ggplot(micasense_rededge3_rsr, aes(x = wavelength_nm, y = relative_spectral_response, color = band_fullname)) +
  geom_line() +
  scale_color_manual(values = c("blue", "green", "red", "#ff0055", "darkred")) +
  labs(color = "Band name",
       x = "Wavelength (nm)",
       y = "Relative spectral response") +
  theme_bw()

neon_micasense_rsr_gg <-
  ggplot(neon_micasense_rsr_zoom, 
         aes(x = wavelength_nm, y = relative_spectral_response, color = source, group = channel)) + 
  geom_line() +
  scale_color_manual(values = c("black", "red", "#ff0055")) +
  theme_bw() +
  labs(x = "Wavelength (nm)",
       y = "Relative spectral response") +
  theme(legend.position = "none")


two_panel <- ((full_redege_rsr_gg + geom_vline(xintercept = c(650, 750), lty = 2)) / (neon_micasense_rsr_gg + geom_vline(xintercept = c(650, 750), lty = 2)))
two_panel <- (two_panel + patchwork::plot_layout(guides = "collect") + patchwork::plot_annotation(tag_levels = "a"))

ggsave(filename = file.path("figs", "relative-spectral-response-rededge3-and-neon-aop.png"), plot = two_panel, dpi = 300, width = 180, units = "mm")