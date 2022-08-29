library(dplyr)
library(ggplot2)

# read in the figure's source data
# figure source data generated in workflow/06_NEON-AOP-data-integration-with-drone/03_compare-rsr-micasense-neon.R

figure_fname <- file.path("figs", "fig05_relative-spectral-response-rededge3-and-neon-aop.pdf")

neon_micasense_rsr <- 
  read.csv(file = "data/out/relative-spectral-response-rededge3-and-neon-aop.csv") %>% 
  dplyr::as_tibble()

neon_micasense_rsr_zoom <- 
  neon_micasense_rsr %>% 
  dplyr::filter(wavelength_nm > 650 & wavelength_nm < 750) %>% 
  dplyr::filter(relative_spectral_response > 0.01) %>% 
  mutate(source = ifelse(source == "rededge3", yes = band_fullname, no = source),
         source = factor(source, levels = c("NEON", "red", "red edge")))

micasense_rededge3_rsr <-
  neon_micasense_rsr %>% 
  dplyr::filter(source == "rededge3") %>% 
  dplyr::mutate(band_fullname = factor(band_fullname, levels = c("blue", "green", "red", "red edge", "near infrared")))

# https://www.johndcook.com/wavelength_to_RGB.html
# Same color scheme as for Figure 2
band_colors <- c("#00c0ffff", "#c3ff00ff", "#ff0000ff", "#e00000ff", "#B09C85FF")

### The plots for a 2-panel figure
label_df <- data.frame(x = c(475, 560, 668, 717, 840), y = 1.025, label = c("blue", "green", "red", "red edge", "near infrared"))

full_rededge_rsr_gg <- 
  ggplot(data = micasense_rededge3_rsr, aes(x = wavelength_nm, y = relative_spectral_response, color = band_fullname)) +
  geom_line() +
  scale_color_manual(values = band_colors) +
  labs(color = "Band name",
       x = "Wavelength (nm)",
       y = "Relative spectral response") +
  theme_bw() +
  theme(axis.text = element_text(color="black"),
        axis.ticks = element_line(color = "black")) +
  geom_label(data = label_df, mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE, hjust = 0.5, vjust = 0) +
  theme(legend.position = "none") +
  xlim(c(400, 900)) +
  ylim(c(0, 1.05)) +
  geom_segment(aes(x = 650, xend = 650, y = 0, yend = 1), color = "black", lty = 2) +
  geom_segment(aes(x = 750, xend = 750, y = 0, yend = 1), color = "black", lty = 2)

neon_micasense_rsr_gg <-
  ggplot(neon_micasense_rsr_zoom, 
         aes(x = wavelength_nm, y = relative_spectral_response, color = source, group = channel)) + 
  geom_line() +
  scale_color_manual(values = c("black", band_colors[3:4])) +
  theme_bw() +
  labs(x = "Wavelength (nm)",
       y = "Relative spectral response") +
  theme(legend.position = "none",
        axis.text = element_text(color="black"),
        axis.ticks = element_line(color = "black")) +
  geom_label(data = label_df[3:4, ], mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE, hjust = 0.5, vjust = 0) +
  theme(legend.position = "none") + 
  ylim(c(0, 1.05)) +
  geom_segment(aes(x = 650, xend = 650, y = 0, yend = 1), color = "black", lty = 2) +
  geom_segment(aes(x = 750, xend = 750, y = 0, yend = 1), color = "black", lty = 2)

two_panel <- (full_rededge_rsr_gg) / (neon_micasense_rsr_gg)
two_panel <- (two_panel + patchwork::plot_layout(guides = "collect") + patchwork::plot_annotation(tag_levels = "a"))

ggsave(filename = figure_fname, plot = two_panel, width = 180, height = 220, units = "mm")
