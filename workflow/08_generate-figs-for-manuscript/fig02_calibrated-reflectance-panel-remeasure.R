# Create figure showing ASD handheld reflectance spectra along with 
# vendor-provided reflectance spectra at the time of panel purchase. 

# Download the spectral data .csv files from the USGS data release:
# https://www.sciencebase.gov/catalog/item/613ba6a0d34e40dd9c0f974f

# Place the .csv files in the data/raw folder.

# ------ Setup ----------------------------------------------------

library(dplyr)
library(ggplot2)
library(cowplot)

# Read the csv files
asd_data <- read.csv("data/raw/handheld-collected_spectral_reflectance_data.csv")
micasense_data <- read.csv("data/raw/vendor-provided_spectral_reflectance_data.csv")

# Specify the panel ID that you would like to create a figure for.
panel_ID <- "RP02-1701230-SC"

# Create directory for output figures
out_dir <- "figs"

# create a folder for the output files if it doesn't already exist
if(!dir.exists(out_dir)){
  dir.create(out_dir)
}

figure_fname <- file.path(out_dir, "fig02_micasense-rededge3-calibrated-reflectance-panel-deterioration.pdf")

# ------ Generate the figure --------------------------------------

# Specify the MicaSense sensor specs to illustrate the spectral bands
micasense_sensor <- "RedEdge"
sensor_specs <- data.frame(band_name = c("blue", "green", "red", "red_edge", "nir"),
                           center_wavelength_nm = c(475, 560, 668, 717, 840),
                           bandwidth_nm = c(20, 20, 10, 10, 40))

# band_colors <- c("blue", "green", "red", "#ff0055", "darkred")

# https://www.johndcook.com/wavelength_to_RGB.html
band_colors <- c("#00c0ffff", "#c3ff00ff", "#ff0000ff", "#e00000ff", "#B09C85FF")

print(paste0(micasense_sensor, "band info: "))
print(sensor_specs)

# reduce the wavelengths of the spectrum prior to plotting based on the spectral range 
min_sensor_wavelength <- sensor_specs$center_wavelength_nm[1] - 40 
max_sensor_wavelength <- sensor_specs$center_wavelength_nm[dim(sensor_specs)[1]] + 40 

# Filter the vendor data for the panel of interest and wavelengths sensed by RedEdge
vendor_spectrum <- micasense_data %>%
  dplyr::filter(panel_id == panel_ID) %>%
  dplyr::filter(wavelength_nm > min_sensor_wavelength) %>%
  dplyr::filter(wavelength_nm < max_sensor_wavelength)


# calculate mean handheld ASD spectrum
mean_asd <- asd_data %>%
  dplyr::filter(panel_id == panel_ID) %>%
  group_by(wavelength_nm) %>%
  summarise(refl = mean(reflectance)) %>% 
  dplyr::filter(wavelength_nm > min_sensor_wavelength) %>%
  dplyr::filter(wavelength_nm < max_sensor_wavelength)

# Plot the mean vendor-provided and handheld ASD spectra 
# along with shaded regions indicating the spectral bands of the MicaSense RedEge
micasense_spectrum_plot <- ggplot2::ggplot() + 
  # Vendor-provided reflectance profile 
  ggplot2::geom_line(data = vendor_spectrum, 
                     aes(x = wavelength_nm, 
                         y = reflectance, 
                         color = "MicaSense, 2017"),
                     size = 1) + 
  # ASD mean reflectance profile 
  ggplot2::geom_line(data = mean_asd, 
                     aes(x = as.numeric(wavelength_nm), 
                         y = as.numeric(refl),
                         color = "ASD, 2020"),
                     size = 1, linetype = "dashed") + 
  theme_bw() + 
  # theme and text parameters
  theme(# move legend inside graph area to save space on border
        legend.position = c(0.21, 0.09),
        axis.text = element_text(color="black"),
        axis.ticks = element_line(color = "black")) +
  labs(x = "Wavelength (nm)",
       y = "Reflectance"
       #,title = "Panel reflectance comparison"
       ) + 
  # extend x axis limits so the label isn't cut off
  xlim(min(mean_asd$wavelength_nm)-5,
       max(mean_asd$wavelength_nm)+30) + 
  # Legend
  scale_color_manual(values = c(
    'MicaSense, 2017' = 'black',
    'ASD, 2020' = 'blue')) +
  labs(color = 'Data source')

micasense_spectrum_plot

# add shading to show the sensitivity of each spectral band 
for(b in 1:length(sensor_specs$band_name)){
  print(b)
  micasense_spectrum_plot <- micasense_spectrum_plot + 
    annotate("rect", xmin = (sensor_specs$center_wavelength_nm[b] -
                               (sensor_specs$bandwidth_nm[b] /2)), 
             xmax = (sensor_specs$center_wavelength_nm[b] + 
                       (sensor_specs$bandwidth_nm[b] /2)), 
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill= band_colors[b])
    
} 

# show the micasense plot 
label_df <- data.frame(x = c(475, 560, 668, 717, 840), y = 0.7, label = c("blue", "green", "red", "red\nedge", "near\ninfrared"))

micasense_spectrum_plot <-
  micasense_spectrum_plot +
  geom_label(data = label_df, mapping = aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 2.5, hjust = 0.5, vjust = 0.5)

# write plot to file 
ggplot2::ggsave(filename = file.path(out_dir, 
                                     paste0("micasense-vs-asd-spectrum_", 
                                            panel_ID, ".png")), 
                plot = micasense_spectrum_plot,
                device = "png",
                width = 6, height = 7, dpi = 600, units = "in")

# Get the panel photo
photo_path <- "figs/panel-photo_cropped.jpeg"

fig02b <- 
  cowplot::ggdraw() +
  cowplot::draw_image(image = photo_path)

fig02b

fig02 <-
  cowplot::plot_grid(micasense_spectrum_plot, fig02b, labels = "auto", rel_widths = c(1.4, 1))

fig02

ggsave(filename = figure_fname, plot = fig02, width = 180, height = 135, units = "mm")
