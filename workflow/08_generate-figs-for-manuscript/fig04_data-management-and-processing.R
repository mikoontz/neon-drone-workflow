library(ggplot2)
library(cowplot)
library(magick)

# read in source data (the hand-modified .png image)
# there are no data that go into generating this figure, so this script is mostly a
# placeholder to show where the specific file is within the data organization scheme
data_management_model_fname <- "figs/fig04_data-management-and-processing.pdf"

data_management_model_gg <- 
  cowplot::ggdraw() +
  cowplot::draw_image(image = data_management_model_fname)

data_management_model_gg
