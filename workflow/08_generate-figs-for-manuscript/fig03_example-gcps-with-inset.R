library(ggplot2)
library(cowplot)
library(magick)

# read in source data (the hand-modified .png image)
# there are no data that go into generating this figure, so this script is mostly a
# placeholder to show where the specific file is within the data organization scheme
gcp_photo_with_inset_fname <- "figs/fig03_example-gcps-with-inset.pdf"

gcp_photo_with_inset_gg <- 
  cowplot::ggdraw() +
  cowplot::draw_image(image = gcp_photo_with_inset_fname)

gcp_photo_with_inset_gg
