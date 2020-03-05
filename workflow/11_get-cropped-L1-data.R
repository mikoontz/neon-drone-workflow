# Purpose: get all the data files that will be necessary

dependencies <- c("tidyverse", "sf", "raster", "lidR", "stars", "ForestTools")
need_install <- dependencies[!sapply(dependencies, require, character.only = TRUE)]
install.packages(need_install, character.only = TRUE)

library(raster)
library(lidR)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"


# if on macOS, get xquartz to visualize point clouds----------------------

# https://www.xquartz.org/
# https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg

# create directory to store L1 products -----------------------------------

if(!dir.exists(file.path("data", "data_drone", "L1", site_name, flight_datetime))) {
  dir.create(file.path("data", "data_drone", "L1", site_name, flight_datetime), recursive = TRUE)
}

# get L1 products from S3 -------------------------------------------------


# orthomosaic -------------------------------------------------------------

download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_ortho_cropped.tif",
              destfile = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif")),
              method = "curl")

# digital surface model ---------------------------------------------------

download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_dsm_cropped.tif",
              destfile = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif")),
              method = "curl")

# dense point cloud -------------------------------------------------------

download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_dense-point-cloud_cropped.las", 
              destfile =  file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las")),
              method = "curl")


# sparse point cloud ------------------------------------------------------


download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_sparse-point-cloud_cropped.las",
              destfile = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las")),
              method = "curl")
