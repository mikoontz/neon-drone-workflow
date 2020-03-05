# Purpose: get all the data files that will be necessary

library(raster)
library(lidR)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"


# create directory to store L1 products -----------------------------------

if(!dir.exists(file.path("data", "data_drone", "L1", site_name, flight_datetime))) {
  dir.create(file.path("data", "data_drone", "L1", site_name, flight_datetime), recursive = TRUE)
}

# get L1 products from S3 -------------------------------------------------


# orthomosaic -------------------------------------------------------------

cropped_ortho <- raster::brick(x = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_ortho_cropped.tif")

writeRaster(x = cropped_ortho, filename = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif")), overwrite = TRUE)


# digital surface model ---------------------------------------------------

cropped_dsm <- raster::raster(x = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_dsm_cropped.tif")

writeRaster(x = cropped_dsm, filename = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif")), overwrite = TRUE)


# dense point cloud -------------------------------------------------------

download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_dense-point-cloud_cropped.las", 
              destfile =  file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las")),
              method = "curl")


# sparse point cloud ------------------------------------------------------


download.file(url = "https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/niwo_017_2019-10-09_sparse-point-cloud_cropped.las",
              destfile = file.path("data", "data_drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las")),
              method = "curl")
