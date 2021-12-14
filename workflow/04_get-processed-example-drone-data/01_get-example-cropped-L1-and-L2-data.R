# Purpose: get all the data files that will be necessary for intergrating with
# NEON field and AOP data
# Users may wish to start from this point to download the data that have
# already been through the structure from motion workflow

dependencies <- c("osfr")
need_install <- dependencies[!sapply(dependencies, require, character.only = TRUE)]
install.packages(need_install, character.only = TRUE)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

# if on macOS, get xquartz to visualize point clouds----------------------

# https://www.xquartz.org/
# https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg

# create directory to store L1 products -----------------------------------

if(!dir.exists(file.path("data", "drone", "L1", site_name, flight_datetime))) {
  dir.create(file.path("data", "drone", "L1", site_name, flight_datetime),
             recursive = TRUE)
}

# files to be downloaded from remote and named
cropped_ortho_fname <- file.path("data", "drone", "L2", "radiometric-corrections", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_ortho_cropped.tif"))
cropped_dsm_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dsm_cropped.tif"))
cropped_dense_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las"))
cropped_sparse_point_cloud_fname <- file.path("data", "drone", "L1", site_name, flight_datetime, paste0(site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las"))

# these data are permanently archived on the Open Science Framework
cropped_ortho_rgb_osf_id <- osfr::osf_retrieve_file(id = "x57dk")
cropped_ortho_osf_id <- osfr::osf_retrieve_file(id = "kh7vu")
cropped_dsm_osf_id <- osfr::osf_retrieve_file(id = "zdext")
cropped_dense_point_cloud_osf_id <- osfr::osf_retrieve_file(id = "eg2sn")
cropped_sparse_point_cloud_osf_id <- osfr::osf_retrieve_file(id = "7pjmc")

# try to retrieve data from the permanently-archived versions on Open Science
# Framework
# orthomosaic -------------------------------------------------------------
if(!file.exists(cropped_ortho_fname)) {
  osfr::osf_download(x = cropped_ortho_osf_id, 
                     path = dirname(cropped_ortho_fname))
}

# digital surface model ---------------------------------------------------
if(!file.exists(cropped_dsm_fname)) {
  osfr::osf_download(x = cropped_dsm_osf_id, 
                     path = dirname(cropped_dsm_fname))
}

# dense point cloud -------------------------------------------------------
if(!file.exists(cropped_dense_point_cloud_fname)) {
  osfr::osf_download(x = cropped_dense_point_cloud_osf_id, 
                     path =  dirname(cropped_dense_point_cloud_fname))
}

# sparse point cloud ------------------------------------------------------
if(!file.exists(cropped_sparse_point_cloud_fname)) {
  osfr::osf_download(x = cropped_sparse_point_cloud_osf_id, 
                     path = dirname(cropped_sparse_point_cloud_fname))
}



# If Open Science Framework doesn't work, you can see whether they are
# still available via the CU Boulder Earth Lab Amazon S3 bucket. But they
# might have been purged, depending on how much time has passed! If those
# two sources fail (OSF and AWS), you can email mikoontz <at> gmail <dot> com
# to request direct access

# remote files (these are the non-permanently-archived versions on an Earth Lab AWS S3 bucket)
cropped_ortho_url <- paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/", site_name, "_", flight_datetime, "_ortho_cropped.tif")
cropped_dsm_url <- paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/", site_name, "_", flight_datetime, "_dsm_cropped.tif")
cropped_dense_point_cloud_url <- paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/", site_name, "_", flight_datetime, "_dense-point-cloud_cropped.las")
cropped_sparse_point_cloud_url <- paste0("https://earthlab-mkoontz.s3-us-west-2.amazonaws.com/neon-drone-workflow/", site_name, "_", flight_datetime, "_sparse-point-cloud_cropped.las")


# get L1 and L2 products from S3 -------------------------------------------------

# orthomosaic -------------------------------------------------------------
if(!file.exists(cropped_ortho_fname)) {
  download.file(url = cropped_ortho_url, destfile = cropped_ortho_fname)
}

# digital surface model ---------------------------------------------------
if(!file.exists(cropped_dsm_fname)) {
  download.file(url = cropped_dsm_url, destfile = cropped_dsm_fname)
}

# dense point cloud -------------------------------------------------------
if(!file.exists(cropped_dense_point_cloud_fname)) {
  download.file(url = cropped_dense_point_cloud_url, destfile =  cropped_dense_point_cloud_fname)
}

# sparse point cloud ------------------------------------------------------
if(!file.exists(cropped_sparse_point_cloud_fname)) {
  download.file(url = cropped_sparse_point_cloud_url, destfile = cropped_sparse_point_cloud_fname)
}
