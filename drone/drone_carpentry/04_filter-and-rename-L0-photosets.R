# Purpose: clean up the RGB and multispectral photo sets to just include photos taken within the survey area
# and to flatten the overly complicated file structure into filenames that contain all that information but
# without any folder hierarchy.

library(tidyverse)
library(exiftoolr)
library(sf)

site_name <- "niwo_017"
flight_datetime <- "2019-10-09"

rgb_dir <- paste0("drone/L0/originals/", site_name, "/", flight_datetime, "/rgb")
multispec_dir <- paste0("drone/L0/originals/", site_name, "/", flight_datetime, "/multispectral")

target_dir <- paste0("drone/L0/photos/", site_name, "/", flight_datetime)

survey_area <- sf::st_read(paste0("drone/L1/survey-extent/", site_name, "/", flight_datetime, "_site-bounds.geojson"))
dem <- raster::raster(paste0("drone/L1/survey-extent/", site_name, "/", flight_datetime, "_site-dem.tif"))

# Create new directory to store all the photos
# dir.create(paste0("data/data_working/", current_site))
dir.create(target_dir, recursive = TRUE)

# Combine the RGB photos, and append an rgb to the filename
rgb_photo_filepaths <- list.files(rgb_dir, recursive = TRUE, full.names = TRUE, pattern = ".JPG")
rgb_photo_old_names <- list.files(rgb_dir, recursive = TRUE, pattern = ".JPG")
rgb_photo_new_filepaths <- paste0(target_dir, "/rgb_", str_replace_all(string = rgb_photo_old_names, pattern = "/", replacement = "_"))

file.copy(from = rgb_photo_filepaths, to = here::here(rgb_photo_new_filepaths), overwrite = TRUE)

# filter out bad multispectral photos and rename --------------------------

# The RedEdge photos need some extra curation help because the camera was just on a timer and so lots of extra pictures
# were taken (of the takeoff point, while the sUAS was taking off/landing, etc.)

# Before proceeding, add the photos of the calibration panel to a directory below the multispec_dir directory called "calibration/"
# The directory with the calibration photos (you've already manually added these photos to "here(multispec_dir)/calibration/" right?)
multispec_calibration_dir <- list.files(multispec_dir, full.names = TRUE, pattern = "calibration")
multispec_photo_dirs <- list.files(multispec_dir, full.names = TRUE)
multispec_nonCalibration_dirs <- multispec_photo_dirs[multispec_photo_dirs != multispec_calibration_dir]

# The list of all the RedEdge photos' full filenames (including path) that aren't calibration panel photos
multispec_photo_filepaths <- 
  multispec_nonCalibration_dirs %>% 
  purrr::map(function(x) {
    list.files(x, recursive = TRUE, full.names = TRUE, pattern = ".tif")
  })

# The list of all RedEdge photos' filename (no path)
multispec_photo_filepaths <- 
  multispec_nonCalibration_dirs %>% 
  purrr::map(function(x) {
    list.files(x, recursive = TRUE, full.names = TRUE, pattern = ".tif")
  })


# Establish the takeoff point for the rededge mission as the location of the very first photo
# Note this assumes the first photo is taken of the takeoff point, not of the calibration panels
# (which were almost always taken some distance from the takeoff point). Ensure that the first
# photo is of the takeoff point (the plywood, in our case) before proceeding
takeoff_points <-
  multispec_photo_filepaths %>% 
  purrr::map(function(x) {
    x[1] %>% 
      exif_read(tags = c("GPSLongitude", "GPSLatitude", "GPSAltitude", "DateTimeOriginal", "Yaw", "Pitch", "Roll", "PressureAlt")) %>% 
      readr::type_convert() %>% 
      st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)
  })

# Get the DEM elevation for the takeoff point location
takeoff_elev <- 
  takeoff_points %>% 
  purrr::map(function(x) {
    raster::extract(dem, x, method = "bilinear")
  })

# Calculate the offset between what the RedEdge altitude thinks it is and what the DEM thinks it is
takeoff_elev_offset <- purrr::map2(.x = takeoff_elev, .y = takeoff_points, .f = function(.x, .y) {.x - .y$PressureAlt})

# Use the exiftoolsr package to read the metadata of all of the rededge photos
# First filter any files that are of size 0 bytes (these are just bad triggers of the RedEdge and exiftools won't play nicely with them)
# Filter out photos that are missing any of the lat/lon or altitude

exif_data <-
  tibble(dir = multispec_nonCalibration_dirs, takeoff_points, takeoff_elev) %>% 
  purrr::pmap(.f = function(dir, takeoff_points, takeoff_elev) {
    
    top_photo_dir <- str_split(dir, pattern = "/", simplify = TRUE)
    top_photo_dir <- top_photo_dir[, ncol(top_photo_dir)]
    
    tibble(SourceFile = list.files(dir, recursive = TRUE, full.names = TRUE, pattern = ".tif")) %>% 
      dplyr::mutate(multispec_photo_old_names = list.files(dir, recursive = TRUE, pattern = ".tif")) %>% 
      dplyr::mutate(DestFile = paste0(target_dir, "/multispec_", top_photo_dir, "_", str_replace_all(string = multispec_photo_old_names, pattern = "/", replacement = "_"))) %>% 
      dplyr::filter(file.size(SourceFile) > 0) %>% 
      left_join(exif_read(.$SourceFile, tags = c("GPSLongitude", "GPSLatitude", "PressureAlt")), by = "SourceFile") %>% 
      readr::type_convert() %>% 
      dplyr::filter(!is.na(GPSLongitude), !is.na(GPSLatitude), !is.na(PressureAlt)) %>% 
      dplyr::select(SourceFile, DestFile, GPSLongitude, GPSLatitude, PressureAlt) 
  })


# Convert the metadata to a spatial object, calculate the agl (above ground level altitude)
# as the difference between the RedEdge recorded altitude, the DEM altitude at that point
# and the offset between the DEM and the RedEdge altimeter

# Perform 2 checks: 1st, is the drone high enough? Give some wiggle room here and say any photo taken where 
# we calculate the drone was 90m or higher (agl) was within the mission parameters
# 2nd, check to see which photos are within the mission footprint (plus a 5 meter buffer) as defined by the
# flight logs from the RGB camera

# Then, "mission photos" are all photos that meet both of the above criteria

exif_data <-
  exif_data %>%
  map(.f = function(x) {
    st_as_sf(x, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) 
  })

exif_data <-
  map2(.x = exif_data, .y = takeoff_elev_offset, .f = function(.x, .y) { 
    .x %>% 
      mutate(band_num = str_sub(SourceFile, start = (nchar(SourceFile) - 4), end = (nchar(SourceFile) - 4))) %>% 
      mutate(band_name = case_when(band_num == 1 ~ "blue",
                                   band_num == 2 ~ "green",
                                   band_num == 3 ~ "red",
                                   band_num == 4 ~ "nir",
                                   band_num == 5 ~ "re"),
             agl = PressureAlt - raster::extract(x = dem, y = ., method = "bilinear") + .y,
             suas_at_altitude = ifelse(agl > 70, yes = TRUE, no = FALSE),
             suas_within_footprint = st_intersects(x = st_transform(., 3310), y = st_buffer(st_transform(survey_area, 3310), 10), sparse = FALSE)[ , 1],
             mission_photo = suas_at_altitude & suas_within_footprint)}) %>% 
  do.call("rbind", .)

photo_points_gg <- 
  ggplot(exif_data, aes(x = GPSLongitude, y = GPSLatitude, color = mission_photo)) +
  geom_point() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Mission photo?") +
  scale_color_manual(values = c("red", "black"))

ggsave(filename = paste0("figures/", site_name, "_multispec-photo-points.pdf"))

exif_data %>% 
  group_by(band_name) %>% 
  summarize(n = n())


# copy the mission photos to the L0 folder --------------------------------

photos_to_copy <-
  exif_data %>% 
  filter(mission_photo)


# Copy the photos from the working folder to the final photos folder using the new names
file.copy(from = photos_to_copy$SourceFile, to = photos_to_copy$DestFile, overwrite = TRUE)




# Add the calibration panel photos to the final photos directory

cal_exif <- 
  tibble(SourceFile = list.files(multispec_calibration_dir, recursive = TRUE, full.names = TRUE, pattern = ".tif"),
         multispec_old_photo_names = list.files(multispec_calibration_dir, recursive = TRUE, pattern = ".tif"),
         DestFile = paste0(target_dir, "/calibration_multispec_", multispec_old_photo_names)) %>% 
  left_join(exif_read(.$SourceFile, tags = c("GPSLongitude", "GPSLatitude", "PressureAlt")), by = "SourceFile") %>% 
  readr::type_convert() %>% 
  dplyr::select(SourceFile, DestFile, GPSLongitude, GPSLatitude, PressureAlt) %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326, remove = FALSE) 

cal_exif$DestFile[1]


calibration_SourceDir <- paste0(multispec_dir, "/calibration")
calibration_SourceFile <- list.files(calibration_SourceDir, full.names = TRUE)
calibration_SourceNames <- list.files(calibration_SourceDir)
calibration_DestFile <- paste0(target_dir, "/multispec_calibration_", calibration_SourceNames)

file.copy(from = cal_exif$SourceFile, to = cal_exif$DestFile)

# After manually curating the photos, get the list of all photos used for analysis
final_photo_list <- list.files(target_dir)

metadata <- data.frame(site = site_name, flight_datetime = flight_datetime, processed_photos = final_photo_list)

# Add the file size to the metadata
metadata <-
  metadata %>% 
  mutate(file_size_MB = file.size(paste0(target_dir, "/", metadata$processed_photos)) / 1e6)

write_csv(metadata, paste0("drone/L0/", site_name, "_", flight_datetime, "_photos_metadata.csv"))

