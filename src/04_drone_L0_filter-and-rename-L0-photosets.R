# Purpose: clean up the RGB and multispectral photo sets to just include photos taken within the survey area
# and to flatten the overly complicated file structure into filenames that contain all that information but
# without any folder hierarchy.
# Simultaneously, capture the metadata for the photos and save to a csv file for ready access (instead of relying on
# the slow process of using exiftool)

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

# Because Map Pilot properly tags each photo with the offset in altitude based on the altitude when the first photo is taken,
# there is no need for us to manually calculate this value for each photo
# So we can get the metadata from the copied photos directly and write it to disk

single_photo_meta <-
  list.files(path = here::here(target_dir), full.names = TRUE, pattern = "rgb")[1] %>% 
  exiftoolr::exif_read()

# We want to drop these particular metadata because they are meaningless for our purposes and are *gigantic*
single_photo_meta <-
  single_photo_meta %>% 
  dplyr::select(-ThumbnailImage, -PreviewImage, -ImageUIDList)

rgb_photo_meta <-
  list.files(path = here::here(target_dir), full.names = TRUE, pattern = "rgb") %>% 
  exiftoolr::exif_read(tags = names(single_photo_meta))

rgb_photo_meta <-
  rgb_photo_meta %>% 
  dplyr::mutate(takeoff_photo = ifelse(test = (RelativeAltitude == "+0.00"), yes = TRUE, no = FALSE))

# Calculate the AGL elevation for each photo based on the ground elevation (from the SRTM DEM),
# the lon/lat of each photo, and the GPS altitude of each photo

# Total number of flights: 
total_flights <- sum(rgb_photo_meta$takeoff_photo)

# Create a vector that assigns each photo to its proper flight number
# This can be used to each photo's location to a proper GPS altitude offset based on what the
# GPS said the altitude was at the takeoff location (when the drone is on the ground)
new_flight_idx <- c(which(rgb_photo_meta$takeoff_photo), (nrow(rgb_photo_meta) + 1))
flight_number <- rep(seq_along(1:total_flights), times = new_flight_idx[-1] - new_flight_idx[-length(new_flight_idx)])
rgb_photo_meta <-
  rgb_photo_meta %>% 
  dplyr::mutate(flight_num = flight_number)

# Get the takeoff points (where the speed in all dimensions is 0; originally was based on when relative altitude was 0,
# but this can occur during a mission during flight if drone decends a big hill it could be at the same GPS altitude
# as the takeoff point, but still be in the air)
rgb_takeoff_points <-
  rgb_photo_meta %>% 
  filter(SpeedX == 0, SpeedY == 0, SpeedZ == 0) %>% 
  sf::st_as_sf(coords = c("GPSLongitude", "GPSLatitude"), crs = 4326) %>% 
  split(f = .$flight_num)

# Get the DEM elevation for the takeoff point locations
rgb_takeoff_elevs <- 
  rgb_takeoff_points %>% 
  purrr::map(function(x) {
    raster::extract(dem, x, method = "bilinear")
  })

# Calculate the offset between what the RGB camera thinks its altitude is and what the DEM thinks it is
rgb_takeoff_elev_offsets <- purrr::map2(.x = rgb_takeoff_elevs, .y = rgb_takeoff_points, .f = function(.x, .y) {.x - .y$GPSAltitude})

# Split the RGB metadata into a separate list element for each flight (because the elevation offset from
# the takeoff point will be different for each flight)
rgb_photo_meta_list <-
  rgb_photo_meta %>% 
  split(f = .$flight_num)

# Add the ground elevation based on the DEM (the ground underneath where each photo is taken), the
# elevation offset (difference between altitude of takeoff elevation and altitude of the camera
# according to the GPS at each photo point), and the altitude at each photo point (according to
# camera GPS)
rgb_photo_meta <-
  rgb_photo_meta_list %>% 
  map(.f = function(x) st_as_sf(x, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)) %>% 
  map2(rgb_takeoff_elev_offsets, .f = function(data, takeoff_elev_offset) {
    
    data %>% 
      dplyr::mutate(ground_elev = raster::extract(x = dem, y = ., method = "bilinear"),
                    takeoff_elev_offset = takeoff_elev_offset,
                    agl = GPSAltitude - ground_elev + takeoff_elev_offset)
    
  }) %>% 
  do.call("rbind", .)


write_csv(x = rgb_photo_meta %>% sf::st_drop_geometry(), path = "drone/L0/niwo_017_2019-10-09_rgb-photos_metadata.csv")

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
takeoff_elevs <- 
  takeoff_points %>% 
  purrr::map(function(x) {
    raster::extract(dem, x, method = "bilinear")
  })

# Calculate the offset between what the RedEdge altitude thinks it is and what the DEM thinks it is
takeoff_elev_offsets <- purrr::map2(.x = takeoff_elevs, .y = takeoff_points, .f = function(.x, .y) {.x - .y$PressureAlt})

# Use the exiftoolsr package to read the metadata of all of the rededge photos
# First filter any files that are of size 0 bytes (these are just bad triggers of the RedEdge and exiftools won't play nicely with them)
# Filter out photos that are missing any of the lat/lon or altitude
(start <- Sys.time())
exif_data <-
  tibble(dir = multispec_nonCalibration_dirs, takeoff_point = takeoff_points, takeoff_elev = takeoff_elevs) %>% 
  purrr::pmap(.f = function(dir, takeoff_point, takeoff_elev) {
    
    exif_d <-
      tibble(SourceFile = list.files(dir, recursive = TRUE, full.names = TRUE, pattern = ".tif")) %>% 
      dplyr::filter(file.size(SourceFile) > 0) %>% 
      pull(SourceFile) %>% 
      # read the EXIF metadata from each file
      exiftoolr::exif_read() %>% 
      readr::type_convert() %>% 
      # For the rededge camera, we want to include in the new filename the two directories above the file
      dplyr::mutate(new_FileName = map(str_split(string = Directory, pattern = "/", simplify = FALSE), .f = function(x) paste(rev(rev(x)[1:2]), collapse = "_")),
                    new_FileName = paste0(new_FileName, "_", FileName)) %>% 
      dplyr::mutate(DestFile = paste0(target_dir, "/multispec_", new_FileName)) %>% 
      dplyr::filter(!is.na(GPSLongitude), !is.na(GPSLatitude), !is.na(PressureAlt))
  })
(Sys.time() - start)

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
  map2(.x = exif_data, .y = takeoff_elev_offsets, .f = function(data, takeoff_elev_offset) { 
    data %>% 
      mutate(band_num = str_sub(SourceFile, start = (nchar(SourceFile) - 4), end = (nchar(SourceFile) - 4))) %>% 
      mutate(band_name = case_when(band_num == 1 ~ "blue",
                                   band_num == 2 ~ "green",
                                   band_num == 3 ~ "red",
                                   band_num == 4 ~ "nir",
                                   band_num == 5 ~ "re"),
             ground_elev = raster::extract(x = dem, y = ., method = "bilinear"),
             takeoff_elev_offset = takeoff_elev_offset,
             agl = PressureAlt - ground_elev + takeoff_elev_offset,
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

# Deal with the EXIF metadata that are list columns by pasting together the elements into
# a single character string with elements separated by ", "
photos_to_copy <-
  exif_data %>% 
  sf::st_drop_geometry() %>% 
  filter(mission_photo) %>% 
  dplyr::mutate_if(is.list, .funs = function(x) purrr::map_chr(x, toString))

write_csv(x = photos_to_copy, path = "drone/L0/niwo_017_2019-10-09_multispec-photos_metadata.csv")

# Copy the photos from the working folder to the final photos folder using the new names
file.copy(from = photos_to_copy$SourceFile, to = photos_to_copy$DestFile, overwrite = TRUE)

# Add the calibration panel photos to the final photos directory

cal_exif <-
  list.files(multispec_calibration_dir, recursive = TRUE, full.names = TRUE, pattern = ".tif") %>% 
  # read the EXIF metadata from each file
  exiftoolr::exif_read() %>% 
  readr::type_convert() %>% 
  dplyr::mutate(DestFile = paste0(target_dir, "/calibration_multispec_", FileName)) %>% 
  mutate(band_num = str_sub(SourceFile, start = (nchar(SourceFile) - 4), end = (nchar(SourceFile) - 4))) %>% 
  mutate(band_name = case_when(band_num == 1 ~ "blue",
                               band_num == 2 ~ "green",
                               band_num == 3 ~ "red",
                               band_num == 4 ~ "nir",
                               band_num == 5 ~ "re")) %>% 
  dplyr::mutate_if(is.list, .funs = function(x) purrr::map_chr(x, toString))


write_csv(x = cal_exif, path = "drone/L0/niwo_017_2019-10-09_calibration-multispec-photos_metadata.csv")       
file.copy(from = cal_exif$SourceFile, to = cal_exif$DestFile, overwrite = TRUE)


