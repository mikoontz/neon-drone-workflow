# Purpose: We want to assess the degree of overlap in photos from a mission.
# The challenge is that the flight planning app uses camera characteristics
# from the DJI camera to plan missions, but the camera characteristics from
# the multispectral camera are different and produce different overlap than
# the DJI camera. Further, even the overlap for the DJI camera is different
# than how it was planned if a double grid mission were flown or if an "offset"
# was used (where overlap is planned using an altitude X, but the drone is
# flown at an altitude of X + offset. This produces greater overlap on the
# ground than the planned overlap at X).

# the variables named with "planned" refer to the overlap/footprint length/etc.
# at X meters below the camera lens (reflecting the altitude of X used to plan
# the mission). The variables named with "actual" refer to the overlap/footprint
# length/etc. on the ground (reflecting the altitude of X + offset at which
# the drone actually flew.)

# flight characteristics --------------------------------------------------

x3_planned_forward_overlap <- 0.95
x3_planned_side_overlap <- 0.9
agl_m <- 100 
offset_m <- 0

# zenmuse x3 sensor characteristics ---------------------------------------
# https://www.helicomicro.com/wp-content/uploads/2015/09/x5-x5r.pdf

x3_sensor_width_px <- 4000  
x3_sensor_height_px <- 3000

x3_focal_length_mm <- 3.6 # could be 4.8, 12, or 70
x3_sensor_width_mm <- 6.17 # Fixed for Rededge3; sensor width to focal length proportion is equal to swath width to altitude proportion
x3_sensor_height_mm <- 4.55

# Note this is set in the mission planner app and is limited by the write speed 
# of the DJI X3 camera to the SD card. 0.5 is the fastest possible speed that 
# MapPilot can tell the DJI X3 camera to write, and that will only work with a fast 
# SD card
x3_shutter_speed <- 0.5 # 

x3_h_footprint_planned_agl_m <- (x3_sensor_width_mm / x3_focal_length_mm) * agl_m
x3_v_footprint_planned_agl_m <- (x3_sensor_height_mm / x3_focal_length_mm) * agl_m

transect_spacing <- (1 - x3_planned_side_overlap) * x3_h_footprint_planned_agl_m
flight_speed <- (1 - x3_planned_forward_overlap) * x3_v_footprint_planned_agl_m * x3_shutter_speed

x3_h_footprint_actual_agl_m <- (x3_sensor_width_mm / x3_focal_length_mm) * (agl_m + offset_m)
x3_v_footprint_actual_agl_m <- (x3_sensor_height_mm / x3_focal_length_mm) * (agl_m + offset_m)

x3_actual_side_overlap_agl <- 1 - (transect_spacing / x3_h_footprint_actual_agl_m)
x3_actual_forward_overlap_agl <- 1 - (flight_speed / x3_shutter_speed / x3_v_footprint_actual_agl_m)

# rededge sensor characteristics --------------------------------------------------
# From: https://www.usgs.gov/media/images/micasense-rededge-3-specifications

re_sensor_width_mm <- 4.8 # Fixed for Rededge3; sensor width to focal length proportion is equal to swath width to altitude proportion
re_sensor_height_mm <- 3.6
re_sensor_width_px <- 1280
re_sensor_height_px <- 960
re_focal_length_mm <- 5.5

# Note that this shutter speed is set in the web browser interface to the RedEdge
# camera. A shutter speed of 1 image per second is the fastest that the RedEdge
# camera can write.
re_shutter_speed <- 1

re_h_footprint_planned_agl_m <- (re_sensor_width_mm / re_focal_length_mm) * agl_m
re_v_footprint_planned_agl_m <- (re_sensor_height_mm / re_focal_length_mm) * agl_m

re_h_footprint_actual_agl_m <- (re_sensor_width_mm / re_focal_length_mm) * (agl_m + offset_m)
re_v_footprint_actual_agl_m <- (re_sensor_height_mm / re_focal_length_mm) * (agl_m + offset_m)

re_planned_side_overlap_agl <- 1 - (transect_spacing / re_h_footprint_planned_agl_m)
re_planned_forward_overlap_agl <- 1 - (flight_speed / re_shutter_speed / re_v_footprint_planned_agl_m)

re_actual_side_overlap_agl <- 1 - (transect_spacing / re_h_footprint_actual_agl_m)
re_actual_forward_overlap_agl <- 1 - (flight_speed / re_shutter_speed / re_v_footprint_actual_agl_m)

multispec_meta <- read.csv("drone/L0/niwo_017_2019-10-09_multispec-photos_metadata.csv")

