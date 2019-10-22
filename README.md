# A workflow for doing drone ecology with NEON vegetation structure and AOP data

## Overview

~5 hectares surrounding a 40 x 40m vegetation structure plot was flown on 2019-10-09 @ 1400

## Instrumentation

- DJI Zenmuse X3 12 megapixel RGB camera
- MicaSense Rededge3 five-band multispectral camera

## Flight parameters

- 100m agl, following the terrain as determined by the SRTM 30m product
- Double grid pattern flown
- RGB triggering every 2 seconds; forward flight speed ~3.1 m/s for a forward overlap of 95%
- Side overlap set to 90%
- Rededge3 camera triggering every second; narrower field of view compared to RGB camera translates to XXXX% forward overlap
- Rededge3 camera horizontal field of view narrower than RGB camera, so side overlap translates to XXXX% side overlap

## Data sources

- The high-precision locations of the NEON 40 x 40m plot monuments can be found here: [Geospatial data for NEON TOS plots](https://data.neonscience.org/documents/-/document_library_display/JEygRkSpUBoq/view_file/2480213?_110_INSTANCE_JEygRkSpUBoq_redirect=https%3A%2F%2Fdata.neonscience.org%2Fdocuments%2F-%2Fdocument_library_display%2FJEygRkSpUBoq%2Fview%2F2233450%3F_110_INSTANCE_JEygRkSpUBoq_redirect%3Dhttps%253A%252F%252Fdata.neonscience.org%252Fdocuments%253Fp_p_id%253D110_INSTANCE_JEygRkSpUBoq%2526p_p_lifecycle%253D0%2526p_p_state%253Dnormal%2526p_p_mode%253Dview%2526p_p_col_id%253Dcolumn-1%2526p_p_col_count%253D1)

## Data preparation

1. Fetch the 30m DEM from the Shuttle Radar Topography Mission (SRTM) that covers the whole area of interest for the project.
This can be acquired from earthdata.nasa.gov/, Google Earth Engine, or other free sources. Save it to disk in "data/data_raw/"
2. Fetch the flight logs and save them in "drone/L0/flight-logs/niwo_017/"
3. Use the flight logs to create a polygon representing the survey area.
4. Use the mission area polygon to filter the multispectral images that were taken outside of the mission area (take offs, landings, etc.)
5. Flatten the file structure of the RGB and multispectral imagery.