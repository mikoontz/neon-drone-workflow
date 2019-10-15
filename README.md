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

## Data preparation

1. Fetch the 30m DEM from the Shuttle Radar Topography Mission (SRTM) that covers the whole area of interest for the project.
This can be acquired from earthdata.nasa.gov/, Google Earth Engine, or other free sources. Save it to disk in "data/data_raw/"
2. Fetch the flight logs and save them in "drone/L0/flight-logs/niwo_017/"
3. Use the flight logs to create a polygon representing the survey area.
4. Use the mission area polygon to filter the multispectral images that were taken outside of the mission area (take offs, landings, etc.)
5. Flatten the file structure of the RGB and multispectral imagery.