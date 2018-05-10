# Script for clipping raw data; see documentation for full details

library(tidyverse)
library(sf)

# Define paths (as to be run from the notebook.Rmd file)

raw_path <- "../raw/"
src_path <- "../src/"
data_path <- "../data/"

# Define equal area projection

epsg102003 <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
epsg102003$epsg <- 102003

# Read in raw data; filter to NC for script development

print("reading in raw data...")

sovi <- read_sf(paste0(raw_path, "SVI2016_US.shp")) %>%
  rename_all(tolower) %>%
  st_transform(epsg102003)

npls <- read_sf(paste0(raw_path, "superfund_shapefile/superfund_npl.shp")) %>%
  rename_all(tolower)

# Fix bad geometry for Ore Knob Mine, NC

sfc = st_sfc(st_point(c(-81.323889, 36.408611)))
st_geometry(npls[npls$site_id == "0409895",]) <- st_geometry(sfc)

npls <- npls %>%
  st_transform(epsg102003)

cntys <- read_sf(paste0(raw_path, "cb_2017_us_county_20m.shp")) %>%
  rename_all(tolower) %>%
  st_transform(epsg102003)

print("processing data...")

# Clip data

cntys_filt <- cntys %>%
  filter(sapply(st_intersects(cntys, st_buffer(npls, 5000)), 
                function(x) {length(x) != 0}))

sovi_filt <- sovi %>%
  filter(sapply(st_intersects(sovi, st_buffer(cntys_filt, 5000)),
                function(x) {length(x) != 0}))

# Write .shp files to `data`

print("writing data to `data` directory...")

st_write(cntys_filt, paste0(data_path, "counties.shp"))
st_write(sovi_filt, paste0(data_path, "sovi.shp"))
st_write(npls, paste0(data_path, "npl.shp"))

rm(cntys_filt, sovi_filt)
