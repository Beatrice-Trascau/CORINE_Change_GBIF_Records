##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_corine_change_layer_and_ssb_grids
# This script contains code which combines the CORINE land cover CHANGE layers
# with the ssb administrative grids for future analyses
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. LOAD DATA ----

# Add download link from box
# norway_corine_change_stack <- ("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif")
# ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")

# Download files
# download.file(norway_corine_change_stack, here("data", 
                                 # "norway_corine_change_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
                                  # "ruter500m_Norge.zip"))

# Read in the layers
norway_corine_change_modified_stack <- rast(here("data", 
                                                 "norway_corine_change_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data", "ruter500m_Norge.shp")) 

# 2. EXPLORE LAYERS ----
plot(ssb_grids)

## 2.1. Project SSB grids to match CORINE ----

# Check projection of SSB grids
crs(ssb_grids, proj = TRUE) # "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

# Re-project SSB grids to match the projection and extent of CORINE
norway_ssb_grids <- terra::project(ssb_grids,
                                   "epsg:3035")

# Check that projections match
crs(norway_ssb_grids, proj = TRUE) # "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(norway_corine_change_modified_stack[[1]], proj = TRUE) # "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

## 2.2. Combine CORINE layers and SSB Layer ----

# Rasterize SSB grid to CORINE
SSB_raster <- terra::rasterize(norway_ssb_grids,
                               norway_corine_change_modified_stack[[1]],
                               field = "SSBid",
                               method = "simple")


# Save raster file
writeRaster(SSB_raster, here("data", "norway_SSB_raster.tif"))
