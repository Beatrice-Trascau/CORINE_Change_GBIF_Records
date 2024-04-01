##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_corine_change_layer_and_ssb_grids
# This script contains code which combines the CORINE land cover CHANGE layers
# with the ssb administrative grids for future analyses and GBIF occurrences
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
# occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")

# Download files
# download.file(norway_corine_change_stack, here("data", 
                                 # "norway_corine_change_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
                                  # "ruter500m_Norge.zip"))

# download.file(cleaned_occurrences, here("data","cleaned_occurrences.txt"))

# Read in the data
norway_corine_change_modified_stack <- rast(here("data", 
                                                 "norway_corine_change_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data", "ruter500m_Norge.shp"))

occurrences_norway <- fread(here("data", "cleaned_occurrences.txt"))

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
# SSB_raster <- terra::rasterize(norway_ssb_grids,
#                                norway_corine_change_modified_stack[[1]],
#                                field = "SSBid",
#                                method = "simple")

# Save raster file
# writeRaster(SSB_raster, here("data", "norway_SSB_raster.tif"))

# 3. ADD GBIF RECORDS ----

# Extract cell ID, SSB grid value and land cover value for each occurrence

## 3.1. Convert occurrences to spatial dataframe ----
occurrences_sp <- st_as_sf(occurrences_norway, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(norway_corine_change_modified_stack))

# Convert occurrences to spatial vector
occurrences_vect <- vect(occurrences_sp)

## 3.2. Extract cell information for each occurrence ----

#Create additional layer with unique ID for the CORINE layers
ID_raster <- norway_corine_change_modified_stack[[1]]
values(ID_raster) <- 1:ncell(norway_corine_change_modified_stack[[1]])

# Combine ID raster with CORINE
corine_ID <- c(norway_corine_change_modified_stack[[1]], ID_raster)
corine_ID_all_layers <- c(norway_corine_change_modified_stack, ID_raster)
  
# Extract raster values for occurrences and SSB IDs
corine_ID_occurrences <- terra::extract(corine_ID, occurrences_vect)
corine_ID_SSBs <- terra::extract(corine_ID, norway_ssb_grids)

# Add extracted values to spatial dataframe
occurrences_vect$land_cover <- corine_ID_occurrences[,2]
occurrences_vect$cell_ID <- corine_ID_occurrences[,3]
occurrences_vect$SSBs <- corine_ID_SSBs[,2]

# Write dataframe
saveRDS(occurrences_vect, here("data", "occurrences_vect.rds"))

# Do extraction for all corine layers at a time and make into df
corine_ID_all_layers_occurrences <- terra::extract(corine_ID, occurrences_vect)
saveRDS(corine_ID_all_layers_occurrences, here("data", "corine_ID_all_layers_occurrences.rds"))

corine_ID_all_layers_occurrences_df <- terra::extract(corine_ID, occurrences_vect,
                                                   df =  TRUE)
write.csv(corine_ID_all_layers_occurrences_df, here("data",
                                                    "corine_ID_all_layers_occurrences_df.csv"))