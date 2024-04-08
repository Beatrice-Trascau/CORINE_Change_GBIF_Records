##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_corine_change_layer_and_ssb_grids
# This script contains code which combines the CORINE land cover STATUS layers
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
# norway_corine_status_modified_stack <- ("https://ntnu.box.com/shared/static/z1751qp8epnqkmjs8mex9vdl29tjjr5i.tif")
# ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")
# occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")

# Download files
# download.file(norway_corine_status_modified_stack, here("data", 
# "norway_corine_status_modified_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
# "ruter500m_Norge.zip"))

# download.file(cleaned_occurrences, here("data","cleaned_occurrences.txt"))

# Read in the data
norway_corine_status_modified_stack <- rast(here("data", 
                                                 "norway_corine_status_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data", "ruter500m_Norge.shp"))

occurrences_norway <- fread(here("data", "cleaned_occurrences.txt"))

# 2. ADD GBIF OCCURRENCE RECORDS ----

## 2.1. Re-project CORINE STATUS and SSB grids to match the occurrences ----

# Check projections
crs(norway_corine_status_modified_stack, proj = TRUE) #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(ssb_grids, proj = TRUE) #"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

# Re-project SSB grids to match the projection of occurrences
norway_ssb_grids <- terra::project(ssb_grids,
                                   "+proj=longlat +datum=WGS84 +no_defs")

# Re-project CORINE STATUS to match the projection of occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, 
                        "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Check that projections match
crs(norway_ssb_grids, proj = TRUE) # "+proj=longlat +datum=WGS84 +no_defs"
crs(corine_status_wgs84[[1]], proj = TRUE) #"+proj=longlat +datum=WGS84 +no_defs"

# Check values for reprojected CORINE layer
levels(as.factor(as.data.frame(corine_status_wgs84[[1]])$U2006_CLC2000_V2020_20u1))

## 2.2. Convert occurrences to spatial object ----

# Convert occurrences to spatial dataframe 
occurrences_sp <- st_as_sf(occurrences_norway, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(corine_status_wgs84))

# Convert occurrences to spatial vector
occurrences_vect <- vect(occurrences_sp)

## 2.3. Create additional layer with a unique cell ID for each CORINE STATUS cell ----

#Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]

# Assign each cell a unique number
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Combine the ID raster with the CORINE STATUS raster
corine_ID <- c(corine_status_wgs84, ID_raster)

## 2.4. Extract cell values for each occurrence record ----

# Extract raster values for occurrences and SSB IDs
corine_ID_occurrences <- terra::extract(corine_ID, occurrences_vect)
corine_ID_SSBs <- terra::extract(corine_ID, norway_ssb_grids)

# Add extracted values to spatial dataframe
occurrences_vect$land_cover_2000 <- corine_ID_occurrences[,2]
occurrences_vect$land_cover_2006 <- corine_ID_occurrences[,3]
occurrences_vect$land_cover_2012 <- corine_ID_occurrences[,4]
occurrences_vect$land_cover_2018 <- corine_ID_occurrences[,5]
occurrences_vect$cell_ID <- corine_ID_occurrences[,6]
occurrences_vect$SSB_ID <- corine_ID_SSBs[,2]

# Save spatial dataframe to file
saveRDS(occurrences_vect, here("data", "occurrences_corine_status_vect.rds"))

# Extract raster values for occurrences and SSB IDs into dataframe
corine_status_occurrences_df <- terra::extract(corine_ID, occurrences_vect,
                                            df = TRUE)
corine_status_SSBid_df <- terra::extract(corine_ID, norway_ssb_grids,
                                      df = TRUE)

# Save dataframes to file
write.csv(corine_status_occurrences_df, here("data",
                                             "corine_satus_ID_all_layers_occurrences_df.csv"))

write.csv(corine_status_SSBid_df, here("data",
                                             "corine_satus_ID_all_layers_SSBid_df.csv"))


# 3. COMPARE SPECIES RICHNESS BETWEEN CHANGED AND UNCHNGED PIXELS ----

## 3.1. 2000 - 2006 ----

# Convert occurrences_vect to dataframe
occurrences_df <- as.data.frame(occurrences_vect)

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unneccesary columns, add cover change? column,
occurrences_df_2000_2006 <- occurrences_df |>
  select(V1, gbifID, year, species, land_cover_2000, land_cover_2006) |>
  filter(!is.na (land_cover_2000) & !is.na(land_cover_2006)) |>
  mutate(cover_change = if_else (land_cover_2000 == land_cover_2006, "Y", "N"))














