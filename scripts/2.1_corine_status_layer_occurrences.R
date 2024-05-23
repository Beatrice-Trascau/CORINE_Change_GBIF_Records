##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.1_corine_status_layer_occurrences
# This script compares the number of occurrence records in CORINE land cover
# STATUS pixels that change land cover through time to those that do not change
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)

# 1. LOAD & PREPARE DATA -----

## 1.1. Download data (if needed) ----

# Add download links
# norway_corine_status_modified_stack <- ("https://ntnu.box.com/shared/static/z1751qp8epnqkmjs8mex9vdl29tjjr5i.tif")
# occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")

# Download files
# download.file(norway_corine_status_modified_stack, here("data", 
# "norway_corine_status_modified_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
# "ruter500m_Norge.zip"))

## 1.2. Load and prepare data ----

# Read in the data
norway_corine_status_modified_stack <- rast(here("data", 
                                                 "norway_corine_status_modified_stack.tif"))
occurrences_norway <- fread(here("data", "cleaned_occurrences.txt"))

# Check projections of CORINE and occurrences
crs(norway_corine_status_modified_stack, proj = TRUE) #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
# projection of occurrence records is "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

# Re-project CORINE STATUS raster to match the projection of the occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, 
                               "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Check if projections match
setequal(crs(corine_status_wgs84, proj = TRUE),
         "+proj=longlat +datum=WGS84 +no_defs") #TRUE

# Check values for reprojected CORINE layer
levels(as.factor(as.data.frame(corine_status_wgs84[[1]])$U2006_CLC2000_V2020_20u1)) #looks correct

# Convert occurrences to spatial dataframe 
occurrences_sp <- st_as_sf(occurrences_norway, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(corine_status_wgs84))

# 2. COUNT OCCURRENCES IN CHANGED AND UNCHANGED PIXELS 2000 - 2006 ----

## 2.1. Count records in changed pixels ----

# Identify the pixels that change their land cover between 2000 and 2006
change_pixels <- corine_status_wgs84[[1]] != corine_status_wgs84[[2]]

# Convert to polygons
change_pixels_sf <- st_as_sf(as.polygons(change_pixels, dissolve = TRUE))

# Extract records found in the changed pixels
records_in_changed_pixels <- st_join(occurrences_sp, change_pixels_sf, join = st_intersects)
