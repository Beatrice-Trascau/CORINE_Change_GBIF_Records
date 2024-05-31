##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.1_corine_layers_preparation
# This script contains code which loads and prepares the CORINE land cover 
# CHANGE and STATUS layers for further analysis
##----------------------------------------------------------------------------##

# 1. DEFINE FUNCTIONS ----------------------------------------------------------

## 1.1. Function to download files if they do not yet exist --------------------
download_files <- function(urls, filenames, dir = "data/raw_data") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  for (i in seq_along(urls)) {
    file_path <- file.path(dir, filenames[i])
    if (!file.exists(file_path)) {
      download.file(urls[i], file_path)
    }
  }
}

## 1.2. Function to read rasters -----------------------------------------------
read_rasters <- function(filenames, dir = "data/raw_data") {
  rasters <- lapply(filenames, function(x) rast(here(dir, x)))
  return(do.call(c, rasters))
}

## 1.3. Function to crop and mask rasters to Norway ----------------------------
crop_mask_to_norway <- function(raster_stack, norway_shape) {
  return(crop(raster_stack, norway_shape, mask = TRUE))
}

## 1.4. Function to modify classes in the rasters ------------------------------
modify_class_values <- function(raster_stack, class_modifications) {
  modified_stack <- raster_stack
  for (mod in class_modifications) {
    modified_stack <- app(modified_stack, fun = function(x) {
      x[x %in% mod$from] <- mod$to
      return(x)
    })
  }
  return(modified_stack)
}

## 1.5. Define URLs, names and new values for rasters --------------------------

# URLs and Filenames for the CORINE Change Layers
change_urls <- c(
  "https://ntnu.box.com/shared/static/vduuevecunldbrc7jb60jarjts99c4db.tif",
  "https://ntnu.box.com/shared/static/nmn2kguk9ipx0u4a2a8yfvcozdf6g9ij.tif",
  "https://ntnu.box.com/shared/static/pah7ig013inqeepg3gwvfan9w00anitp.tif",
  "https://ntnu.box.com/shared/static/g9grkxsvv20sz48rkbig8f9tb8gennfy.tif",
  "https://ntnu.box.com/shared/static/v51lua6b9fph0k7bmsbbc1g20tkdjqh9.tif",
  "https://ntnu.box.com/shared/static/x7ck0jnagfoxvjxvxf99l9lhknky5xlt.tif")

change_filenames <- c(
  "U2006_CHA0006_00_V2020_20u1.tif",
  "U2006_CHA0006_06_V2020_20u1.tif",
  "U2012_CHA0612_06_V2020_20u1.tif",
  "U2012_CHA0612_12_V2020_20u1.tif",
  "U2018_CHA1218_12_V2020_20u1.tif",
  "U2018_CHA1218_18_V2020_20u1.tif")

# URLs and Filenames for the CORINE Status Layers
status_urls <- c(
  "https://ntnu.box.com/shared/static/ffmbbb89aikwg64tg9ei30c8fnf7chl2.tif",
  "https://ntnu.box.com/shared/static/2x6g9jaov5rex3u0xt3hq9mmy91d63ew.tif",
  "https://ntnu.box.com/shared/static/ut1pcbnj7xgfwv3ptahu5c3krdy24l7d.tif",
  "https://ntnu.box.com/shared/static/iub514rfjnkopg3nu4nc18j4axq5jfon.tif")

status_filenames <- c(
  "U2006_CLC2000_V2020_20u1.tif",
  "U2012_CLC2006_V2020_20u1.tif",
  "U2018_CLC2012_V2020_20u1.tif",
  "U2018_CLC2018_V2020_20u1.tif")

# Changes for classes in the rasters
class_modifications <- list(
  list(from = 1:11, to = 1),
  list(from = c(12, 18, 20), to = 80),
  list(from = 21, to = 103),
  list(from = c(23, 24, 25), to = 250),
  list(from = c(26, 27), to = 380),
  list(from = 29, to = 590),
  list(from = 32, to = 711),
  list(from = c(30, 31, 33, 34, 35, 36, 39, 40, 41, 43, 44, 127, 128), to = NA))

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)
library(geodata)
library(mapview)

# 1. READ IN CORINE LAYERS -----------------------------------------------------

## 1.1. Download layers (if needed) ----
download_files(change_urls, change_filenames)
download_files(status_urls, status_filenames)


## 1.2. Read in  layers --------------------------------------------------------
corine_change_stack <- read_rasters(change_filenames)
corine_status_stack <- read_rasters(status_filenames)

# 2. CUT AND MASK CHANGE LAYERS TO NORWAY --------------------------------------

## 2.1. Download country shapefile ----
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
#Check shapefile
plot(norway)

## 2.2. Re-project Norway shapefile to match projection of CORINE layers -------

# Check projections
crs(norway, proj = TRUE)
crs(corine_change_stack[[1]], proj = TRUE)

# Reproject Norway shapefile to the CORINE layers
norway_corine_projection <- project(norway, crs(corine_change_stack))

# Check projection
crs(norway_corine_projection, proj = TRUE) #projection correct now

## 2.3. Crop and mask CORINE stack to Norway -----------------------------------
norway_corine_change_stack <- crop_mask_to_norway(corine_change_stack, 
                                                  norway_corine_projection)
norway_corine_status_stack <- crop_mask_to_norway(corine_status_stack, 
                                                  norway_corine_projection)

# Save the cropped layers 
terra::writeRaster(norway_corine_change_stack, here("data", "derived_data",
                                                    "norway_corine_change_stack.tif"))
terra::writeRaster(norway_corine_status_stack, here("data", "derived_data",
                                                    "norway_corine_status_stack.tif"), 
                   overwrite = TRUE)

# 3. MODIFY VALUES OF CHANGE LAYERS TO HELP IDENTIFY LAND COVER CHANGES----
#The class codes/values are changed to unique numbers which will help identify 
  # the land cover transitions between years

## 3.1. Change land cover class values ----

# Urban Fabric
# all the urban classes are pooled together, due to their sparse distribution across Norway
norway_corine_change_modified <- app(norway_corine_change_stack,
                                     fun = function(x){x[x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)] <- 1; 
                                     return(x)})

# Complex agricultural patterns
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x %in% c(12, 18, 20)] <- 80; 
                                     return(x)})

# Agriculture and significant natural vegetation
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x == 21] <- 103; 
                                     return(x)})

# Forests
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x %in% c(23, 24, 25)] <- 250; 
                                     return(x)})

# Moors, Heathland & Natural Grassland
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x %in% c(26, 27)] <- 380; 
                                     return(x)})
# Transitional woodland shrub
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x == 29] <- 590; return(x)})

# Sparsely vegetated areas
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x == 32] <- 711; return(x)})

# Other classes
norway_corine_change_modified <- app(norway_corine_change_modified,
                                     fun = function(x){x[x %in% c(30, 31, 33, 34, 35, 36, 39, 40, 41, 43, 44, 127, 128)] <- NA; 
                                     return(x)})

# Save the changed layers 
terra::writeRaster(norway_corine_change_modified, 
                   here("data", "norway_corine_change_modified_stack.tif"),
                   overwrite = TRUE)

# 4. READ IN CORINE STATUS LAYERS ----

## 4.1. Download status layers (if needed) ----

#Add Download link from box
# U2006_CLC2000_V2020_20u1 <- ("https://ntnu.box.com/shared/static/ffmbbb89aikwg64tg9ei30c8fnf7chl2.tif")
# U2012_CLC2006_V2020_20u1 <- ("https://ntnu.box.com/shared/static/2x6g9jaov5rex3u0xt3hq9mmy91d63ew.tif")
# U2018_CLC2012_V2020_20u1 <- ("https://ntnu.box.com/shared/static/ut1pcbnj7xgfwv3ptahu5c3krdy24l7d.tif")
# U2018_CLC2018_V2020_20u1 <- ("https://ntnu.box.com/shared/static/iub514rfjnkopg3nu4nc18j4axq5jfon.tif")
# 
# #Download the files
# download.file(U2006_CLC2000_V2020_20u1, "U2006_CLC2000_V2020_20u1.tif")
# download.file(U2012_CLC2006_V2020_20u1, "U2012_CLC2006_V2020_20u1.tif")
# download.file(U2018_CLC2012_V2020_20u1, "U2018_CLC2012_V2020_20u1.tif")
# download.file(U2018_CLC2018_V2020_20u1, "U2018_CLC2018_V2020_20u1.tif")

## 4.2. Read in status layers ----

# Read in CORINE layers downloaded above
corine_status_2000 <- rast(here("data", "raw_data", "U2006_CLC2000_V2020_20u1.tif"))
corine_status_2006 <- rast(here("data", "raw_data", "U2012_CLC2006_V2020_20u1.tif"))
corine_status_2012 <- rast(here("data", "raw_data", "U2018_CLC2012_V2020_20u1.tif"))
corine_status_2018 <- rast(here("data", "raw_data", "U2018_CLC2018_V2020_20u1.tif"))

# Stack layers into one object
corine_status_stack <- c(corine_status_2000, corine_status_2006,
                         corine_status_2012, corine_status_2018)

# 5. CUT AND MASK STATUS LAYERS TO NORWAY ----

# Crop and mask CORINE STATUS to Norway
norway_corine_status_stack <- crop(corine_status_stack, norway_corine_projection,
                            mask = TRUE)

# Save the cropped status layers
terra::writeRaster(norway_corine_status_stack,
                   here("data", "norway_corine_status_stack.tif"),
                   overwrite = TRUE)

# 6. MODIFY VALUES OF STATUS LAYERS TO HELP IDENTIFY LAND COVER CHANGES ----
#The class codes/values are changed to unique numbers which will help identify the land cover transitions between years
#this will only be done for the Norway stack, as this is the one that will be used for analysis

## 6.1. Change land cover class values ----

# Urban Fabric
# all the urban classes are pooled together, due to their sparse distribution across Norway
norway_corine_status_modified <- app(norway_corine_change_stack,
                                     fun = function(x){x[x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)] <- 1; 
                                     return(x)})

# Complex agricultural patterns
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x %in% c(12, 18, 20)] <- 80; 
                                     return(x)})

# Agriculture and significant natural vegetation
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x == 21] <- 103; 
                                     return(x)})

# Forests
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x %in% c(23, 24, 25)] <- 250; 
                                     return(x)})

# Moors, Heathland & Natural Grassland
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x %in% c(26, 27)] <- 380; 
                                     return(x)})
# Transitional woodland shrub
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x == 29] <- 590; return(x)})

# Sparsely vegetated areas
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x == 32] <- 711; return(x)})

# Other classes
norway_corine_status_modified <- app(norway_corine_status_modified,
                                     fun = function(x){x[x %in% c(30, 31, 33, 34, 35, 36, 39, 40, 41, 43, 44, 127, 128)] <- NA; 
                                     return(x)})

# Save the changed layers 
terra::writeRaster(norway_corine_status_modified, 
                   here("data", "norway_corine_status_modified_stack.tif"),
                   overwrite = TRUE)



# END OF SCRIPT ----

