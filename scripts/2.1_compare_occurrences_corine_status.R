##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.1_compare_occurrences_corine_status
# This script compares the number of occurrence records in CORINE land cover
# STATUS pixels that change land cover through time to those that do not change
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(data.table)
library(sf)
library(tidyterra)
library(tidyverse)
library(patchwork)

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

## 2.1. Count records in changed pixels - Method 1 ----

# Identify the pixels that change their land cover between 2000 and 2006
#change_pixels <- corine_status_wgs84[[1]] != corine_status_wgs84[[2]]

# Convert to polygons
#change_pixels_sf <- st_as_sf(as.polygons(change_pixels, dissolve = TRUE))

# Extract records found in the changed pixels
#records_in_changed_pixels <- st_join(occurrences_sp, change_pixels_sf, join = st_intersects)

## 2.2. Count records in changed pixels - Method 2 ----

# Convert occurrences to spatial vector
occurrences_vect <- vect(occurrences_sp)

#Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]

# Assign each cell a unique number
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Change name of layer in ID_raster so it does not duplicate the ones in corine_status_wgs84
ID_raster <- ID_raster |>
  rename(cell_ID = U2006_CLC2000_V2020_20u1)

# Combine the ID raster with the CORINE STATUS raster
corine_ID <- c(corine_status_wgs84, ID_raster)

# Extract raster values for occurrences into dataframe
corine_status_occurrences_df <- terra::extract(corine_ID, occurrences_vect,
                                               df = TRUE)
# Save dataframes to file
write.csv(corine_status_occurrences_df, here("data",
                                             "corine_satus_occurrence_count_df.csv"))


# Add columns from the dataframe with the extracted values
occurrence_corine_df <- bind_cols(occurrences_norway, 
                               select(corine_status_occurrences_df, 2:6))

## 2.3. Plot the number of occurrence records for pixels that have changed land cover between 2000 - 2006 ----

# Add column that indicates change in land cover between 2000 and 2006
occurrence_corine_df <- occurrence_corine_df |>
  rename(land_cover_2000 = U2006_CLC2000_V2020_20u1,
         land_cover_2006 = U2012_CLC2006_V2020_20u1,
         land_cover_2012 = U2018_CLC2012_V2020_20u1,
         land_cover_2018 = U2018_CLC2018_V2020_20u1) |>
  filter(!is.na (land_cover_2000) & !is.na(land_cover_2006)) |>
  mutate(cover_change2000.2006 = if_else (land_cover_2000 == land_cover_2006, "N", "Y"))

# Calculate the number of occurrences in changed and unchanged pixels
occurrence_counts <- occurrence_corine_df |>
  group_by(cell_ID, cover_change2000.2006) |>
  summarise(count = n(), .groups = 'drop')

# Barplot 

## 2.4. Signed rank test ----

# Create 2 separate dfs for changed and unchanged pixels
changed_counts <- occurrence_counts |>
  filter(cover_change2000.2006 == "Y") |>
  pull(count)

unchanged_counts <- occurrence_counts |>
  filter(cover_change2000.2006 == "N") |>
  pull(count)

# Wilcoxon signed-rank test
wilcox_test_result <- wilcox.test(changed_counts, unchanged_counts, 
                                  paired = FALSE)

# Check output
print(wilcox_test_result)







