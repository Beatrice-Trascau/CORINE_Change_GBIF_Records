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

## 2.3. Extract SSB ID for occurrences ----
occurrenes_SSB <- terra::intersect(occurrences_vect, norway_ssb_grids)

## 2.4. Create additional layer with a unique cell ID for each CORINE STATUS cell ----

#Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]

# Assign each cell a unique number
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Combine the ID raster with the CORINE STATUS raster
corine_ID <- c(corine_status_wgs84, ID_raster)

## 2.5. Extract land cover cell values for each occurrence record ----

# Extract raster values for occurrences into dataframe
corine_status_occurrences_df <- terra::extract(corine_ID, occurrenes_SSB,
                                            df = TRUE)
# Save dataframes to file
write.csv(corine_status_occurrences_df, here("data",
                                             "corine_satus_ID_all_layers_occurrences_df.csv"))

## 2.6. Add extracted values to the occurrences ----

# Convert SpatVector to dataframe
occurrence_SSB_df <- as.data.frame(occurrenes_SSB)

# Add columns from the dataframe with the extracted values
occurrence_SSB_df <- bind_cols(occurrence_SSB_df, 
                               select(corine_status_occurrences_df, 2:5))

# For some reason there is a mismatch
nrow(occurrence_SSB_df) #22405844
nrow(corine_status_occurrences_df) #22406267

# Truncate corine_status_occurrences_df to match occurrence_SSB_df - for now
corine_status_occurrences_df_trimmed <- corine_status_occurrences_df[1:nrow(occurrence_SSB_df), 2:6]

# Try binding again
occurrence_SSB_df <- occurrence_SSB_df |>
  mutate(land_cover_2000 = corine_status_occurrences_df_trimmed$U2006_CLC2000_V2020_20u1,
         land_cover_2006 = corine_status_occurrences_df_trimmed$U2012_CLC2006_V2020_20u1,
         land_cover_2012 = corine_status_occurrences_df_trimmed$U2018_CLC2012_V2020_20u1,
         land_cover_2018 = corine_status_occurrences_df_trimmed$U2018_CLC2018_V2020_20u1)

# 3. COMPARE SPECIES RICHNESS BETWEEN CHANGED AND UNCHNGED PIXELS ----

## 3.1. 2000 - 2006 ----

# Convert occurrences_vect to dataframe
occurrences_df <- as.data.frame(occurrences_vect)

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occurrences_df_2000_2006 <- occurrences_df |>
  select(V1, gbifID, year, species, land_cover_2000, land_cover_2006, cell_ID) |>
  filter(!is.na (land_cover_2000) & !is.na(land_cover_2006)) |>
  mutate(cover_change = if_else (land_cover_2000 == land_cover_2006, "N", "Y"))

# Calculate species richness for each cell_ID
occurrences_df_2000_2006_richness <- occurrences_df_2000_2006 |>
  group_by(cell_ID) |>
  summarise(
    species_richness = n_distinct(species),
    land_cover_2000 = first(land_cover_2000),
    land_cover_2006 = first(land_cover_2006),
    cover_change = first(cover_change))


# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occurrences_2000_2006 <- occurrences_df_2000_2006_richness |>
  group_by(land_cover_2000, cell_ID) |>
  filter(n() > 1) |>
  ungroup()


# Plot comparison for each combination
ggplot(filtered_occurrences_2000_2006, 
       aes(x = cover_change, y = species_richness, fill = cover_change)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~ land_cover_2000 + SSB_ID) +
  labs(title = "Species Richness by Cover Change", 
       x = "Cover Change", 
       y = "Average Species Richness") +
  theme_minimal()










