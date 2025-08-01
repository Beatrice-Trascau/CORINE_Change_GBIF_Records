##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.5_CORINE_status_GBIF_SSB_combine_new
# This script contains code which combines the CORINE land cover status layers
# with the SSB administrative grids and GBIF occurrences for future analyses
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

## 1.1. Download data (if needed) ----------------------------------------------

# CORINE Status Layers
drive_download(as_id("1TEUH2UUEXsdT-4eeWGSVornzu1paQjZW"),
               path = here("data", "derived_data", 
                           "norway_corine_status_modified_stack.tif"))

# SSB Grids (zipped)
drive_download(as_id("1_AcppTeFEjQi1lX1-tn9GJONf4dxdxeo"),
               path = here("data", "raw_data", 
                           "SSB050KM.zip"))
# Unzip SSB Grids
unzip(here("data", "raw_data", "SSB050KM.zip"),
      exdir = here("data", "raw_data"))

# Cleaned occurrences
drive_download(as_id("1cOiUATYaZO3eQXGz2BoXpIMtX8hcbDlI"),
               path = here("data", "derived_data", "cleaned_occurrences_feb17_25.rda"))

## 1.2. Read in data -----------------------------------------------------------

# CORINE Status Layers
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# SSB Grid
ssb_grids <- vect(here("data", "raw_data",
                       "SSB050KM", "ssb50km.shp"))

# Cleaned occurrence records
load(here("data", "derived_data","cleaned_occurrences_feb17_25.rda"))
occurrences_norway <- clean_occurrences

# 2. CREATE SPATIAL REFERENCE GRID ---------------------------------------------

# Use first CLC layer as basis for reference grid
reference_grid <- norway_corine_status_modified_stack[[1]]

# Reset all values to NA
reference_grid[] <- NA

# Find all valid cells in the first layer
valid_cells <- which(!is.na(values(norway_corine_status_modified_stack[[1]])))
cat("Total cells in raster:", ncell(reference_grid), "\n")
cat("Valid cells with land cover data:", length(valid_cells), "\n")

# Assign cell IDs to valid cells in the reference grid
reference_grid[valid_cells] <- 1:length(valid_cells)

# Rename the layer of the reference grid
names(reference_grid) <- "cell_id"

# Extract df with cell ids and coordinates
grid_df <- as.data.frame(reference_grid, xy = TRUE) |>
  filter(!is.na(cell_id))

# 3. EXTRACT OCCURRENCES TO REFERENCE GRID -------------------------------------

## 3.1. Convert occurrences to spatial object ----------------------------------

# Convert occurrences to sf
occurrences_sf <- sf::st_as_sf(occurrences_norway,
                           coords = c("decimalLongitude", "decimalLatitude"),
                           crs = 4326)

# Check projections
cat("CORINE CRS:", as.character(crs(reference_grid)), "\n")
cat("Occurrences CRS:", as.character(st_crs(occurrences_sf)), "\n")

# Reproject occurrences to match CORINE grid
occurrences_sf_reprojected <- st_transform(occurrences_sf, crs(reference_grid))

## 3.2. Extract cell IDs for occurrences ---------------------------------------

# Extract cell IDs from the reference grid
occurrences_sf_reprojected$cell_ID <- terra::extract(reference_grid,
                                                     st_coordinates(occurrences_sf_reprojected))[, "cell_id"]

# Remove occurrences outside of valid grid cells
occurrences_sf_valid <- occurrences_sf_reprojected |>
  filter(!is.na(cell_ID))

# Calculate how many occurrences were retained
retention_rate <- nrow(occurrences_sf_valid) / nrow(occurrences_sf_reprojected) * 100
cat("Occurrences assigned to valid grid cells:", nrow(occurrences_sf_valid), 
    sprintf("(%.2f%%)\n", retention_rate)) #5342487 (86.24%)
cat("Unique cells with occurrences:", 
    length(unique(occurrences_sf_valid$cell_ID)), "\n") #505047 

# 4. EXTRACT LAND COVER DATA TO REFERENCE GRID ---------------------------------

# Add reference grid to CLC stack
combined_stack <- c(reference_grid, norway_corine_status_modified_stack)

# Convert the new stack to dataframe
lc_df <- as.data.frame(combined_stack, cells = TRUE) |>
  filter(!is.na(cell_id)) |>
  select(-cell) |>
  rename(cell_ID = cell_id,
         land_cover_2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# 5. EXTRACT SSB GRID DATA TO REFERENCE GRID -----------------------------------

## 5.1. Extract SSB ID for reference grid --------------------------------------

# Convert SSB grid to sf
ssb_grid_sf <- st_as_sf(ssb_grids)

# Check if the SSB grid has a CRS and set it manually if it doesn't
if(is.na(st_crs(ssb_grid_sf))){
  st_crs(ssb_grid_sf) <- crs(reference_grid)
  cat("SSB grid CRS was missing - set to match reference grid\n")
}

# Reproject SSB grid to match reference grid CRS
ssb_grid_reprojected <- st_transform(ssb_grid_sf, crs(reference_grid))

# Convert grid dataframe to sf object
grid_centroids <- st_as_sf(grid_df, coords = c("x", "y"),
                           crs = crs(reference_grid))

# Intersect grid centroids with the SSB polygons
ssb_intersect <- st_intersection(grid_centroids, ssb_grid_reprojected)

# Create lookup table for cell_ID to SSB ID
ssb_lookup <- data.frame(cell_ID = ssb_intersect$cell_id,
                         SSBID = ssb_intersect$SSBID)

## 5.2. Make sure SSB ID was assigned correctly --------------------------------

# Get the number of SSB IDs with multiple cells assigned
cell_id_counts <- table(ssb_lookup$cell_ID)
multiple_assignments <- cell_id_counts[cell_id_counts > 1]

# Check if there are any cells assigned to multiple SSB IDs
if(length(multiple_assignments) > 0){
  cat("Warning:", length(multiple_assignments),
      "cells assigned to multiple SSB IDs\n")
  print(head(multiple_assignments))
} else {
  cat("Good: Each cell assigned to exactly one SSB ID\n")
}
