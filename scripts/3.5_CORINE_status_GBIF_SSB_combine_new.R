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