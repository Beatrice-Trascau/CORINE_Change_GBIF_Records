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
         land_cover2000 = U2006_CLC2000_V2020_20u1,
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

# 6. SUMMARISE OCCURRENCES BY CELL AND TIME PERIOD -----------------------------

## 6.1. Add time period classification to occurrences --------------------------

# Convert occurrences back to normal df from spatial object
occurrences_df <- occurrences_sf_valid |>
  st_drop_geometry()

# All the before periods (avoids overlap)
occurrences_before <- occurrences_df |>
  filter(year %in% c(2000:2003, 2006:2009, 2012:2015))|>
  mutate(time_period = case_when(year %in% 2000:2003 ~ "before_2000_2006",
                                 year %in% 2006:2009 ~ "before_2006_2012",
                                 year %in% 2012:2015 ~ "before_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# All the after periods (avoids overlap)
occurrences_after <- occurrences_df |>
  filter(year %in% c(2003:2006, 2009:2012, 2015:2018))|>
  mutate(time_period = case_when(year %in% 2003:2006 ~ "after_2000_2006",
                                 year %in% 2009:2012 ~ "after_2006_2012",
                                 year %in% 2015:2018 ~ "after_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# Combine before and after
occurrences_df_periods <- bind_rows(occurrences_before, occurrences_after)

# Summarise occurrences by cell and time period
occurrnces_summary <- occurrences_df_periods |>
  group_by(cell_ID, time_period) |>
  summarise(n_occurrences = n(),
            n_species = length(unique(species)),
            species_list = list(unique(species)),
            .groups = "drop")

# 7. COMBINE ALL DATA USING CELL_ID AS KEY -------------------------------------

## 7.1. Add land cover & SSB ID ------------------------------------------------

# Add land cover data
combined_data <- lc_df

# Add occurrence data
combined_data <- combined_data |>
  left_join(occurrnces_summary, by = "cell_ID")

# Add SSB data
combined_data_SSB <- combined_data  |>
  left_join(ssb_lookup, by = "cell_ID")

# Replace NA values in occurrencce counts with 0
combined_data_SSB <- combined_data_SSB |>
  mutate(n_occurrences = ifelse(is.na(n_occurrences), 0, n_occurrences),
         n_species = ifelse(is.na(n_species), 0, n_species))


## 7.2. Add land cover change classifications ----------------------------------

# Convert numeric land cover codes to their names
combined_data_SSB_lc_names <- combined_data_SSB |>
  mutate(land_cover2000_name = case_when(land_cover2000 == 1 ~ "urban",
                                         land_cover2000 == 80 ~ "complex_agri",
                                         land_cover2000 == 103 ~ "agri_sig_veg",
                                         land_cover2000 == 250 ~ "forests",
                                         land_cover2000 == 380 ~ "moors_heath_grass",
                                         land_cover2000 == 590 ~ "woodland_shrub",
                                         land_cover2000 == 711 ~ "sparse_veg",
                                         is.na(land_cover2000) ~ "other"),
         land_cover2006_name = case_when(land_cover2006 == 1 ~ "urban",
                                         land_cover2006 == 80 ~ "complex_agri",
                                         land_cover2006 == 103 ~ "agri_sig_veg",
                                         land_cover2006 == 250 ~ "forests",
                                         land_cover2006 == 380 ~ "moors_heath_grass",
                                         land_cover2006 == 590 ~ "woodland_shrub",
                                         land_cover2006 == 711 ~ "sparse_veg",
                                         is.na(land_cover2006) ~ "other"),
         land_cover2012_name = case_when(land_cover2012 == 1 ~ "urban",
                                         land_cover2012 == 80 ~ "complex_agri",
                                         land_cover2012 == 103 ~ "agri_sig_veg",
                                         land_cover2012 == 250 ~ "forests", 
                                         land_cover2012 == 380 ~ "moors_heath_grass",
                                         land_cover2012 == 590 ~ "woodland_shrub",
                                         land_cover2012 == 711 ~ "sparse_veg",
                                         is.na(land_cover2012) ~ "other"),
         land_cover2018_name = case_when(land_cover2018 == 1 ~ "urban",
                                         land_cover2018 == 80 ~ "complex_agri",
                                         land_cover2018 == 103 ~ "agri_sig_veg",
                                         land_cover2018 == 250 ~ "forests",
                                         land_cover2018 == 380 ~ "moors_heath_grass", 
                                         land_cover2018 == 590 ~ "woodland_shrub",
                                         land_cover2018 == 711 ~ "sparse_veg",
                                         is.na(land_cover2018) ~ "other"))

# Calculate Y/N land cover change indicators
combined_data_SSB_lc_yn <- combined_data_SSB_lc_names |>
  mutate(cover_change_2000_2006 = ifelse(!is.na(land_cover2000) & !is.na(land_cover2006),
                                         ifelse(land_cover2000 == land_cover2006, "N", "Y"),
                                         NA_character_),
         cover_change_2006_2012 = ifelse(!is.na(land_cover2006) & !is.na(land_cover2012),
                                        ifelse(land_cover2006 == land_cover2012, "N", "Y"), 
                                        NA_character_),
         cover_change_2012_2018 = ifelse(!is.na(land_cover2012) & !is.na(land_cover2018),
                                         ifelse(land_cover2012 == land_cover2018, "N", "Y"),
                                         NA_character_))

# Create transition categories
combined_data_SSB_lc_transitions <- combined_data_SSB_lc_yn |>
  unite("transition_2000_2006", land_cover2000_name, land_cover2006_name, 
        sep = "_to_", remove = FALSE) |>
  unite("transition_2006_2012", land_cover2006_name, land_cover2012_name,
        sep = "_to_", remove = FALSE) |>
  unite("transition_2012_2018", land_cover2012_name, land_cover2018_name,
        sep = "_to_", remove = FALSE)

# Create intensification/extensification categories
combined_data_SSB_lc_intens_extens <- combined_data_SSB_lc_transitions |>
  mutate(change_value_2000_2006 = land_cover2000 - land_cover2006,
         change_value_2006_2012 = land_cover2006 - land_cover2012,
         change_value_2012_2018 = land_cover2012 - land_cover2018,
         intens_extens_2000_2006 = case_when(change_value_2000_2006 %in% c(79, 102, 249, 
                                                                           379, 589, 710,
                                                                           23, 147, 170, 
                                                                           608, 631, -102,
                                                                           -79, 487, 510, 
                                                                           277, 300, -340,
                                                                           -331, -461, -130) ~ "Intensification",
                                             change_value_2000_2006 %in% c(-249, -379, -589, 
                                                                           -170, -300, -510,
                                                                           -147, -277, -487, 
                                                                           130, -210, 461, 
                                                                           331,121, 340, 
                                                                           -23, -710, -121, 
                                                                           -608) ~ "Extensification",
                                             change_value_2000_2006 == 0 ~ "No_change",
                                             TRUE ~ NA_character_),
         intens_extens_2006_2012 = case_when(change_value_2006_2012 %in% c(79, 102, 249, 
                                                                           379, 589, 710,
                                                                           23, 147, 170, 
                                                                           608, 631, -102,
                                                                           -79, 487, 510, 
                                                                           277, 300, -340,
                                                                           -331, -461, -130) ~ "Intensification",
                                             change_value_2006_2012 %in% c(-249, -379, -589, 
                                                                           -170, -300, -510,
                                                                           -147, -277, -487, 
                                                                           130, -210, 461, 
                                                                           331,121, 340, 
                                                                           -23, -710, -121, 
                                                                           -608) ~ "Extensification",
                                             change_value_2006_2012 == 0 ~ "No_change",
                                             TRUE ~ NA_character_),
         intens_extens_2012_2018 = case_when(change_value_2012_2018 %in% c(79, 102, 249, 
                                                                           379, 589, 710,
                                                                           23, 147, 170, 
                                                                           608, 631, -102,
                                                                           -79, 487, 510, 
                                                                           277, 300, -340,
                                                                           -331, -461, -130) ~ "Intensification",
                                             change_value_2012_2018 %in% c(-249, -379, -589, 
                                                                           -170, -300, -510,
                                                                           -147, -277, -487, 
                                                                           130, -210, 461, 
                                                                           331,121, 340, 
                                                                           -23, -710, -121, 
                                                                           -608) ~ "Extensification",
                                             change_value_2012_2018 == 0 ~ "No_change",
                                             TRUE ~ NA_character_))

# Save combined dataset
combined_corine_gbif_ssb_august2025 <- combined_data_SSB_lc_intens_extens
save(combined_corine_gbif_ssb_august2025, 
     file = here("data", "derived_data",
                 "combined_corine_gbif_ssb_august2025.rda"))

# 8. RESTRUCTURE DATA FOR MODELING ---------------------------------------------

## 8.1. Create separate dfs for each period of analysis ------------------------

# Current df has one row per cell with occurrence data for multiple time periods
# For models, we need to have separate before/after observations

# Period 1: 2000-2006
data_2000_2006 <- combined_data_SSB_lc_intens_extens |>
  filter(!is.na(cover_change_2000_2006)) |>
  select(cell_ID, SSBID, 
         land_cover2000, land_cover2006, land_cover2000_name, land_cover2006_name,
         cover_change_2000_2006, transition_2000_2006, intens_extens_2000_2006,
         time_period, n_occurrences, n_species, species_list) |>
  filter(time_period %in% c("before_2000_2006", "after_2000_2006")) |>
  mutate(analysis_period = "2000_2006")

# Period 2: 2006-2012 analysis  
data_2006_2012 <- combined_data_SSB_lc_intens_extens |>
  filter(!is.na(cover_change_2006_2012)) |>
  select(cell_ID, SSBID,
         land_cover2006, land_cover2012, land_cover2006_name, land_cover2012_name, 
         cover_change_2006_2012, transition_2006_2012, intens_extens_2006_2012,
         time_period, n_occurrences, n_species, species_list) |>
  filter(time_period %in% c("before_2006_2012", "after_2006_2012")) |>
  mutate(analysis_period = "2006_2012")

# Period 3: 2012-2018 analysis
data_2012_2018 <- combined_data_SSB_lc_intens_extens |>
  filter(!is.na(cover_change_2012_2018)) |>
  select(cell_ID, SSBID,
         land_cover2012, land_cover2018, land_cover2012_name, land_cover2018_name,
         cover_change_2012_2018, transition_2012_2018, intens_extens_2012_2018, 
         time_period, n_occurrences, n_species, species_list) |>
  filter(time_period %in% c("before_2012_2018", "after_2012_2018")) |>
  mutate(analysis_period = "2012_2018")

## 8.2. Standardise column names across periods --------------------------------

# Rename columns across all period dfs
data_2000_2006 <- data_2000_2006 |>
  rename(land_cover_start = land_cover2000,
         land_cover_end = land_cover2006,
         land_cover_start_name = land_cover2000_name,
         land_cover_end_name = land_cover2006_name,
         cover_change = cover_change_2000_2006,
         transition_type = transition_2000_2006,
         intens_extens = intens_extens_2000_2006)

data_2006_2012 <- data_2006_2012 |>
  rename(land_cover_start = land_cover2006,
         land_cover_end = land_cover2012,
         land_cover_start_name = land_cover2006_name, 
         land_cover_end_name = land_cover2012_name,
         cover_change = cover_change_2006_2012,
         transition_type = transition_2006_2012,
         intens_extens = intens_extens_2006_2012)

data_2012_2018 <- data_2012_2018 |>
  rename(land_cover_start = land_cover2012,
         land_cover_end = land_cover2018,
         land_cover_start_name = land_cover2012_name,
         land_cover_end_name = land_cover2018_name,
         cover_change = cover_change_2012_2018,
         transition_type = transition_2012_2018,
         intens_extens = intens_extens_2012_2018)

## 8.3. Combine all periods to create final dataset ----------------------------

# Combine all period dfs into a single one
final_modeling_data <- bind_rows(data_2000_2006, data_2006_2012, data_2012_2018)

# Convert factors for modeling
modeling_data_combined_corine_gbif_ssb_august2025 <- final_modeling_data |>
  mutate(cell_ID = as.factor(cell_ID),
         SSBID = as.factor(SSBID),
         cover_change = as.factor(cover_change),
         intens_extens = as.factor(intens_extens),
         analysis_period = as.factor(analysis_period),
         time_period = as.factor(time_period))

# Save final dataset
save(modeling_data_combined_corine_gbif_ssb_august2025,
     file = here("data", "derived_data",
                 "modeling_data_combined_corine_gbif_ssb_august2025.rda"))