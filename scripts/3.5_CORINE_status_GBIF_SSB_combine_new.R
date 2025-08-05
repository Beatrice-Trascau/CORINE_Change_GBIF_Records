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
drive_download(as_id("1pbI26-2VFHPl1A7LsBZHmexC8tguFtv3"),
               path = here("data", "derived_data", "cleaned_occurrences_feb17_25.rda"))

## 1.2. Read in data -----------------------------------------------------------

# CORINE Status Layers
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# SSB Grid
ssb_grids <- vect(here("data", "raw_data",
                       "SSB050KM", "ssb50km.shp"))

# Cleaned occurrence records
load(here("data", "derived_data","clened_occurrences_redownloaded_August2025.rda"))
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
  filter(year %in% c(1997:2000, 2003:2006, 2009:2012))|>
  mutate(time_period = case_when(year %in% 1997:2000 ~ "before_2000_2006",
                                 year %in% 2003:2006 ~ "before_2006_2012",
                                 year %in% 2009:2012 ~ "before_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# All the after periods (avoids overlap)
occurrences_after <- occurrences_df |>
  filter(year %in% c(2006:2009, 2012:2015, 2018:2021))|>
  mutate(time_period = case_when(year %in% 2006:2009 ~ "after_2000_2006",
                                 year %in% 2012:2015 ~ "after_2006_2012",
                                 year %in% 2018:2021 ~ "after_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# Combine before and after
occurrences_df_periods <- bind_rows(occurrences_before, occurrences_after)

## 6.2. Create complete summary including cells with 0 occurrences -------------

# Create complete grid of all possible cell_ID + time_period combinations
all_cells <- unique(lc_df$cell_ID)
all_time_periods <- c("before_2000_2006", "after_2000_2006",
                      "before_2006_2012", "after_2006_2012", 
                      "before_2012_2018", "after_2012_2018")
complete_grid <- expand_grid(cell_ID = all_cells, 
                             time_period = all_time_periods)

# Calculate occurrences (only for cells that have any)
occurrences_summary_non0 <- occurrences_df_periods |>
  group_by(cell_ID, time_period) |>
  summarise(n_occurrences = n(),
            n_species = length(unique(species)),
            species_list = list(unique(species)),
            kingdom_list = list(unique(kingdom)),
            phylum_list = list(unique(phylum)),
            class_list = list(unique(class)),
            order_list = list(unique(order)),
            family_list = list(unique(family)),
            publisher_list = list(unique(publisher)),
            datasetName_list = list(unique(datasetName)),
            .groups = "drop")

# Join with complete grid to include cells with 0 occurrences
occurrences_summary <- complete_grid |>
  left_join(occurrences_summary_non0, by = c("cell_ID", "time_period")) |>
  mutate(n_occurrences = ifelse(is.na(n_occurrences), 0, n_occurrences),
         n_species = ifelse(is.na(n_species), 0, n_species),
         species_list = ifelse(is.na(species_list), list(character(0)), species_list),
         kingdom_list = ifelse(is.na(kingdom_list), list(character(0)), kingdom_list),
         phylum_list = ifelse(is.na(phylum_list), list(character(0)), phylum_list),
         class_list = ifelse(is.na(class_list), list(character(0)), class_list),
         order_list = ifelse(is.na(order_list), list(character(0)), order_list),
         family_list = ifelse(is.na(family_list), list(character(0)), family_list),
         publisher_list = ifelse(is.na(publisher_list), list(character(0)), publisher_list),
         datasetName_list = ifelse(is.na(datasetName_list), list(character(0)), datasetName_list))

# Check how many cells  have occurrences and how many do not
cat("Cells with zero occurrences:", sum(occurrences_summary$n_occurrences == 0), "\n")
cat("Cells with occurrences:", sum(occurrences_summary$n_occurrences > 0), "\n")

# 7. COMBINE ALL DATA USING CELL_ID AS KEY -------------------------------------

## 7.1. Add land cover & SSB ID ------------------------------------------------

# Add land cover data
combined_data <- lc_df

# Add occurrence data
combined_data <- combined_data |>
  left_join(occurrences_summary, by = "cell_ID")

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
         time_period, n_occurrences, n_species, species_list,
         kingdom_list, phylum_list, class_list, order_list, family_list,
         publisher_list, datasetName_list) |>
  filter(time_period %in% c("before_2000_2006", "after_2000_2006")) |>
  mutate(analysis_period = "2000_2006")

# Period 2: 2006-2012 analysis  
data_2006_2012 <- combined_data_SSB_lc_intens_extens |>
  filter(!is.na(cover_change_2006_2012)) |>
  select(cell_ID, SSBID,
         land_cover2006, land_cover2012, land_cover2006_name, land_cover2012_name, 
         cover_change_2006_2012, transition_2006_2012, intens_extens_2006_2012,
         time_period, n_occurrences, n_species, species_list,
         kingdom_list, phylum_list, class_list, order_list, family_list,
         publisher_list, datasetName_list) |>
  filter(time_period %in% c("before_2006_2012", "after_2006_2012")) |>
  mutate(analysis_period = "2006_2012")

# Period 3: 2012-2018 analysis
data_2012_2018 <- combined_data_SSB_lc_intens_extens |>
  filter(!is.na(cover_change_2012_2018)) |>
  select(cell_ID, SSBID,
         land_cover2012, land_cover2018, land_cover2012_name, land_cover2018_name,
         cover_change_2012_2018, transition_2012_2018, intens_extens_2012_2018, 
         time_period, n_occurrences, n_species, species_list,
         kingdom_list, phylum_list, class_list, order_list, family_list,
         publisher_list, datasetName_list) |>
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

# 9. CHECK DATA WAS CREATED CORRECTLY ------------------------------------------

## 9.1. Check for duplicates in final datasets ---------------------------------

# Check for duplicates in the combined dataset - we expect one row per cell_ID + time_period
combined_duplicates <- combined_corine_gbif_ssb_august2025 |>
  group_by(cell_ID, time_period) |>  
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1)

if(nrow(combined_duplicates) > 0) {
  cat("❌ WARNING: Found", nrow(combined_duplicates), "duplicate cell_ID/time_period combinations in combined dataset!\n")
} else {
  cat("✅ GOOD: No duplicate cell_ID/time_period combinations in combined dataset\n")
} # GOOD: No duplicate cell_ID/time_period combinations in combined dataset

# Check for duplicates in the modeling data
modeling_duplicates <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  group_by(cell_ID, time_period) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1)

if(nrow(modeling_duplicates) > 0) {
  cat("❌ WARNING: Found", nrow(modeling_duplicates), "duplicate cell_ID/time_period combinations in modeling dataset!\n")
  print(head(modeling_duplicates))
} else {
  cat("✅ GOOD: No duplicate cell_ID/time_period combinations in modeling dataset\n")
} # No duplicate cell_ID/time_period combinations in modeling dataset

## 9.2. Check data consistency across each processing step ---------------------

# Check that cell_IDs are consistent across datasets
cells_in_combined <- unique(combined_corine_gbif_ssb_august2025$cell_ID)
cells_in_modeling <- unique(modeling_data_combined_corine_gbif_ssb_august2025$cell_ID)
cells_gained <- setdiff(cells_in_modeling, cells_in_combined)


if(length(cells_gained) > 0) {
  cat("❌ WARNING:", length(cells_gained), "cells in modeling data that weren't in combined data\n")
} else {
  cat("✅ GOOD: No unexpected cells gained in modeling dataset\n")
} #GOOD: No unexpected cells gained in modeling dataset

# Check that land cover values are consistent
lc_check <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  select(cell_ID, land_cover_start, land_cover_end, cover_change) |>
  mutate(calculated_change = ifelse(land_cover_start == land_cover_end, "N", "Y"),
         change_consistent = (calculated_change == cover_change) | is.na(cover_change))

inconsistent_changes <- sum(!lc_check$change_consistent, na.rm = TRUE)
if(inconsistent_changes > 0) {
  cat("❌ WARNING:", inconsistent_changes, "rows with inconsistent land cover change calculations\n")
} else {
  cat("✅ GOOD: All land cover change calculations are consistent\n")
} #GOOD: All land cover change calculations are consistent

## 9.3. Check occurrence data integrity ----------------------------------------

# Check for negative occurrence counts
negative_counts <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(n_occurrences < 0 | n_species < 0)

if(nrow(negative_counts) > 0) {
  cat("❌ WARNING: Found", nrow(negative_counts), "rows with negative occurrence/species counts\n")
} else {
  cat("✅ GOOD: No negative occurrence or species counts\n")
} #GOOD: No negative occurrence or species counts

# Check that n_species <= n_occurrences (you can't have more species than occurrences)
species_count_issues <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(n_species > n_occurrences & n_occurrences > 0)

if(nrow(species_count_issues) > 0) {
  cat("❌ WARNING: Found", nrow(species_count_issues), "rows where n_species > n_occurrences\n")
} else {
  cat("✅ GOOD: Species counts are consistent with occurrence counts\n")
} #GOOD: Species counts are consistent with occurrence counts

# Check for cells with species lists but zero species count
species_list_inconsistency <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(n_species == 0 & !is.na(species_list) & lengths(species_list) > 0)

if(nrow(species_list_inconsistency) > 0) {
  cat("❌ WARNING: Found", nrow(species_list_inconsistency), "rows with species lists but zero species count\n")
} else {
  cat("✅ GOOD: Species lists are consistent with species counts\n")
} #GOOD: Species lists are consistent with species counts

## 9.4. Check spatial data integrity -------------------------------------------

# Check for missing SSB IDs
missing_ssb <- sum(is.na(modeling_data_combined_corine_gbif_ssb_august2025$SSBID))
total_rows <- nrow(modeling_data_combined_corine_gbif_ssb_august2025)
ssb_coverage <- (total_rows - missing_ssb) / total_rows * 100

cat("SSB ID coverage:", sprintf("%.2f%%", ssb_coverage), 
    sprintf("(%d of %d rows)\n", total_rows - missing_ssb, total_rows)) #SSB ID coverage: 100.00% (726136 of 726136 rows)

if(ssb_coverage < 90) {
  cat("❌ WARNING: Low SSB ID coverage - may indicate spatial matching issues\n")
} else {
  cat("✅ GOOD: High SSB ID coverage\n")
} #GOOD: High SSB ID coverage

# Check for impossible cell_ID values (should be between 1 and max valid cells)
max_valid_cells <- length(valid_cells)
invalid_cell_ids <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(as.numeric(as.character(cell_ID)) < 1 | as.numeric(as.character(cell_ID)) > max_valid_cells)

if(nrow(invalid_cell_ids) > 0) {
  cat("❌ WARNING: Found", nrow(invalid_cell_ids), "rows with invalid cell_ID values\n")
} else {
  cat("✅ GOOD: All cell_ID values are within valid range\n")
} #GOOD: All cell_ID values are within valid range

## 9.5. Check time period assignments ------------------------------------------

# Check that each analysis period has both before and after data
time_period_check <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  group_by(analysis_period) |>
  summarise(has_before = any(grepl("before", time_period)),
            has_after = any(grepl("after", time_period)),
            n_rows = n(),
            .groups = "drop")

incomplete_periods <- time_period_check |>
  filter(!has_before | !has_after)

if(nrow(incomplete_periods) > 0) {
  cat("❌ WARNING: Some analysis periods missing before or after data:\n")
  print(incomplete_periods)
} else {
  cat("✅ GOOD: All analysis periods have both before and after data\n")
} #GOOD: All analysis periods have both before and after data

# Show time period distribution
cat("\nTime period distribution:\n")
print(table(modeling_data_combined_corine_gbif_ssb_august2025$time_period, 
            modeling_data_combined_corine_gbif_ssb_august2025$analysis_period))

## 9.6. Check cells with zero occurrences are handled correctly ----------------

# Check that every cell appears in every relevant time period
expected_combinations <- length(unique(combined_corine_gbif_ssb_august2025$cell_ID)) * 6  # 6 time periods
actual_combinations <- nrow(modeling_data_combined_corine_gbif_ssb_august2025)

cat("Expected cell/time_period combinations:", expected_combinations, "\n")
cat("Actual cell/time_period combinations:", actual_combinations, "\n")

if(actual_combinations == expected_combinations) {
  cat("✅ GOOD: All cells appear in all time periods\n")
} else if(actual_combinations < expected_combinations) {
  missing_combinations <- expected_combinations - actual_combinations
  cat("❌ WARNING:", missing_combinations, "missing cell/time_period combinations\n")
} else {
  extra_combinations <- actual_combinations - expected_combinations  
  cat("❌ WARNING:", extra_combinations, "unexpected extra cell/time_period combinations\n")
}

# Check zero occurrence distribution
zero_occ_count <- sum(modeling_data_combined_corine_gbif_ssb_august2025$n_occurrences == 0)
total_rows <- nrow(modeling_data_combined_corine_gbif_ssb_august2025)
zero_percentage <- zero_occ_count / total_rows * 100

cat("Cells with zero occurrences:", zero_occ_count, sprintf("(%.1f%%)\n", zero_percentage))

# This should be a high percentage - most cells don't have occurrence data
if(zero_percentage < 50) {
  cat("❌ WARNING: Unexpectedly low percentage of zero-occurrence cells\n")
  cat("   This might indicate that zero cells weren't properly included\n")
} else {
  cat("✅ GOOD: High percentage of zero-occurrence cells (expected for biodiversity data)\n")
}

# Check that zero cells are distributed across land cover change categories
zero_by_change <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(n_occurrences == 0) |>
  group_by(cover_change) |>
  summarise(count = n(), .groups = "drop")

cat("Zero-occurrence cells by land cover change:\n")
print(zero_by_change)

# Verify that cells with zero occurrences have appropriate species_list structure
zero_species_list_check <- modeling_data_combined_corine_gbif_ssb_august2025 |>
  filter(n_occurrences == 0) |>
  summarise(all_zero_species = all(n_species == 0),
            all_empty_species_lists = all(lengths(species_list) == 0),
            all_empty_kingdom_lists = all(lengths(kingdom_list) == 0),
            all_empty_phylum_lists = all(lengths(phylum_list) == 0),
            all_empty_class_lists = all(lengths(class_list) == 0),
            all_empty_order_lists = all(lengths(order_list) == 0),
            all_empty_family_lists = all(lengths(family_list) == 0),
            all_empty_publisher_lists = all(lengths(publisher_list) == 0),
            all_empty_datasetName_lists = all(lengths(datasetName_list) == 0))

if(all(unlist(zero_species_list_check))) {
  cat("✅ GOOD: Zero-occurrence cells have consistent empty lists for all variables\n")
} else {
  cat("❌ WARNING: Some zero-occurrence cells have inconsistent list data\n")
}

## 9.7. Final summary statistics -----------------------------------------------

cat("\n--- FINAL SUMMARY STATISTICS ---\n")
cat("Combined dataset:\n")
cat("  - Total cells:", nrow(combined_corine_gbif_ssb_august2025), "\n")
cat("  - Cells with occurrences:", sum(combined_corine_gbif_ssb_august2025$n_occurrences > 0, na.rm = TRUE), "\n")
cat("  - Total occurrences:", sum(combined_corine_gbif_ssb_august2025$n_occurrences, na.rm = TRUE), "\n")

cat("\nModeling dataset:\n")
cat("  - Total rows:", nrow(modeling_data_combined_corine_gbif_ssb_august2025), "\n")
cat("  - Unique cells:", length(unique(modeling_data_combined_corine_gbif_ssb_august2025$cell_ID)), "\n")
cat("  - Rows with occurrences:", sum(modeling_data_combined_corine_gbif_ssb_august2025$n_occurrences > 0), "\n")
cat("  - Total occurrences:", sum(modeling_data_combined_corine_gbif_ssb_august2025$n_occurrences), "\n")

# Check occurrence retention from original data
original_occurrences <- nrow(occurrences_sf_valid)
final_occurrences <- sum(modeling_data_combined_corine_gbif_ssb_august2025$n_occurrences)
retention_rate_final <- final_occurrences / original_occurrences * 100

cat("  - Occurrence retention rate:", sprintf("%.2f%%", retention_rate_final), "\n")

if(retention_rate_final < 50) {
  cat("❌ WARNING: Low occurrence retention rate - check time period filters\n")
} else {
  cat("✅ GOOD: Reasonable occurrence retention rate\n")
}

# END OF SCRIPT ----------------------------------------------------------------