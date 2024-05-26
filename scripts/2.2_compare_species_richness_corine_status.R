##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.2_corine_change_layer_and_ssb_grids
# This script contains code which compares the number of occurrence records and
# species richness between CORINE pixels that are (not)changing and SSB IDs
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

## 1.1. Download data (if needed) ----

# Add download links
# occurrences_SSB_land_cover <- ("") # add link here

# Download files
# download.file(occurrences_SSB_land_cover, here("data", "occurrences_SSB_land_cover.rda"))


## 1.2. Load and prepare data ----

# Load data
readRDS("/export/beatrimt/CORINE_Change_GBIF_Records/data/occurrences_SSB_land_cover.rds")
load(here("data", "occurrences_SSB_land_cover.rda"))
occ_SSB_land_cover <- occurrences_SSB_land_cover.rda

# Map land cover values to their names
land_cover_mapping <- setNames(c("Urban", "Complex Agriculture", "Agriculture & Natural Vegetation", 
                                 "Forests", "Moors & Grasslands", "Transitional Woodland Shrub", 
                                 "Sparsely Vegetated Areas"), 
                               c(1, 80, 103, 250, 380, 590, 711))

# 2. COMPARE SPECIES RICHNESS BETWEEN CHANGED AND UNCHANGED PIXELS ----

## 2.1. 2000-2006 ----

### 2.1.1. Before change (1997-2000) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_1997_2000 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate species richness for each SSB id for the period 1997-2000 (before change)
occ_df_1997_2000_richness <- occ_df_1997_2000 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_1997_2000 = n_distinct(species),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2000.2006 <- occ_df_1997_2000_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2000.2006 <- ssb_ids_with_both_before_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2000.2006 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2000_2006_richness <- occ_df_1997_2000_richness |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2000_2006_richness$land_cover2000 <- factor(filtered_occ_df_before2000_2006_richness$land_cover2000,
                                                             levels = names(land_cover_mapping),
                                                             labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2000_2006 <- filtered_occ_df_before2000_2006_richness |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2000_2006, 
       aes(x = cover_change, y = species_richness_1997_2000, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.1.2. After change (2006-2009) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2006_2009 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2006_2009_richness <- occ_df_2006_2009 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2006_2009 = n_distinct(species),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2000.2006 <- occ_df_2006_2009_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2000.2006 <- ssb_ids_with_both_after_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2000.2006 <- ssbid_counts_after_2000.2006 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2000_2006_richness <- occ_df_2006_2009_richness |>
  filter(SSBid %in% top_ssbids_after_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2000_2006_richness$land_cover2000 <- factor(filtered_occ_df_after2000_2006_richness$land_cover2000,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2000_2006 <- filtered_occ_df_after2000_2006_richness |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2000_2006, 
       aes(x = cover_change, y = species_richness_2006_2009, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()
