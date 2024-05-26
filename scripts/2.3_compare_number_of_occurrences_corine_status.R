##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.3_corine_change_layer_and_ssb_grids
# This script contains code which compares the number of occurrence records 
# between CORINE pixels that are (not)changing and SSB IDs
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

# 2. COMPARE THE NUMBER OF OCCURRENCES BETWEEN CHANGED AND UNCHANGED PIXELS ----

## 2.1. 2000-2006 ----

### 2.1.1. Before change (1997-2000) for biodiversity records ----

# Prepare data for the period 1997-2000
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    num_records_1997_2000 = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2000.2006 <- occ_df_before_2000.2006_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2000.2006 <- ssb_ids_with_both_before_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2000.2006 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2000.2006 <- occ_df_before_2000.2006_records |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2000.2006$land_cover2000 <- factor(filtered_occ_records_before_2000.2006$land_cover2000,
                                              levels = names(land_cover_mapping),
                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2000.2006 <- filtered_occ_records_before_2000.2006 |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2000.2006, 
       aes(x = cover_change, y = num_records_1997_2000, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (1997-2000)") +
  theme_minimal()

### 2.1.2. After change (2006-2009) for biodiversity records ----

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2000.2006_records <- occ_df_after_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2006_2009 = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2000.2006 <- occ_df_after_2000.2006_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2000.2006 <- ssb_ids_with_both_after_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2000.2006 <- ssbid_counts_after_2000.2006 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2000.2006 <- occ_df_after_2000.2006_records |>
  filter(SSBid %in% top_ssbids_after_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2000.2006$land_cover2000 <- factor(filtered_occ_records_after_2000.2006$land_cover2000,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2000.2006 <- filtered_occ_records_after_2000.2006 |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2000.2006, 
       aes(x = cover_change, y = num_records_2006_2009, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (1997-2000)") +
  theme_minimal()

## 2.2. 2006-2012 ----

### 2.2.1. Before change (2003-2006) for biodiversity records ----

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2003_2006 = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2006.2012 <- occ_df_before_2006.2012_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2006.2012 <- ssb_ids_with_both_before_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2006.2012 <- ssbid_counts_before_2006.2012 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2006.2012 <- occ_df_before_2006.2012_records |>
  filter(SSBid %in% top_ssbids_before_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2006.2012$land_cover2006 <- factor(filtered_occ_records_before_2006.2012$land_cover2006,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2006.2012 <- filtered_occ_records_before_2006.2012 |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2006.2012, 
       aes(x = cover_change, y = num_records_2003_2006, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2003-2006)") +
  theme_minimal()

### 2.2.2. After change (2012-2015) for biodiversity records ----

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2006.2012_records <- occ_df_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2012_2015 = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2006.2012 <- occ_df_after_2006.2012_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2006.2012 <- ssb_ids_with_both_after_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2006.2012 <- ssbid_counts_after_2006.2012 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2006.2012 <- occ_df_after_2006.2012_records |>
  filter(SSBid %in% top_ssbids_after_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2006.2012$land_cover2006 <- factor(filtered_occ_records_after_2006.2012$land_cover2006,
                                                              levels = names(land_cover_mapping),
                                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2006.2012 <- filtered_occ_records_after_2006.2012 |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2006.2012, 
       aes(x = cover_change, y = num_records_2012_2015, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2012-2015)") +
  theme_minimal()

## 2.3. 2012-2018 ----

### 2.3.1. Before change (2009-2012) for biodiversity records ----

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2009_2012 = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2012.2018 <- occ_df_before_2012.2018_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2012.2018 <- ssb_ids_with_both_before_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2012.2018 <- ssbid_counts_before_2012.2018 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2012.2018 <- occ_df_before_2012.2018_records |>
  filter(SSBid %in% top_ssbids_before_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2012.2018$land_cover2012 <- factor(filtered_occ_records_before_2012.2018$land_cover2012,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2012.2018 <- filtered_occ_records_before_2012.2018 |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2012.2018, 
       aes(x = cover_change, y = num_records_2009_2012, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2009-2012)") +
  theme_minimal()

### 2.3.2. After change (2015-2018) for biodiversity records ----

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2012.2018_records <- occ_df_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2015_2018 = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2012.2018 <- occ_df_after_2012.2018_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2012.2018 <- ssb_ids_with_both_after_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2012.2018 <- ssbid_counts_after_2012.2018 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2012.2018 <- occ_df_after_2012.2018_records |>
  filter(SSBid %in% top_ssbids_after_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2012.2018$land_cover2012 <- factor(filtered_occ_records_after_2012.2018$land_cover2012,
                                                              levels = names(land_cover_mapping),
                                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2012.2018 <- filtered_occ_records_after_2012.2018 |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2012.2018, 
       aes(x = cover_change, y = num_records_2015_2018, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2015-2018)") +
  theme_minimal()
