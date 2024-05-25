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
# occurrences_SSB_land_cover <- ("") # add link here

# Download files
# download.file(occurrences_SSB_land_cover, here("data", "occurrences_SSB_land_cover.rda"))


## 1.2. Load and prepare data ----

# Load data
load(here("data", "occurrences_SSB_land_cover.rda"))

occ_SSB_land_cover <- occurrences_SSB_land_cover.rda


# 2. COUNT OCCURRENCES IN CHANGED AND UNCHANGED PIXELS 2000 - 2006 ----

## 2.1. Compare records in changed and unchanged pixels - Mann-Whitney U test ----

# Add column that indicates change in land cover between 2000 and 2006
occ_SSB_land_cover2000.2006 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover_2000) & !is.na(land_cover_2006)) |>
  mutate(cover_change2000.2006 = if_else (land_cover_2000 == land_cover_2006, "N", "Y"))

# Calculate the number of occurrences in changed and unchanged pixels
occurrence_counts <- occ_SSB_land_cover2000.2006 |>
  group_by(cell_ID, cover_change2000.2006) |>
  summarise(count = n(), .groups = 'drop')

# Create 2 separate dfs for changed and unchanged pixels
changed_counts <- occurrence_counts |>
  filter(cover_change2000.2006 == "Y") |>
  pull(count)

unchanged_counts <- occurrence_counts |>
  filter(cover_change2000.2006 == "N") |>
  pull(count)

# Wilcoxon signed-rank test
wilcox_test_result <- wilcox.test(count, cover_change2000.2006, data = occurrence_counts)

# Check output
print(wilcox_test_result)

# Violing plot






