##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.2_corine_change_layer_and_ssb_grids
# This script contains code which compares the number of 
# species richness between CORINE pixels that are (not)changing and SSB IDs
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Occurrence data
load(here("data","derived_data", 
          "occurrences_SSB_municipalities_land_cover.rda"))

# 2. PREP DATA FOR ANALYSIS ----------------------------------------------------

# Change column names in the occurrence df
occ_SSB_land_cover <- occurrence_municipalities_df |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# Map land cover values to their names
land_cover_mapping <- setNames(c("Urban", "Complex Agriculture", 
                                 "Agriculture & Natural Vegetation", 
                                 "Forests", "Moors & Grasslands", 
                                 "Transitional Woodland Shrub", 
                                 "Sparsely Vegetated Areas"), 
                               c(1, 80, 103, 250, 380, 590, 711))

# Define time periods and their corresponding columns
time_periods <- list(
  list(
    start_years = 1997:2000, 
    end_years = 2006:2009, 
    start_col = "land_cover2000", 
    end_col = "land_cover2006",
    start_label = "species_richness_1997_2000",
    end_label = "species_richness_2006_2009",
    label = "2000-2006"),
  list(
    start_years = 2003:2006, 
    end_years = 2012:2015, 
    start_col = "land_cover2006", 
    end_col = "land_cover2012",
    start_label = "species_richness_2003_2006",
    end_label = "species_richness_2012_2015",
    label = "2006-2012"),
  list(
    start_years = 2009:2012, 
    end_years = 2015:2018, 
    start_col = "land_cover2012", 
    end_col = "land_cover2018",
    start_label = "species_richness_2009_2012",
    end_label = "species_richness_2015_2018",
    label = "2012-2018"))

# 3. COMPARE SPECIES RICHNESS IN EACH TIME PERIOD ------------------------------

# Set up loop
for (period in time_periods) {
  cat("\nProcessing period:", period$label, "\n")
  
  # Before change
  occ_df_start <- occ_SSB_land_cover |>
    select(V1, gbifID, year, species, !!sym(period$start_col), !!sym(period$end_col), 
           SSBID, cell_ID) |>
    filter(!is.na(.data[[period$start_col]]) & !is.na(.data[[period$end_col]])) |>
    filter(year %in% period$start_years) |>
    mutate(cover_change = if_else(.data[[period$start_col]] == .data[[period$end_col]], "N", "Y"))
  
  occ_df_start_richness <- occ_df_start |>
    group_by(cell_ID) |>
    summarise(
      !!sym(period$start_label) := n_distinct(species),
      land_cover_start = first(.data[[period$start_col]]),
      land_cover_end = first(.data[[period$end_col]]),
      cover_change = first(cover_change),
      SSBID = first(SSBID))
  
  ssb_ids_with_both_start <- occ_df_start_richness |>
    group_by(SSBID) |>
    filter(all(c("Y", "N") %in% cover_change)) |>
    ungroup()
  
  ssbid_counts_start <- ssb_ids_with_both_start |>
    group_by(SSBID) |>
    summarise(unique_corine_count = n_distinct(cell_ID)) |>
    ungroup()
  
  top_ssbids_start <- ssbid_counts_start |>
    arrange(desc(unique_corine_count)) |>
    slice_head(n = 5)
  
  filtered_occ_df_start_richness <- occ_df_start_richness |>
    filter(SSBID %in% top_ssbids_start$SSBID)
  
  filtered_occ_df_start_richness$land_cover_start <- factor(filtered_occ_df_start_richness$land_cover_start,
                                                            levels = names(land_cover_mapping),
                                                            labels = land_cover_mapping)
  
  filtered_occ_start <- filtered_occ_df_start_richness |>
    group_by(land_cover_start, SSBID) |>
    filter(n() > 1) |>
    ungroup()
  
  ggplot(filtered_occ_start, 
         aes(x = cover_change, y = .data[[period$start_label]], fill = cover_change)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ land_cover_start + SSBID, scales = "free_y", ncol = 5) +
    labs(x = "Cover Change", y = "Species Richness") +
    theme_minimal() +
    ggtitle(paste("Before Change", period$label)) -> p1
  
  ggsave(here("figures", "additional_figures", paste0("violin_plots_before_", period$label, ".png")), plot = p1)
  
  # After change
  occ_df_end <- occ_SSB_land_cover |>
    select(V1, gbifID, year, species, !!sym(period$start_col), !!sym(period$end_col), 
           SSBID, cell_ID) |>
    filter(!is.na(.data[[period$start_col]]) & !is.na(.data[[period$end_col]])) |>
    filter(year %in% period$end_years) |>
    mutate(cover_change = if_else(.data[[period$start_col]] == .data[[period$end_col]], "N", "Y"))
  
  occ_df_end_richness <- occ_df_end |>
    group_by(cell_ID) |>
    summarise(
      !!sym(period$end_label) := n_distinct(species),
      land_cover_start = first(.data[[period$start_col]]),
      land_cover_end = first(.data[[period$end_col]]),
      cover_change = first(cover_change),
      SSBID = first(SSBID))
  
  ssb_ids_with_both_end <- occ_df_end_richness |>
    group_by(SSBID) |>
    filter(all(c("Y", "N") %in% cover_change)) |>
    ungroup()
  
  ssbid_counts_end <- ssb_ids_with_both_end |>
    group_by(SSBID) |>
    summarise(unique_corine_count = n_distinct(cell_ID)) |>
    ungroup()
  
  top_ssbids_end <- ssbid_counts_end |>
    arrange(desc(unique_corine_count)) |>
    slice_head(n = 5)
  
  filtered_occ_df_end_richness <- occ_df_end_richness |>
    filter(SSBID %in% top_ssbids_end$SSBID)
  
  filtered_occ_df_end_richness$land_cover_start <- factor(filtered_occ_df_end_richness$land_cover_start,
                                                          levels = names(land_cover_mapping),
                                                          labels = land_cover_mapping)
  
  filtered_occ_end <- filtered_occ_df_end_richness |>
    group_by(land_cover_start, SSBID) |>
    filter(n() > 1) |>
    ungroup()
  
  ggplot(filtered_occ_end, 
         aes(x = cover_change, y = .data[[period$end_label]], fill = cover_change)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ land_cover_start + SSBID, scales = "free_y", ncol = 5) +
    labs(x = "Cover Change", y = "Species Richness") +
    theme_minimal() +
    ggtitle(paste("After Change", period$label)) -> p2
  
  ggsave(here("figures", "additional_figures", paste0("violin_plots_after_", period$label, ".png")), plot = p2)
}







# 2. COMPARE SPECIES RICHNESS BETWEEN CHANGED AND UNCHANGED PIXELS ----

## 2.1. 2000-2006 ----

### 2.1.1. Before change (1997-2000) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_1997_2000 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID) |>
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

## 2.2. 2006-2012 ----

### 2.2.1. Before change (2003-2006) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2003_2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate species richness for each SSB id for the period 2003-2006 (before change)
occ_df_2003_2006_richness <- occ_df_2003_2006 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2003_2006 = n_distinct(species),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2006.2012 <- occ_df_2003_2006_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2006.2012 <- ssb_ids_with_both_before_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2006.2012 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2006_2012_richness <- occ_df_2003_2006_richness |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2006_2012_richness$land_cover2006 <- factor(filtered_occ_df_before2006_2012_richness$land_cover2006,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2006_2012 <- filtered_occ_df_before2006_2012_richness |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2006_2012, 
       aes(x = cover_change, y = species_richness_2003_2006, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.2.2. After change (2012-2015) for biodiversity records ----

# Prep dataframe: subset for 2006-2012, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2012_2015 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2012_2015_richness <- occ_df_2012_2015 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2012_2015 = n_distinct(species),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2006.2012 <- occ_df_2012_2015_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2006.2012 <- ssb_ids_with_both_after_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2006.2012 <- ssbid_counts_after_2006.2012 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2006_2012_richness <- occ_df_2012_2015_richness |>
  filter(SSBid %in% top_ssbids_after_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2006_2012_richness$land_cover2006 <- factor(filtered_occ_df_after2006_2012_richness$land_cover2006,
                                                                 levels = names(land_cover_mapping),
                                                                 labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2006_2012 <- filtered_occ_df_after2006_2012_richness |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2006_2012, 
       aes(x = cover_change, y = species_richness_2012_2015, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

## 2.3. 2012-2018 ----

### 2.3.1. Before change (2009-2012) for biodiversity records ----

# Prep dataframe: subset for 2012-2018, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2009_2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate species richness for each SSB id for the period 2003-2006 (before change)
occ_df_2009_2012_richness <- occ_df_2009_2012 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2009_2012 = n_distinct(species),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2012.2018 <- occ_df_2009_2012_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2012.2018 <- ssb_ids_with_both_before_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2012.2018 <- ssbid_counts_before_2012.2018 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2012_2018_richness <- occ_df_2009_2012_richness |>
  filter(SSBid %in% top_ssbids_before_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2012_2018_richness$land_cover2012 <- factor(filtered_occ_df_before2012_2018_richness$land_cover2012,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2012_2018 <- filtered_occ_df_before2012_2018_richness |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2012_2018, 
       aes(x = cover_change, y = species_richness_2009_2012, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.3.2. After change (2015-2018) for biodiversity records ----

# Prep dataframe: subset for 2012-2018, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2015_2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2015_2018_richness <- occ_df_2015_2018 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2015_2018 = n_distinct(species),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2012.2018 <- occ_df_2015_2018_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2012.2018 <- ssb_ids_with_both_after_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2012.2018 <- ssbid_counts_after_2012.2018 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2012_2018_richness <- occ_df_2015_2018_richness |>
  filter(SSBid %in% top_ssbids_after_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2012_2018_richness$land_cover2012 <- factor(filtered_occ_df_after2012_2018_richness$land_cover2012,
                                                                 levels = names(land_cover_mapping),
                                                                 labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2012_2018 <- filtered_occ_df_after2012_2018_richness |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2012_2018, 
       aes(x = cover_change, y = species_richness_2015_2018, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()
