##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.3_CORINE_GBIF_SSB_compare_richness
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
  
  # Create df of occurrences before land cover change
  occ_df_start <- occ_SSB_land_cover |>
    select(V1, gbifID, year, species, !!sym(period$start_col), !!sym(period$end_col), 
           SSBID, cell_ID) |>
    filter(!is.na(.data[[period$start_col]]) & !is.na(.data[[period$end_col]])) |>
    filter(year %in% period$start_years) |>
    mutate(cover_change = if_else(.data[[period$start_col]] == .data[[period$end_col]], "N", "Y"))
  
  # Calculate species richness for each cell_ID
  occ_df_start_richness <- occ_df_start |>
    group_by(cell_ID) |>
    summarise(
      !!sym(period$start_label) := n_distinct(species),
      land_cover_start = first(.data[[period$start_col]]),
      land_cover_end = first(.data[[period$end_col]]),
      cover_change = first(cover_change),
      SSBID = first(SSBID))
  
  # Identify SSB IDs that have pixels with both cover change Y and N
  ssb_ids_with_both_start <- occ_df_start_richness |>
    group_by(SSBID) |>
    filter(all(c("Y", "N") %in% cover_change)) |>
    ungroup()
  
  # Count unique cell IDs for each SSB ID
  ssbid_counts_start <- ssb_ids_with_both_start |>
    group_by(SSBID) |>
    summarise(unique_corine_count = n_distinct(cell_ID)) |>
    ungroup()
  
  # Select top 5 SSB IDs with most unique cell IDS
  top_ssbids_start <- ssbid_counts_start |>
    arrange(desc(unique_corine_count)) |>
    slice_head(n = 5)
  
  # Filter df to only include the top SSB IDs
  filtered_occ_df_start_richness <- occ_df_start_richness |>
    filter(SSBID %in% top_ssbids_start$SSBID)
  
  # Change land cover values based on mapping defined above
  filtered_occ_df_start_richness$land_cover_start <- 
    factor(filtered_occ_df_start_richness$land_cover_start,
           levels = names(land_cover_mapping),
           labels = land_cover_mapping)
  
  # Subset df based on unique combinations of land cover and SSB ID
  filtered_occ_start <- filtered_occ_df_start_richness |>
    group_by(land_cover_start, SSBID) |>
    filter(n() > 1) |>
    ungroup()
  
  # Plot comparisons in species richness for each combination
  ggplot(filtered_occ_start, 
         aes(x = cover_change, y = .data[[period$start_label]], fill = cover_change)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ land_cover_start + SSBID, scales = "free_y", ncol = 5) +
    labs(x = "Cover Change", y = "Species Richness") +
    theme_minimal() +
    ggtitle(paste("Before Change", period$label)) -> p1
  
  # Save the plot
  ggsave(here("figures", "additional_figures", paste0("violin_plots_before_", period$label, ".png")), plot = p1)
  
  # After change
  occ_df_end <- occ_SSB_land_cover |>
    select(V1, gbifID, year, species, !!sym(period$start_col), !!sym(period$end_col), 
           SSBID, cell_ID) |>
    filter(!is.na(.data[[period$start_col]]) & !is.na(.data[[period$end_col]])) |>
    filter(year %in% period$end_years) |>
    mutate(cover_change = if_else(.data[[period$start_col]] == .data[[period$end_col]], "N", "Y"))
  
  # Calculate species richness for each cell ID for the end period
  occ_df_end_richness <- occ_df_end |>
    group_by(cell_ID) |>
    summarise(
      !!sym(period$end_label) := n_distinct(species),
      land_cover_start = first(.data[[period$start_col]]),
      land_cover_end = first(.data[[period$end_col]]),
      cover_change = first(cover_change),
      SSBID = first(SSBID))
  
  # Identify SSB IDs that have both cover change Y and N
  ssb_ids_with_both_end <- occ_df_end_richness |>
    group_by(SSBID) |>
    filter(all(c("Y", "N") %in% cover_change)) |>
    ungroup()
  
  # Count unique cell IDs for each SSB ID
  ssbid_counts_end <- ssb_ids_with_both_end |>
    group_by(SSBID) |>
    summarise(unique_corine_count = n_distinct(cell_ID)) |>
    ungroup()
  
  # Identify top 5 SSB IDs with the most unique cell IDs
  top_ssbids_end <- ssbid_counts_end |>
    arrange(desc(unique_corine_count)) |>
    slice_head(n = 5)
  
  # Filter the data frame to include only the top SSB IDs
  filtered_occ_df_end_richness <- occ_df_end_richness |>
    filter(SSBID %in% top_ssbids_end$SSBID)
  
  # Change land cover values based on the mapping defined above
  filtered_occ_df_end_richness$land_cover_start <- 
    factor(filtered_occ_df_end_richness$land_cover_start,
           levels = names(land_cover_mapping),
           labels = land_cover_mapping)
  
  # Subset data based on unique combinations of land cover and SSB ID
  filtered_occ_end <- filtered_occ_df_end_richness |>
    group_by(land_cover_start, SSBID) |>
    filter(n() > 1) |>
    ungroup()
  
  # Plot comparisons for each combination
  ggplot(filtered_occ_end, 
         aes(x = cover_change, y = .data[[period$end_label]], fill = cover_change)) +
    geom_violin(trim = FALSE) +
    facet_wrap(~ land_cover_start + SSBID, scales = "free_y", ncol = 5) +
    labs(x = "Cover Change", y = "Species Richness") +
    theme_minimal() +
    ggtitle(paste("After Change", period$label)) -> p2
  
  # Save plot to file
  ggsave(here("figures", "additional_figures", paste0("violin_plots_after_", period$label, ".png")), plot = p2)
}

# END OF SCRIPT ----------------------------------------------------------------