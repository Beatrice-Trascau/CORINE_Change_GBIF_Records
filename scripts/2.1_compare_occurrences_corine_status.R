##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.2_CORINE_GBIF_SSB_compare_occurrences
# This script compares the number of occurrence records in CORINE land cover
# STATUS pixels that change land cover through time to those that do not change
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Occurrences
load(here("data","derived_data", "occurrences_SSB_municipalities_land_cover.rda"))

# CORINE status layers
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# Norway shapefile
norway <- geodata::gadm(country = "NOR", level = 0, path = tempdir(), version = "latest")

# 2. PREP DATA FOR ANALYSIS ----------------------------------------------------

# Change column names in the occurrence df
occ_SSB_land_cover <- occurrence_municipalities_df |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# Re-project CORINE STATUS to match the projection of occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, 
                               "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# 3. COMPARE THE NUMBER OF OCCURRENCES IN TIME ---------------------------------

# Define periods and corresponding columns
time_periods <- list(
  list(start_col = "land_cover2000", end_col = "land_cover2006", 
       start_years = c(1997, 1998, 1999, 2000), 
       end_years = c(2006, 2007, 2008, 2009), label = "2000-2006"),
  list(start_col = "land_cover2006", end_col = "land_cover2012", 
       start_years = c(2003, 2004, 2005, 2006), 
       end_years = c(2012, 2013, 2014, 2015), label = "2006-2012"),
  list(start_col = "land_cover2012", end_col = "land_cover2018", 
       start_years = c(2009, 2010, 2011, 2012), 
       end_years = c(2015, 2016, 2017, 2018), label = "2012-2018"))

# Define loop that compares the number of occurrences for each period
for (i in seq_along(time_periods)) {
  period <- time_periods[[i]]
  cat("\nProcessing period:", period$label, "\n")
  
  # Filter and mutate data for the current period
  occ_SSB_land_cover_period <- occ_SSB_land_cover |>
    filter(!is.na(.data[[period$start_col]]) & !is.na(.data[[period$end_col]])) |>
    mutate(cover_change = if_else(.data[[period$start_col]] == .data[[period$end_col]], "N", "Y"))
  
  # Calculate occurrences
  occurrence_counts <- occ_SSB_land_cover_period |>
    group_by(cell_ID, cover_change) |>
    summarise(count = n(), .groups = 'drop')
  
  # Separate counts for changed and unchanged pixels
  changed_counts <- occurrence_counts |> filter(cover_change == "Y") |> pull(count)
  unchanged_counts <- occurrence_counts |> filter(cover_change == "N") |> pull(count)
  
  # Wilcoxon signed-rank test
  wilcox_test_result <- wilcox.test(changed_counts, unchanged_counts, paired = FALSE)
  print(wilcox_test_result)
  
  # Create violin plots - for changed pixels
  changed_violins <- occurrence_counts |>
    filter(cover_change == "Y") |>
    ggplot(aes(x = cover_change, y = count, fill = cell_ID)) +
    geom_violin(width = 1.4) +
    geom_boxplot(width = 0.1, color = "#800080", alpha = 0.2) +
    xlab("Land Cover Change") +
    ylab("Number of Occurrences") +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11)
    ) +
    scale_y_continuous(labels = comma) +
    ggtitle(paste("Changed Pixels", period$label))
 
  # Create violin plots - for changed pixels
  unchanged_violins <- occurrence_counts |>
    filter(cover_change == "N") |>
    ggplot(aes(x = cover_change, y = count, fill = cell_ID)) +
    geom_violin(width = 1.4) +
    geom_boxplot(width = 0.1, color = "#800080", alpha = 0.2) +
    xlab("Land Cover Change") +
    ylab("Number of Occurrences") +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11)
    ) +
    scale_y_continuous(labels = comma) +
    ggtitle(paste("Unchanged Pixels", period$label))
  
  # Combine the two violins into a single figure
  violin_plot <- plot_grid(changed_violins, unchanged_violins, ncol = 1)
  
  # Save figure to file
  ggsave(here("figure", "additional_figures", 
              paste0("violin_plots_", period$label, ".png")), 
         plot = violin_plot)
  
  # Create dominant period maps
  occ_before <- occ_SSB_land_cover_period |>
    filter(year %in% !!period$start_years)
  occ_after <- occ_SSB_land_cover_period |>
    filter(year %in% !!period$end_years)
  
  # Summarise the number of occurrences before and after land cover change
  counts_before <- occ_before |>
    group_by(cell_ID) |>
    summarise(occ_before = n())
  counts_after <- occ_after |>
    group_by(cell_ID) |>
    summarise(occ_after = n())
  
  # Combine the counts from before and after into one df
  counts <- full_join(counts_before, counts_after, by = "cell_ID") |>
    replace_na(list(occ_before = 0, occ_after = 0)) |>
    mutate(dominant_period = ifelse(occ_before > occ_after, "before", "after"))
  
  # Create new raster with for the dominant period
  dominant_period_raster <- ID_raster
  
  # Change values to NA so the raster is blank
  values(dominant_period_raster) <- NA
  
  # Assign values to the raster based on which period is dominant
  dominant_period_raster[match(counts$cell_ID, values(ID_raster))] <- 
    ifelse(counts$dominant_period == "before", 1, 2)
  
  # Convert raster to df
  dominant_period_df <- as.data.frame(dominant_period_raster, xy = TRUE, 
                                      na.rm = TRUE) |>
    mutate(dominant_period = factor(ifelse(U2006_CLC2000_V2020_20u1 == 1, 
                                           "before", 
                                           "after"), levels = c("before", "after")))
  
  # Plot the map of dominant period
  map_plot <- ggplot() +
    geom_sf(data = norway, fill = "lightgrey", color = "black") +
    geom_point(data = dominant_period_df, aes(x = x, y = y, color = dominant_period), size = 2) +
    scale_color_manual(values = c("before" = "#800080", "after" = "#FFD700")) +
    coord_sf() +
    labs(x = "Longitude", y = "Latitude", color = "Period") +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    ) +
    ggtitle(paste("Dominant Period Map", period$label))
  
  # Convert map to interactive object with plotly
  interactive_map <- ggplotly(map_plot)
  
  # Save the interactive map to file
  htmlwidgets::saveWidget(interactive_map, 
                          here("figure", "additional_figures", 
                               paste0("dominant_period_map_", 
                                      period$label, ".html")))
}

# END OF SCRIPT ----------------------------------------------------------------