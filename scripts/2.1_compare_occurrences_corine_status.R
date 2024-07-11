##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.1_compare_occurrences_corine_status
# This script compares the number of occurrence records in CORINE land cover
# STATUS pixels that change land cover through time to those that do not change
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----

# Load data
load(here("data","derived_data", "occurrences_SSB_municipalities_land_cover.rda"))
occ_SSB_land_cover <- occurrence_municipalities_df |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# Re-project CORINE STATUS to match the projection of occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Define periods and corresponding columns
time_periods <- list(
  list(start_col = "land_cover2000", end_col = "land_cover2006", start_years = c(1997, 1998, 1999, 2000), end_years = c(2006, 2007, 2008, 2009), label = "2000-2006"),
  list(start_col = "land_cover2006", end_col = "land_cover2012", start_years = c(2003, 2004, 2005, 2006), end_years = c(2012, 2013, 2014, 2015), label = "2006-2012"),
  list(start_col = "land_cover2012", end_col = "land_cover2018", start_years = c(2009, 2010, 2011, 2012), end_years = c(2015, 2016, 2017, 2018), label = "2012-2018")
)

# 2. PROCESS EACH PERIOD ----

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
  
  # Create violin plots
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
  
  violin_plot <- plot_grid(changed_violins, unchanged_violins, ncol = 1)
  ggsave(here("figure", "additional_figures", 
              paste0("violin_plots_", period$label, ".png")), 
         plot = violin_plot)
  
  # Create dominant period maps
  occ_before <- occ_SSB_land_cover_period |>
    filter(year %in% !!period$start_years)
  occ_after <- occ_SSB_land_cover_period |>
    filter(year %in% !!period$end_years)
  
  counts_before <- occ_before |>
    group_by(cell_ID) |>
    summarise(occ_before = n())
  counts_after <- occ_after |>
    group_by(cell_ID) |>
    summarise(occ_after = n())
  
  counts <- full_join(counts_before, counts_after, by = "cell_ID") |>
    replace_na(list(occ_before = 0, occ_after = 0)) |>
    mutate(dominant_period = ifelse(occ_before > occ_after, "before", "after"))
  
  dominant_period_raster <- ID_raster
  values(dominant_period_raster) <- NA
  dominant_period_raster[match(counts$cell_ID, values(ID_raster))] <- ifelse(counts$dominant_period == "before", 1, 2)
  
  dominant_period_df <- as.data.frame(dominant_period_raster, xy = TRUE, na.rm = TRUE) |>
    mutate(dominant_period = factor(ifelse(U2006_CLC2000_V2020_20u1 == 1, 
                                           "before", 
                                           "after"), levels = c("before", "after")))
  
  norway <- geodata::gadm(country = "NOR", level = 0, path = tempdir(), version = "latest")
  
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
  
  interactive_map <- ggplotly(map_plot)
  htmlwidgets::saveWidget(interactive_map, 
                          here("figure", "additional_figures", 
                               paste0("dominant_period_map_", 
                                      period$label, ".html")))
}

# 2. MAIN SCRIPT ----

--------------------------------------------------------------------------------
# 0. PACKAGES ----
library(here)
library(terra)
library(data.table)
library(sf)
library(tidyterra)
library(tidyverse)
library(patchwork)
library(styler)
library(scales)
library(cowplot)
library(plotly)

# 1. LOAD & PREPARE DATA -----

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

# 2. COUNT OCCURRENCES IN CHANGED AND UNCHANGED PIXELS 2000 - 2006 ----

## 2.1. Compare records in changed and unchanged pixels - Mann-Whitney U test ----

# Add column that indicates change in land cover between 2000 and 2006
occ_SSB_land_cover2000.2006 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover2000) & !is.na(land_cover2006)) |>
  mutate(cover_change2000.2006 = if_else (land_cover2000 == land_cover2006, "N", "Y"))

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
wilcox_test_result <- wilcox.test(changed_counts, unchanged_counts, paired = FALSE)

# Check output
print(wilcox_test_result)

# Histogram of the number of records in changed pixels
occurrence_counts |>
  filter(cover_change2000.2006 == "Y") |>
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 20, fill="#800080", color="#800080", alpha=0.9)+
  theme_classic()

# Check how many cell_IDs have 0 records
changed_counts_df <- occurrence_counts |>
  filter(cover_change2000.2006 == "Y")
max(changed_counts_df$count)
min(changed_counts_df$count)

# Histogram of of the number of records in changed pixels > 2000 records only
occurrence_counts |>
  filter(cover_change2000.2006 == "Y") |>
  filter(count < 100) |>
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 2, fill="#800080", color="#800080", alpha=0.9)+
  theme_classic()


# Violin plot for changed pixels
changed_violins <- occurrence_counts |>
    filter(cover_change2000.2006 == "Y") |>
    ggplot(aes(x=cover_change2000.2006, y=count, fill=cell_ID)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
    xlab("Land Cover Change") +
    ylab("Number of Occurrences")+
    theme_classic() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)) +
    scale_y_continuous(labels = comma)

unchanged_violins <- occurrence_counts |>
  filter(cover_change2000.2006 == "N") |>
  ggplot(aes(x=cover_change2000.2006, y=count, fill=cell_ID)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
  xlab("Land Cover Change") +
  ylab("Number of Occurrences")+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  scale_y_continuous(labels = comma)

plot_grid(changed_violins, unchanged_violins , ncol = 1)

## 2.2. Map of Norway showing which pixels have more records before change (1997-2000) and which have more records after change (2006-2009) ----

### 2.2.1 Changed Pixels ----

# Define before and after period
before <- c(1997, 1998, 1999, 2000)
after <- c(2006, 2007, 2008, 2009)

# Filter the occurrences for each period
occ_changed_before <- occ_SSB_land_cover2000.2006 |>
  filter(cover_change2000.2006 == "Y") |>
  filter(year %in% before)

occ_changed_after <- occ_SSB_land_cover2000.2006 |>
  filter(cover_change2000.2006 == "Y") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_changed_before <- occ_changed_before |>
  group_by(cell_ID) |>
  summarise(occ_changed_before = n())

occ_counts_changed_after <- occ_changed_after |>
  group_by(cell_ID) |>
  summarise(occ_after_changed = n())

# Merge the two in a single df
occ_counts_changed2000.2006 <- full_join(occ_counts_changed_before, occ_counts_changed_after, by = "cell_ID") |>
  replace_na(list(occ_changed_before = 0, occ_after_changed = 0))

# Compare the number of occurrences before and after
occ_counts_changed2000.2006 <- occ_counts_changed2000.2006 |>
  mutate(dominant_period = ifelse(occ_changed_before > occ_after_changed, "1997-2000", "2006-2009"))

# Load the CORINE
norway_corine_status_modified_stack <- rast(here("data", "norway_corine_status_modified_stack.tif"))

# Re-project CORINE STATUS to match the projection of occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, 
                               "+proj=longlat +datum=WGS84 +no_defs", method = "near")

#Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]

# Assign each cell a unique number
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Create a new raster indicating dominant period
dominant_period_raster <- ID_raster
values(dominant_period_raster) <- NA

# Assign values based on dominant period
dominant_period_raster[match(occ_counts_changed2000.2006$cell_ID, values(ID_raster))] <- ifelse(occ_counts_changed2000.2006$dominant_period == "1997-2000", 1, 2)

# Convert raster to df for plotting
dominant_period_df <- as.data.frame(dominant_period_raster, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "1997-2000", "2006-2009"))

# Norway shapefile
norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
# Plot the map
dominant_period_changed2000_2006 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("1997-2000" = "#800080", "2006-2009" = "#FFD700")) +
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
    legend.text = element_text(size = 14))

# Convert ggplot to interactive plot with plotly
interactive_dominant_period_changed2000_2006 <- ggplotly(dominant_period_changed2000_2006)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_changed2000_2006, 
                        "dominant_period_changed_pixels2000_2006_map.html")

### 2.2.2 Unchanged Pixels ----

# Filter the occurrences for each period
occ_unchanged_before <- occ_SSB_land_cover2000.2006 |>
  filter(cover_change2000.2006 == "N") |>
  filter(year %in% before)

occ_unchanged_after <- occ_SSB_land_cover2000.2006 |>
  filter(cover_change2000.2006 == "N") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_unchanged_before <- occ_unchanged_before |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_before = n())

occ_counts_unchanged_after <- occ_unchanged_after |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_after = n())

# Merge the two in a single df
occ_counts_unchanged2000.2006 <- full_join(occ_counts_unchanged_before, occ_counts_unchanged_after, by = "cell_ID") |>
  replace_na(list(occ_unchanged_before = 0, occ_unchanged_after = 0))

# Compare the number of occurrences before and after
occ_counts_unchanged2000.2006 <- occ_counts_unchanged2000.2006 |>
  mutate(dominant_period = ifelse(occ_unchanged_before > occ_unchanged_after, "1997-2000", "2006-2009"))

# Create a new raster indicating dominant period
dominant_period_raster_unchanged_2000.2006 <- ID_raster
values(dominant_period_raster_unchanged_2000.2006) <- NA

# Assign values based on dominant period
dominant_period_raster_unchanged_2000.2006[match(occ_counts_unchanged2000.2006$cell_ID, values(ID_raster))] <- 
  ifelse(occ_counts_unchanged2000.2006$dominant_period == "1997-2000", 1, 2)

# Convert raster to df for plotting
dominant_period_unchanged_2000.2006_df <- as.data.frame(dominant_period_raster_unchanged_2000.2006, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "1997-2000", "2006-2009"))

# Plot the map
dominant_period_unchanged2000_2006 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_unchanged_2000.2006_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("1997-2000" = "#800080", "2006-2009" = "#FFD700")) +
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
    legend.text = element_text(size = 14))

# Convert ggplot to interactive plot with plotly
interactive_dominant_period_unchanged2000_2006 <- ggplotly(dominant_period_unchanged2000_2006)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_unchanged2000_2006, 
                        here("figures","dominant_period_unchanged_pixels2000_2006_map.html"))