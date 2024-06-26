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