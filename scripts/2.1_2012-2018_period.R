## Extension of script 2.1 but for 2012-2018 period

# 1. SIGNED RANK TEST ----

# Add column that indicates change in land cover between 2006 and 2012
occ_SSB_land_cover2012.2018 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover2012) & !is.na(land_cover2018)) |>
  mutate(cover_change2012.2018 = if_else (land_cover2012 == land_cover2018, "N", "Y"))

# Calculate the number of occurrences in changed and unchanged pixels
occurrence_counts2012.2018 <- occ_SSB_land_cover2012.2018 |>
  group_by(cell_ID, cover_change2012.2018) |>
  summarise(count = n(), .groups = 'drop')

# Create 2 separate dfs for changed and unchanged pixels
changed_counts2012.2018 <- occurrence_counts2012.2018 |>
  filter(cover_change2012.2018 == "Y") |>
  pull(count)

unchanged_counts2012.2018 <- occurrence_counts2012.2018 |>
  filter(cover_change2012.2018 == "N") |>
  pull(count)

# Wilcoxon signed-rank test
wilcox_test_result <- wilcox.test(changed_counts2012.2018, unchanged_counts2012.2018, paired = FALSE)

# Check output
print(wilcox_test_result)


# Violin plot for changed pixels
changed_violins2012.2018 <- occurrence_counts2012.2018 |>
  filter(cover_change2012.2018 == "Y") |>
  ggplot(aes(x=cover_change2012.2018, y=count, fill=cell_ID)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
  xlab("Land Cover Change") +
  ylab("Number of Occurrences")+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  scale_y_continuous(labels = comma)

unchanged_violins2012.2018 <- occurrence_counts2012.2018 |>
  filter(cover_change2012.2018 == "N") |>
  ggplot(aes(x=cover_change2012.2018, y=count, fill=cell_ID)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
  xlab("Land Cover Change") +
  ylab("Number of Occurrences")+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  scale_y_continuous(labels = comma)

plot_grid(changed_violins2012.2018, unchanged_violins2012.2018 , ncol = 1)

# 2. DOMINANT PERIOD MAPS ----
## 2.2. Map of Norway showing which pixels have more records before change (1997-2000) and which have more records after change (2006-2009) ----

# Add column that indicates change in land cover between 2000 and 2006
occ_SSB_land_cover2012.2018 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover2012) & !is.na(land_cover2018)) |>
  mutate(cover_change2012.2018 = if_else (land_cover2012 == land_cover2018, "N", "Y"))

### 2.2.1 Changed Pixels ----

# Define before and after period
before <- c(2009, 2010, 2011, 2012)
after <- c(2015, 2016, 2017, 2018)

# Filter the occurrences for each period
occ_changed_before_2012.2018 <- occ_SSB_land_cover2012.2018 |>
  filter(cover_change2012.2018 == "Y") |>
  filter(year %in% before)

occ_changed_after_2012.2018 <- occ_SSB_land_cover2012.2018 |>
  filter(cover_change2012.2018 == "Y") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_changed_before_2012.2018 <- occ_changed_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(occ_changed_before_2012.2018 = n())

occ_counts_changed_after_2012.2018 <- occ_changed_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(occ_changed_after_2012.2018 = n())

# Merge the two in a single df
occ_counts_changed2012.2018 <- full_join(occ_counts_changed_before_2012.2018, 
                                         occ_counts_changed_after_2012.2018, by = "cell_ID") |>
  replace_na(list(occ_changed_before_2012.2018 = 0, occ_changed_after_2012.2018 = 0))

# Ensure count_period1 and count_period2 are numeric
occ_counts_changed2012.2018 <- occ_counts_changed2012.2018 |>
  mutate(occ_changed_before_2012.2018 = occ_changed_before_2012.2018,
         occ_changed_after_2012.2018 = occ_changed_after_2012.2018)

# Compare the number of occurrences before and after
occ_counts_changed2012.2018 <- occ_counts_changed2012.2018 |>
  mutate(dominant_period_2012.2018 = ifelse(occ_changed_before_2012.2018 > occ_changed_after_2012.2018, "2009-2012", "2015-2018"))

# Create a new raster indicating dominant period
dominant_period_raster_changed2012.2018 <- ID_raster
values(dominant_period_raster_changed2012.2018) <- NA

# Assign values based on dominant period
dominant_period_raster_changed2012.2018[match(occ_counts_changed2012.2018$cell_ID, values(ID_raster))] <- 
  ifelse(occ_counts_changed2012.2018$dominant_period_2012.2018 == "2009-2012", 1, 2)

# Convert raster to df for plotting
dominant_period_changed2012.2018_df <- as.data.frame(dominant_period_raster_changed2012.2018, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "2009-2012", "2015-2018"))

# Plot the map
dominant_period_changed2012_2018 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_changed2012.2018_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("2009-2012" = "#800080", "2015-2018" = "#FFD700")) +
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
interactive_dominant_period_changed2012_2018 <- ggplotly(dominant_period_changed2012_2018)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_changed2012_2018, 
                        here("figures","dominant_period_changed_pixels2012_2018_map.html"))

### 2.2.2 Unchanged Pixels ----

# Filter the occurrences for each period
occ_unchanged_before_2012.2018 <- occ_SSB_land_cover2012.2018 |>
  filter(cover_change2012.2018 == "N") |>
  filter(year %in% before)

occ_unchanged_after_2012.2018 <- occ_SSB_land_cover2012.2018 |>
  filter(cover_change2012.2018 == "N") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_unchanged_before_2012.2018 <- occ_unchanged_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_before_2012.2018 = n())

occ_counts_unchanged_after_2012.2018 <- occ_unchanged_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_after_2012.2018 = n())

# Merge the two in a single df
occ_counts_unchanged2012.2018 <- full_join(occ_counts_unchanged_before_2012.2018, 
                                           occ_counts_unchanged_after_2012.2018, by = "cell_ID") |>
  replace_na(list(occ_unchanged_before_2012.2018 = 0, occ_unchanged_after_2012.2018 = 0))

# Compare the number of occurrences before and after
occ_counts_unchanged2012.2018 <- occ_counts_unchanged2012.2018 |>
  mutate(dominant_period = ifelse(occ_unchanged_before_2012.2018 > occ_unchanged_after_2012.2018, "2009-2012", "2015-2018"))

# Create a new raster indicating dominant period
dominant_period_raster_unchanged_2012.2018 <- ID_raster
values(dominant_period_raster_unchanged_2012.2018) <- NA

# Assign values based on dominant period
dominant_period_raster_unchanged_2012.2018[match(occ_counts_unchanged2012.2018$cell_ID, values(ID_raster))] <- 
  ifelse(occ_counts_unchanged2012.2018$dominant_period == "2009-2012", 1, 2)

# Convert raster to df for plotting
dominant_period_unchanged_2012.2018_df <- as.data.frame(dominant_period_raster_unchanged_2012.2018, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "2009-2012", "2015-2018"))

# Plot the map
dominant_period_unchanged2012_2018 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_unchanged_2012.2018_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("2009-2012" = "#800080", "2015-2018" = "#FFD700")) +
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
interactive_dominant_period_unchanged2012_2018 <- ggplotly(dominant_period_unchanged2012_2018)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_unchanged2012_2018, 
                        "dominant_period_unchanged_pixels2012_2018_map.html")