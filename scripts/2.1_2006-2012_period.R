## Extension of script 2.1 but for 2006-2012 period

# 1. SIGNED RANK TEST ----

# Add column that indicates change in land cover between 2006 and 2012
occ_SSB_land_cover2006.2012 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover2006) & !is.na(land_cover2012)) |>
  mutate(cover_change2006.2012 = if_else (land_cover2006 == land_cover2012, "N", "Y"))

# Calculate the number of occurrences in changed and unchanged pixels
occurrence_counts2006.2012 <- occ_SSB_land_cover2006.2012 |>
  group_by(cell_ID, cover_change2006.2012) |>
  summarise(count = n(), .groups = 'drop')

# Create 2 separate dfs for changed and unchanged pixels
changed_counts2006.2012 <- occurrence_counts2006.2012 |>
  filter(cover_change2006.2012 == "Y") |>
  pull(count)

unchanged_counts2006.2012 <- occurrence_counts2006.2012 |>
  filter(cover_change2006.2012 == "N") |>
  pull(count)

# Wilcoxon signed-rank test
wilcox_test_result <- wilcox.test(changed_counts2006.2012, unchanged_counts2006.2012, paired = FALSE)

# Check output
print(wilcox_test_result)


# Violin plot for changed pixels
changed_violins2006.2012 <- occurrence_counts2006.2012 |>
  filter(cover_change2006.2012 == "Y") |>
  ggplot(aes(x=cover_change2006.2012, y=count, fill=cell_ID)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
  xlab("Land Cover Change") +
  ylab("Number of Occurrences")+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  scale_y_continuous(labels = comma)

unchanged_violins2006.2012 <- occurrence_counts2006.2012 |>
  filter(cover_change2006.2012 == "N") |>
  ggplot(aes(x=cover_change2006.2012, y=count, fill=cell_ID)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="#800080", alpha=0.2) +
  xlab("Land Cover Change") +
  ylab("Number of Occurrences")+
  theme_classic() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  scale_y_continuous(labels = comma)

plot_grid(changed_violins2006.2012, unchanged_violins2006.2012 , ncol = 1)

# 2. DOMINANT PERIOD MAPS ----
## 2.2. Map of Norway showing which pixels have more records before change (1997-2000) and which have more records after change (2006-2009) ----

# Add column that indicates change in land cover between 2000 and 2006
occ_SSB_land_cover2006.2012 <- occ_SSB_land_cover |>
  filter(!is.na (land_cover2006) & !is.na(land_cover2012)) |>
  mutate(cover_change2006.2012 = if_else (land_cover2006 == land_cover2012, "N", "Y"))

### 2.2.1 Changed Pixels ----

# Define before and after period
before <- c(2003, 2004, 2005, 2006)
after <- c(2012, 2013, 2014, 2015)

# Filter the occurrences for each period
occ_changed_before_2006.2012 <- occ_SSB_land_cover2006.2012 |>
  filter(cover_change2006.2012 == "Y") |>
  filter(year %in% before)

occ_changed_after_2006.2012 <- occ_SSB_land_cover2006.2012 |>
  filter(cover_change2006.2012 == "Y") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_changed_before_2006.2012 <- occ_changed_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(occ_changed_before_2006.2012 = n())

occ_counts_changed_after_2006.2012 <- occ_changed_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(occ_changed_after_2006.2012 = n())

# Merge the two in a single df
occ_counts_changed2006.2012 <- full_join(occ_counts_changed_before_2006.2012, 
                                         occ_counts_changed_after_2006.2012, by = "cell_ID") |>
  replace_na(list(occ_changed_before_2006.2012 = 0, occ_changed_after_2006.2012 = 0))

# Compare the number of occurrences before and after
occ_counts_changed2006.2012 <- occ_counts_changed2006.2012 |>
  mutate(dominant_period = ifelse(occ_changed_before_2006.2012 > occ_changed_after_2006.2012, "2003-2006", "2012-2015"))

# Create a new raster indicating dominant period
dominant_period_raster <- ID_raster
values(dominant_period_raster) <- NA

# Assign values based on dominant period
dominant_period_raster[match(occ_counts_changed2006.2012$cell_ID, values(ID_raster))] <- 
  ifelse(occ_counts_changed2006.2012$dominant_period == "2003-2006", 1, 2)

# Convert raster to df for plotting
dominant_period_df <- as.data.frame(dominant_period_raster, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "2003-2006", "2012-2015"))


# Plot the map
dominant_period_changed2006_2012 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("2003-2006" = "#800080", "2012-2015" = "#FFD700")) +
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
interactive_dominant_period_changed2006_2012 <- ggplotly(dominant_period_changed2006_2012)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_changed2006_2012, 
                        here("figures","dominant_period_changed_pixels2006_2012_map.html"))

### 2.2.2 Unchanged Pixels ----

# Filter the occurrences for each period
occ_unchanged_before_2006.2012 <- occ_SSB_land_cover2006.2012 |>
  filter(cover_change2006.2012 == "N") |>
  filter(year %in% before)

occ_unchanged_after_2006.2012 <- occ_SSB_land_cover2006.2012 |>
  filter(cover_change2006.2012 == "N") |>
  filter(year %in% after)

# Count the number of records per cell for each period
occ_counts_unchanged_before_2006.2012 <- occ_unchanged_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_before_2006.2012 = n())

occ_counts_unchanged_after_2006.2012 <- occ_unchanged_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(occ_unchanged_after_2006.2012 = n())

# Merge the two in a single df
occ_counts_unchanged2006.2012 <- full_join(occ_counts_unchanged_before_2006.2012, 
                                           occ_counts_unchanged_after_2006.2012, by = "cell_ID") |>
  replace_na(list(occ_unchanged_before_2006.2012 = 0, occ_unchanged_after_2006.2012 = 0))

# Compare the number of occurrences before and after
occ_counts_unchanged2006.2012 <- occ_counts_unchanged2006.2012 |>
  mutate(dominant_period = ifelse(occ_unchanged_before_2006.2012 > occ_unchanged_after_2006.2012, "2003-2006", "2012-2015"))

# Create a new raster indicating dominant period
dominant_period_raster_unchanged_2006.2012 <- ID_raster
values(dominant_period_raster_unchanged_2006.2012) <- NA

# Assign values based on dominant period
dominant_period_raster_unchanged_2006.2012[match(occ_counts_unchanged2006.2012$cell_ID, values(ID_raster))] <- 
  ifelse(occ_counts_unchanged2006.2012$dominant_period == "2003-2006", 1, 2)

# Convert raster to df for plotting
dominant_period_unchanged_2006.2012_df <- as.data.frame(dominant_period_raster_unchanged_2006.2012, xy = TRUE, na.rm = TRUE) |>
  mutate(dominant_period = if_else(U2006_CLC2000_V2020_20u1 == 1, "2003-2006", "2012-2015"))

# Plot the map
dominant_period_unchanged2006_2012 <- ggplot() +
  geom_sf(data = norway, fill = "lightgrey", color = "black") +
  geom_point(data = dominant_period_unchanged_2006.2012_df, 
             aes(x = x, y = y, color = dominant_period), size = 2) +
  scale_color_manual(values = c("2003-2006" = "#800080", "2012-2015" = "#FFD700")) +
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
interactive_dominant_period_unchanged2006_2012 <- ggplotly(dominant_period_unchanged2006_2012)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_dominant_period_unchanged2006_2012, 
                        "dominant_period_unchanged_pixels2006_2012_map.html")