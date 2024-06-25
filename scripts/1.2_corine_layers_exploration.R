##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_CLC_change_exploration
# This script contains code which explores the CORINE land cover CHANGE layers, 
# calculates land cover changes between 2000-2006, 2006-2012, 2012-2018 and 
# 2000-2018 and visualises the changes for each period
##----------------------------------------------------------------------------##

# 1. READ IN DATA --------------------------------------------------------------

## 1.1. Download layers (if needed) --------------------------------------------

download_file("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif", 
              "data/norway_corine_change_modified_stack.tif")

## 1.2. Read in layers ---------------------------------------------------------
norway_corine_change_modified_stack <- rast(here("data", 
                                        "norway_corine_change_modified_stack.tif"))

# 2. FIGURE 1  - MAPS OF LAND COVER CHANGES FOR EACH PERIOD --------------------

## 2.1. Prepare data -----------------------------------------------------------

# Read in Norway shapefile (original version)
norway <- vect(here("data", "raw_data", "raw_norway_shapefile",
                    "norway.shp"))

# Convert Norway shapefile to sf object
norway_sf <- st_as_sf(norway)

# Re-project CORINE to match the shapefile
norway_corine_wgs84 <- terra::project(norway_corine_change_modified_stack,
                                      "+proj=longlat +datum=WGS84 +no_defs",
                                      method = "near")

# Define dataframe names
df_names <- c("change_2000.2006_2000", "change_2000.2006_2006", 
              "change_2006.2012_2006", "change_2006.2012_2012",
              "change_2012.2018_2012", "change_2012.2018_2018")

# Convert raster layers to dfs
for (i in c(1:6)) {
  # convert to df
  df <- as.data.frame(norway_corine_wgs84[[i]], xy = TRUE) |>
    mutate(index = dplyr::row_number())
  # assign correct name
  assign(df_names[i], df)
}

## 2.2. Plot map for 2000-2006 -------------------------------------------------
change_2000.2006 <- ggplot()+
  geom_sf(data = norway_sf, fill = "lightgrey", color = "black")+
  geom_point(data = change_2000.2006_2000,
             aes(x = x, y = y), color = "#800080", size = 1)+
  coord_sf(ylim = c(58, 72)) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_y = unit(0.8, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.35) +
  
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

## 2.2. Plot map for 2006-2012 and 2012-2018 -----------------------------------
# This was done separately from the 2000-2006 period because I wanted the first
# map to have a North arrow and scale but did not want them in the others

# Define variable names
years <- c(2006, 2012)
data_vars <- c("change_2006.2012_2006", "change_2012.2018_2012")
plot_vars <- c("change_2006.2012", "change_2012.2018")

# Create empty list to store plots: 
plots <- list()

# Loop through years to create plots
for (i in c(1,2)) {
  plots[[plot_vars[i]]] <- ggplot() +
    geom_sf(data = norway_sf, fill = "lightgrey", color = "black") +
    geom_point(data = get(data_vars[i]),
               aes(x = x, y = y), color = "#800080", size = 1) +
    coord_sf(ylim = c(58, 72)) +
    labs(x = "Longitude", y = "Latitude") +
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
    )
}

# Arrange in a single grid
all_years_changes <- plot_grid(change_2000.2006, change_2006.2012, change_2012.2018, 
                               labels = c("a)", "b)", "c)" ), 
                               label_size = 12,
                               ncol = 3, nrow = 1,
                               label_y = 0.7)

# Save to file as .png
ggsave(here("figures", "cover_change_all_periods_Figure1.png"),
       width=17, height=13)

# Save to file as .svg
ggsave(here("figures", "cover_change_all_periods_Figure1.svg"),
       width=17, height=13)


# 3. FIGURE 2 - BARPLOTS COUNTING THE TYPE OF TRANSITIONS FOR ALL YEARS --------

## 3.1. Extract land cover before and after change for each period -------------
# Use (own) function to join land cover change dfs

# 1st period of change: 2000-2006
period1 <- process_period(change_2000.2006_2000, change_2000.2006_2006, 
                          "U2006_CHA0006_00_V2020_20u1", 
                          "U2006_CHA0006_06_V2020_20u1")

# 2nd period of change: 2006-2012
period2 <- process_period(change_2006.2012_2006, change_2006.2012_2012, 
                          "U2012_CHA0612_06_V2020_20u1", 
                          "U2012_CHA0612_12_V2020_20u1")

# 3rd period of change: 2012 -2018
period3 <- process_period(change_2012.2018_2012, change_2012.2018_2018, 
                          "U2018_CHA1218_12_V2020_20u1", 
                          "U2018_CHA1218_18_V2020_20u1")

# Combine all periods in a single df
all_periods <- bind_rows(period1, period2, period3)

## 3.2. Calculate the amount of pixels undergoing each transition type ---------
transitions <- all_periods |>
  group_by(source, target) |>
  summarise(transition_count = n()) |>
  ungroup()

## 3.3 Calculate the gains and losses for each land cover type -----------------

# Define land cover types
land_cover_types <- unique(transitions$source)

# Calculate gains and losses for each land cover type
gain_loss_list <- lapply(land_cover_types, function(land_cover) {
  # filter rows where the source land cover is different from the target one
  transition_counts |>
    filter(source != target) |>
    # create gain and loss columns
    mutate(
      # gain if the target land cover matches the current land_cover type
      gain = ifelse(target == land_cover, transition_count, 0),
      # loss if the source land cover matches the current land_cover type
      loss = ifelse(source == land_cover, -transition_count, 0)) |>
    # conver the gain and loss columns to long format
    pivot_longer(cols = c(gain, loss), names_to = "change", values_to = "count") |>
    # filter out rows with 0 counts
    filter(count != 0) |>
    # add land cover column 
    mutate(land_cover = land_cover)
})

# Combine list in a signle df
gain_loss_df <- bind_rows(gain_loss_list)
