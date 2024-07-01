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

## 3.1. Calculate differences in land cover between periods --------------------
# Using (own) function that loops through all 3 different periods

# Create list of raster indices and the corresponding years
corine_years <- list(
  c(1, 2, "2000", "2006"),
  c(3, 4, "2006", "2012"),
  c(5, 6, "2012", "2018"))

# Create empty list to store dfs
corine_dfs <- list()

# Apply function to each period
for (i in seq_along(corine_years)) {
  indices <- corine_years[[i]]
  df <- process_corine_change(norway_corine_change_modified_stack, 
                              indices[1], indices[2], indices[3], indices[4])
  corine_dfs[[i]] <- df
}

# Combine all dfs into a single one
combined_corine_df <- bind_rows(corine_dfs)

## 3.2. Get source and target land cover values for transitions ----------------

# Create df to map numerical values to land cover change categories
corine_class_meaning <- data.frame(source_number = c(rep(1,7), rep(80,7), 
                                                     rep(103,7),rep(250,7), 
                                                     rep(380,7), rep(590,7),
                                                     rep(711,7)),
                                   source_name = c(rep("Urban Fabric",7), 
                                                   rep("Complex Agriculture",7),
                                                   rep("Agriculture & Vegetation",7), 
                                                   rep("Forests",7),
                                                   rep("Moors, Heath & Grass",7), 
                                                   rep("Transitional Woodland Shrub",7),
                                                   rep("Sparse Vegetation",7)),
                                   target_number = c(rep(c(1,80,103,250,380,590,711), 7)),
                                   target_name = c(rep(c("Urban Fabric", "Complex Agriculture", 
                                                         "Agriculture & Vegetation",
                                                         "Forests", "Moors, Heath & Grass", 
                                                         "Transitional Woodland Shrub",
                                                         "Sparse Vegetation"), 7))) |>
  mutate(difference = source_number - target_number)

# Subset score meaning df to only contain the "differences" found in the layers
norway_corine_class_meaning <- corine_class_meaning |>
  filter(difference %in% all_years$value) |>
  # change column names
  rename(value = difference)

# Merge norway_corine_class_meaning df and combined_corine_df into one
corine_change_meaning <- merge(all_years,
                               norway_corine_class_meaning,
                               by = "value")

## 3.3. Add loss and gain to the df --------------------------------------------

# Create dataframe of "transition to" values
# this dataframe will have the "focus" cover class in the "source" column and 
# the cover class it transitions to in the "target" column the values will be 
# negative because for every source class, this is the area "lost" that is 
# converted to the "target" cover class
loss_all_years <- corine_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

# Create dataframe of "transitions from" values
# this dataframe will have "focus" cover class in the "target" column and the 
# cover class it transitions from in the "target" column the values will be 
# positive because for every source class, this is the area "gained" that is 
#converted from the "source" cover class
gain_all_years <- corine_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)

# Merge the gain and loss dataframes into a single df
gain_loss_all_years <- rbind(loss_all_years, gain_all_years)

## 3.5. Plot land cover transitions --------------------------------------------

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_all_years$scaled_count <- ifelse(abs(gain_loss_all_years$count) > 90,
                                           gain_loss_all_years$count/scaling_factor,
                                           gain_loss_all_years$count)


# Create plot
cover_transitions <- ggplot(gain_loss_all_years, aes(x = focus, y = scaled_count, 
                                                     fill = transition)) +
  geom_bar(stat="identity", position="stack") +
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  ) +
  xlab("Land Cover Classes") +
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                               "#6A3D9A", "#FF7F00",
                               "gold1","maroon"),
                    name = "Land Cover Classes",
                    labels = c("Agriculture & Vegetation", "Complex Agriculture",
                               "Forests", "Moors, Heathland & Grassland",
                               "Sparse Vegetation", "Transitional Woodland Shrub",
                               "Urban Fabric")) +
  scale_x_discrete(
    limits = c("Agriculture & Vegetation", "Complex Agriculture",
               "Moors, Heath & Grass", "Sparse Vegetation",
               "Urban Fabric", "Forests",
               "Transitional Woodland Shrub"),
    labels = c("Agriculture & Vegetation", "Complex Agriculture",
               "Moors, Heathland & Grassland", "Sparse Vegetation",
               "Urban Fabric", "Forests", "Transitional Woodland Shrub")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

## 3.6. Plot intensification and extensification transitions -------------------
# Create new column with Intensification/Extensification based on "difference"
# values. Check CORINE_Land_Cover_Transition_Classes_and_Scores.pdf in 
# T:\vm\inh\zoologi\Bruker\Beatrice\Chapter 1 for details

# Add intensification/extensification category based on the score
# the categorisation was decided a priori - see Appendix Table 3
intens_extens_all_years <- corine_change_meaning |>
  mutate(transition_meaning = case_when(
    difference %in% c(79, 102, 249, 379, 589, 710,
                       23, 147, 170, 608, 631, -102,
                       -79, 487, 510, 277, 300, -340,
                       -331, -461, -130) ~ "Intensification",
    difference %in% c(-249, -379, -589, -170, -300, -510,
                       -147, -277, -487, 130, -210, 461, 331,
                       121, 340, -23, -710, -121, -608) ~ "Extensification",
    difference == 0 ~ "No_change"))  

# Create df of "transition to" values - see above for definition
intens_extens_loss_all_years <- intens_extens_all_years |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select(focus, transition, count, transition_meaning)

# Create df of "transition from" values - see above for definition
intens_extens_gain_all_years <- intens_extens_all_years |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count, transition_meaning)

# Merge gain and loss dfs into a single df
intens_extens_gain_loss_all_years <- rbind(intens_extens_loss_all_years,
                                           intens_extens_gain_all_years)

# Create new column in df to scale down large values
intens_extens_gain_loss_all_years$scaled_count <- ifelse(abs(intens_extens_gain_loss_all_years$count) > 90,
                                                         intens_extens_gain_loss_all_years$count/scaling_factor,
                                                         intens_extens_gain_loss_all_years$count)

# Plot figure of transitions into intensification and extensification
intens_extens_transitions <- ggplot(intens_extens_gain_loss_all_years, aes(x = focus, y = scaled_count,
                                              fill = transition_meaning))+
  geom_bar(stat="identity", position="stack")+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("lightgreen", "sienna"),
                    name = "Transition Type",
                    labels = c("Nature Gain", "Nature Loss"))+ 
  scale_x_discrete(
    limits = c("Agriculture & Vegetation", "Complex Agriculture",
               "Moors, Heath & Grass", "Sparse Vegetation",
               "Urban Fabric", "Forests",
               "Transitional Woodland Shrub"),
    labels = c("Agriculture & Vegetation", "Complex Agriculture",
               "Moors, Heathland & Grassland", "Sparse Vegetation",
               "Urban Fabric", "Forests", "Transitional Woodland Shrub")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 5.5, linetype = "dashed") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2))

## 3.5. Combine plots from 3.3. and 3.4. ---------------------------------------
plot_grid(cover_transitions, intens_extens_transitions,
          labels = c("a)", "b)"),
          ncol = 2,
          align = "h")
