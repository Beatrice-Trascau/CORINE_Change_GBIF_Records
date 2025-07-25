##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_CLC_change_exploration
# This script contains code which explores the CORINE land cover CHANGE layers, 
# calculates land cover changes between 2000-2006, 2006-2012, 2012-2018 and 
# 2000-2018 and visualises the changes for each period
##----------------------------------------------------------------------------##

# 1. READ IN DATA --------------------------------------------------------------

## 1.1. Read in layers ---------------------------------------------------------
norway_corine_change_modified_stack <- rast(here("data", "derived_data",
                                        "norway_corine_change_modified_stack.tif"))

## 1.2. Calculate the amount of land changing in each period -------------------



# Create vector to store the %
change_percentages <- numeric(nlyr(norway_corine_change_modified_stack))

# Loop the calculation of the 
for(i in 1:nlyr(norway_corine_change_modified_stack)){
  # Get the current raster
  raster <- norway_corine_change_modified_stack[[i]]
  
  # Extract total number of cells
  total_cells <- ncell(raster)
  
  # Get the number of non-NA cells
  non_na_cells <- global(!is.na(raster), sum)[1]
  
  # Calculate % on non-NA cells
  change_percentage <- (non_na_cells * 100) / total_cells
  
  # Store result in bector
  change_percentages[i] <- change_percentage
}

# Get the change % for each layer
change_percentages

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
  geom_sf(data = norway_sf, fill = "#F5F5F5", color = "black")+
  geom_point(data = change_2000.2006_2000,
             aes(x = x, y = y), color = "#800080", size = 1, alpha = 0.05)+
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

## 2.3. Plot map for 2006-2012 and 2012-2018 -----------------------------------
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
    geom_sf(data = norway_sf, fill = "#F5F5F5", color = "black") +
    geom_point(data = get(data_vars[i]),
               aes(x = x, y = y), color = "#800080", size = 1, alpha = 0.05) +
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
all_years_changes <- plot_grid(change_2000.2006, plots[[1]], 
                               plots[[2]], 
                               labels = c("a)", "b)", "c)" ), 
                               label_size = 12,
                               ncol = 3, nrow = 1,
                               label_y = 0.7)

# Save to file as .png
# ggsave(here("figures", "cover_change_all_periods_Figure1.png"),
#        width=17, height=13)

# Save to file as .svg
# ggsave(here("figures", "cover_change_all_periods_Figure1.svg"),
#        width=17, height=13)


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
combined_corine_df <- bind_rows(corine_dfs) |>
  # remove the rows where value  = 0 <- no change
  # no change here is the result of changes within the aggregated classes
  # but these are discarded for the purposes of our analysis
  filter(value != 0)

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
  filter(difference %in% combined_corine_df$value) |>
  # change column names
  rename(value = difference)

# Merge norway_corine_class_meaning df and combined_corine_df into one
corine_change_meaning <- merge(combined_corine_df,
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

## 3.4. Plot land cover transitions --------------------------------------------

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_all_years$scaled_count <- ifelse(abs(gain_loss_all_years$count) > 90,
                                           gain_loss_all_years$count/scaling_factor,
                                           gain_loss_all_years$count)


# Create plot
cover_transitions <- ggplot(gain_loss_all_years, aes(x = focus, y = scaled_count, 
                                                     fill = transition)) +
  geom_bar(stat="identity", position="stack", color = NA) +
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  ) +
  xlab("Land Cover Classes") +
  scale_fill_manual(values = c("#0072B2", "#F564E3","#009E73",
                               "#000000", "#E69F00",
                               "#F0E442","#83506c"),
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 11, color = "black"),
        axis.text.y = element_text(size = 11, colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 11, colour = "black",
                                   margin = margin(l = 2))) +
  guides(fill = guide_legend(ncol = 2))

## 3.5. Plot intensification and extensification transitions -------------------
# Create new column with Intensification/Extensification based on "difference"
# values. Check CORINE_Land_Cover_Transition_Classes_and_Scores.pdf in 
# T:\vm\inh\zoologi\Bruker\Beatrice\Chapter 1 for details

# Add intensification/extensification category based on the score
# the categorisation was decided a priori - see Appendix Table 3
intens_extens_all_years <- corine_change_meaning |>
  mutate(transition_meaning = case_when(
    value %in% c(79, 102, 249, 379, 589, 710,
                       23, 147, 170, 608, 631, -102,
                       -79, 487, 510, 277, 300, -340,
                       -331, -461, -130) ~ "Intensification",
    value %in% c(-249, -379, -589, -170, -300, -510,
                       -147, -277, -487, 130, -210, 461, 331,
                       121, 340, -23, -710, -121, -608) ~ "Extensification",
    value == 0 ~ "No_change"))  

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
intens_extens_transitions <- ggplot(intens_extens_gain_loss_all_years, 
                                    aes(x = focus, y = scaled_count,
                                              fill = transition_meaning))+
  geom_bar(stat="identity", position="stack", color = NA)+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("lightgreen", "sienna"),
                    name = "Transition Type",
                    labels = c("Extensification", "Intensification"))+
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   size = 11, colour = "black"),
        axis.text.y = element_text(size = 11, colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 11, colour = "black",
                                   margin = margin(l = 2))) +
  guides(fill = guide_legend(ncol = 2))

## 3.6. Combine plots from 3.3. and 3.4. ---------------------------------------

# Create figure with 2 panels
combined_barplot <- plot_grid(cover_transitions, intens_extens_transitions,
          labels = c("a)", "b)"),
          ncol = 2,
          align = "h")

# Save to file as .png
# ggsave(here("figures", "cover_transitions_all_periods_Figure2.png"),
#        width=20, height=13)

# Save to file as .svg
# ggsave(here("figures", "cover_transitions_all_periods_Figure2.pdf"),
#        width = 20, height=13)


# 4. SANKEY PLOT OF TRANSITIONS FOR ALL YEARS ----------------------------------

## 4.1. Sankey plot with year labels under stacks ------------------------------

# Re-format data for alluvial plot (all classes)
alluvial_data_with_forest_corrected <- corine_change_meaning |>
  mutate(source_name = gsub(" ", "_", source_name),
         target_name = gsub(" ", "_", target_name)) |>
  # create unique identifier for each transition
  mutate(alluvium_id = paste(source_name, target_name, 
                             source_year, target_year, sep = "_")) |>
  # convert to long format
  pivot_longer(cols = c(source_year, target_year), 
               names_to = "year_type", values_to = "year") |>
  # convert pixels to km² (each pixel = 100m × 100m = 0.01 km²)
  mutate(area_km2 = count * 0.01) |>
  # create cover_type column
  mutate(cover_type = ifelse(year_type == "source_year",
                             source_name, target_name)) |>
  # sort the data
  arrange(alluvium_id, year) |>
  # keep only necessary columns
  select(alluvium_id, year, cover_type, area_km2)

# Re-format data for alluvial plot (without Forest <-> TWS transitions)
alluvial_data_forestless_corrected <- corine_change_meaning |>
  # filter out forest <-> transitional woodland shrub transitions
  filter(!(source_name == "Forests" & target_name == "Transitional Woodland Shrub") &
           !(source_name == "Transitional Woodland Shrub" & target_name == "Forests")) |>
  mutate(source_name = gsub(" ", "_", source_name),
         target_name = gsub(" ", "_", target_name)) |>
  mutate(alluvium_id = paste(source_name, target_name, 
                             source_year, target_year, sep = "_")) |>
  pivot_longer(cols = c(source_year, target_year), 
               names_to = "year_type", values_to = "year") |>
  # convert pixels to km² (each pixel = 100m × 100m = 0.01 km²)
  mutate(area_km2 = count * 0.01) |>
  mutate(cover_type = ifelse(year_type == "source_year",
                             source_name, target_name)) |>
  arrange(alluvium_id, year) |>
  select(alluvium_id, year, cover_type, area_km2)

# Create common colour scheme
shared_colors <- scale_fill_manual(values = c("#0072B2", "#F564E3",
                                              "#009E73","#000000", 
                                              "#E69F00","#F0E442",
                                              "#83506c"),
                                   name = "Land Cover Classes",
                                   labels = c("Agriculture & Vegetation",
                                              "Complex Agriculture",
                                              "Forests", 
                                              "Moors, Heathland & Grassland",
                                              "Sparse Vegetation",
                                              "Transitional Woodland Shrub",
                                              "Urban Fabric"))
# Sankey plot with all classes
panel_a_all_classes <- ggplot(alluvial_data_with_forest_corrected,
                            aes(x = as.factor(year), 
                                stratum = cover_type, 
                                alluvium = alluvium_id,
                                y = area_km2, 
                                fill = cover_type)) +
  
  # flow ribbons
  geom_flow(stat = "alluvium", 
            lode.guidance = "frontback", 
            color = "gray20", 
            linewidth = 0.1, 
            alpha = 0.7) +
  
  # strata
  geom_stratum(color = "white", linewidth = 0.5) +
  
  # styling
  shared_colors +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = expression("Area (km"^2*")"),
                     labels = scales::comma_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20))


# Sankey plot without Forest <-> TWS transitions
panel_b_forestless <- ggplot(alluvial_data_forestless_corrected,
                            aes(x = as.factor(year), 
                                stratum = cover_type, 
                                alluvium = alluvium_id,
                                y = area_km2, 
                                fill = cover_type)) +
  
  # flow ribbons
  geom_flow(stat = "alluvium", 
            lode.guidance = "frontback", 
            color = "gray20", 
            linewidth = 0.1, 
            alpha = 0.7) +
  
  # strata
  geom_stratum(color = "white", linewidth = 0.5) +
  
  # styling
  shared_colors +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = expression("Area (km"^2*")"),
                     labels = scales::comma_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.8, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# Combine paels into single figure
combined_plot_corrected <- plot_grid(panel_a_all_classes, panel_b_forestless,
                                     labels = c("a)", "b)"),
                                     label_size = 16,
                                     ncol = 1,
                                     nrow = 2,
                                     align = "v",
                                     axis = "lr",
                                     rel_heights = c(1, 1.2))

# Save to file as .png
ggsave(here("figures", "cover_transitions_alluvials_Figure4.png"),
       width=20, height=13)

# Save to file as .svg
ggsave(here("figures", "cover_transitions_alluvials_Figure4.svg"),
       width=20, height=13)

## 4.2. Sankey plot with year labels under the ribbons -------------------------

# Common margins for both panels
common_margins <- theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# All classes
all_classes_ribbons <- ggplot(alluvial_data_with_forest_corrected,
                              aes(x = as.factor(year), 
                                  stratum = cover_type, 
                                  alluvium = alluvium_id,
                                  y = area_km2, 
                                  fill = cover_type)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "gray20", linewidth = 0.1, alpha = 0.7) +
  geom_stratum(color = "white", linewidth = 0.5) +
  geom_text(data = data.frame(x = c(1.5, 2.5, 3.5),
                              y = rep(0.5, 3),
                              label = c("2000-2006", "2006-2012", "2012-2018")),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            vjust = 1.5, size = 5, fontface = "bold", color = "black") +
  shared_colors +
  guides(color = "none") +  # Prevent spurious legend
  scale_x_discrete(name = "", labels = c("", "", "", "")) +
  scale_y_continuous(name = expression("Area (km"^2*")"),
                     labels = scales::comma_format()) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  common_margins

# Forestless
forestless_ribbons <- ggplot(alluvial_data_forestless_corrected,
                             aes(x = as.factor(year), 
                                 stratum = cover_type, 
                                 alluvium = alluvium_id,
                                 y = area_km2, 
                                 fill = cover_type)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "gray20", linewidth = 0.1, alpha = 0.7) +
  geom_stratum(color = "white", linewidth = 0.5) +
  geom_text(data = data.frame(x = c(1.5, 2.5, 3.5),
                              y = rep(0.5, 3),
                              label = c("2000-2006", "2006-2012", "2012-2018")),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            vjust = 1.5, size = 5, fontface = "bold", color = "black") +
  shared_colors +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE),
         color = "none") +
  scale_x_discrete(name = "", labels = c("", "", "", "")) +
  scale_y_continuous(name = expression("Area (km"^2*")"),
                     labels = scales::comma_format()) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.8, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) +
  common_margins

# Combine panels into single figure
final_ribbons <- plot_grid(all_classes_ribbons, forestless_ribbons,
  labels = c("a)", "b)"), label_size = 16, ncol = 1,
  rel_heights = c(1.1, 1.4))

# Save to file as .png
ggsave(here("figures", "cover_transitions_alluvials_period_labels_Figure5.png"),
       width=20, height=13)

# Save to file as .svg
ggsave(here("figures", "cover_transitions_alluvials_period_labels_Figure5.svg"),
       width=20, height=13)


# 5. EXTRACT TABLE WITH AMOUNT OF LAND FOR EACH TRANSITION IN KM2 --------------

# Create table for all the transitions
transition_quantities <- create_transition_table(corine_change_meaning)

# Check data
head(transition_table)

# Write to file
write.csv(transition_quantities, here("data", "derived_data", 
                                      "land_cover_transitions_quantity_km2.csv"),
          row.names = FALSE)

write_xlsx(transition_quantities, here("data", "derived_data", 
                                       "land_cover_transitions_quantity_km2.xlsx"))

# 6. NEW MAPS WITH AGGREGATIONS ------------------------------------------------

## 6.1. Prepare layers for plotting --------------------------------------------

# Reproject Norway shapefile to match CLC
corine_crs <- crs(norway_corine_change_modified_stack)
norway_sf_projected <- st_transform(norway_sf, crs = corine_crs)

# Extract first layer of each pair
binary_2000_2006 <- !is.na(norway_corine_change_modified_stack[[1]])
binary_2006_2012 <- !is.na(norway_corine_change_modified_stack[[3]])
binary_2012_2018 <- !is.na(norway_corine_change_modified_stack[[5]])

# Aggregate to 15km resolution
agg_factor <- 150

agg_2000_2006 <- aggregate(binary_2000_2006, fact = agg_factor, 
                           fun = calc_percent_change)
agg_2006_2012 <- aggregate(binary_2006_2012, fact = agg_factor, 
                           fun = calc_percent_change)
agg_2012_2018 <- aggregate(binary_2012_2018, fact = agg_factor, 
                           fun = calc_percent_change)

# Convert to dataframe for plotting
df_2000_2006 <- as.data.frame(agg_2000_2006, xy = TRUE) |>
  filter(!is.na(U2006_CHA0006_00_V2020_20u1)) |>
  rename(percent_change = U2006_CHA0006_00_V2020_20u1)

df_2006_2012 <- as.data.frame(agg_2006_2012, xy = TRUE) |>
  filter(!is.na(U2012_CHA0612_06_V2020_20u1)) |>
  rename(percent_change = U2012_CHA0612_06_V2020_20u1)

df_2012_2018 <- as.data.frame(agg_2012_2018, xy = TRUE) |>
  filter(!is.na(U2018_CHA1218_12_V2020_20u1)) |>
  rename(percent_change = U2018_CHA1218_12_V2020_20u1)

## 6.2. Prepare plot -----------------------------------------------------------

# Create base theme for plotting
base_theme <- theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank())

# Previous global min/max calculations remain the same
global_min <- floor(min(c(df_2000_2006$percent_change,
                          df_2006_2012$percent_change,
                          df_2012_2018$percent_change)) * 100) / 100
global_max <- ceiling(max(c(df_2000_2006$percent_change,
                            df_2006_2012$percent_change,
                            df_2012_2018$percent_change)) * 100) / 100


# Function to plot maps
create_period_map <- function(data, include_decorations = FALSE, include_legend = FALSE) {
  # Define all breaks for color scaling (maintaining scientific accuracy)
  color_breaks <- c(0, 0.5, 1, 5, 10, 15)
  
  # Define subset of breaks for label display (omitting 0.5)
  label_breaks <- c(0, 1, 5, 10, 15)
  label_values <- c("0.0", "1.0", "5.0", "10.0", "15.0")
  
  fill_scale <- scale_fill_gradientn(
    colors = c("#FFFFFF", "#83506c", "#F564E3", "#0072B2", "#009E73", "#E69F00"),
    name = "% Area Changed",
    limits = c(0, max(color_breaks)),
    # Use color_breaks for the underlying scale
    values = scales::rescale(color_breaks),
    # Use label_breaks for display
    breaks = label_breaks,
    labels = label_values,
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 0.5,
      nbin = 100,
      draw.ulim = TRUE,
      draw.llim = TRUE,
      frame.colour = "black",
      frame.linewidth = 0.5,
      ticks.colour = "black",
      ticks = TRUE,
      label.theme = element_text(
        size = 10,
        angle = 45,
        hjust = 1
      )
    ),
    na.value = "transparent"
  )
  
  # Construct map with explicit spatial parameters
  p <- ggplot() +
    # Maintain consistent projection and appearance of Norway outline
    geom_sf(data = norway_sf_projected, fill = "#F5F5F5", color = "black") +
    # Ensure proper rendering of change data
    geom_tile(data = data,
              aes(x = x, y = y, fill = percent_change),
              alpha = 0.8) +
    fill_scale +
    # Maintain proper spatial extent
    coord_sf() +
    base_theme
  
  # Add cartographic elements if requested
  if(include_decorations) {
    p <- p +
      annotation_north_arrow(
        location = "br",
        which_north = "true",
        pad_y = unit(0.8, "cm"),
        style = north_arrow_fancy_orienteering
      ) +
      annotation_scale(location = "br", width_hint = 0.35)
  }
  
  # Configure legend visibility and positioning
  if(!include_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.box.spacing = unit(0.2, "cm"),
      legend.margin = margin(t = 0.2, b = 0.2, unit = "cm"),
      legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
  }
  
  return(p)
}
# Create and combine maps as before
map_2000_2006 <- create_period_map(df_2000_2006, 
                                   include_decorations = TRUE, 
                                   include_legend = TRUE)
map_2006_2012 <- create_period_map(df_2006_2012, 
                                   include_decorations = FALSE, 
                                   include_legend = FALSE)
map_2012_2018 <- create_period_map(df_2012_2018, 
                                   include_decorations = FALSE, 
                                   include_legend = FALSE)

# Combine all maps into one plot
all_periods_changes <- plot_grid(
  map_2000_2006, map_2006_2012, map_2012_2018,
  labels = c("a)", "b)", "c)"),
  label_size = 12,
  ncol = 3,
  nrow = 1,
  align = 'h',
  axis = 'tb',
  label_y = 0.8)

# Save to file
ggsave(here("figures", "cover_change_all_periods_Figure1_aggregated50km.png"),
       all_periods_changes, width = 17, height = 13, dpi = 300)

ggsave(here("figures", "cover_change_all_periods_Figure1_aggregated50km.svg"),
       all_periods_changes, width = 17, height = 13,dpi = 300)

# END OF SCRIPT ----------------------------------------------------------------