##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_CLC_change_exploration
# This script contains code which explores the CORINE land cover CHANGE layers, 
# calculates land cover changes between 2000-2006, 2006-2012, 2012-2018 and 
# 2000-2018 and visualises the changes for each period
##----------------------------------------------------------------------------##

## 1.1. Download layers (if needed) --------------------------------------------

download_file("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif", 
              "data/norway_corine_change_modified_stack.tif")

## 1.2. Read in layers ---------------------------------------------------------
norway_corine_change_modified_stack <- rast(here("data", 
                                        "norway_corine_change_modified_stack.tif"))

# 2. EXPLORE LAYERS ------------------------------------------------------------

## 2.1. Convert rasters to dfs -------------------------------------------------

# Read in Norway shapefile (original version)
norway <- vect(here("data", "raw_data", "raw_norway_shapefile",
                    "norway.shp"))

# Convert Norway shapefile to sf object
norway_sf <- st_as_sf(norway)

# Re-project CORINE to match the shapefile
norway_corine_wgs84 <- terra::project(norway_corine_change_modified_stack,
                                      "+proj=longlat +datum=WGS84 +no_defs",
                                      method = "near")


# Convert the 1st (2000), 3rd (2006), and 5th (2012) layers to dataframe
for (i in c(1, 3, 5)) {
  var_name <- paste0("norway_corine", 2000 + (i - 1) * 3)
  assign(var_name, as.data.frame(norway_corine_wgs84[[i]], xy = TRUE))
}

## 2.2. Plot map for 2000-2006 -------------------------------------------------
change_2000.2006 <- ggplot()+
  geom_sf(data = norway_sf, fill = "lightgrey", color = "black")+
  geom_point(data = norway_corine2000,
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
data_vars <- c("norway_corine2006", "norway_corine2012")
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
ggsave(here("figures", "cover_change_all_periods_Figure2.png"),
       width=17, height=13)

# Save to file as .svg
ggsave(here("figures", "cover_change_all_periods_Figure2.svg"),
       width=17, height=13)

# 3. LAND COVER TRANSITIONS ----------------------------------------------------

# Define class modifications for easier mapping 
class_modifications <- list(
  list(from = 1:11, to = 1),
  list(from = c(12, 18, 20), to = 80),
  list(from = 21, to = 103),
  list(from = c(23, 24, 25), to = 250),
  list(from = c(26, 27), to = 380),
  list(from = 29, to = 590),
  list(from = 32, to = 711),
  list(from = c(30, 31, 33, 34, 35, 36, 39, 40, 41, 43, 44, 127, 128), to = NA))

# Prepare class meaning dataframe
class_meaning <- data.frame(
  source_number = c(rep(1,7), rep(80,7), rep(103,7), rep(250,7), rep(380,7), 
                    rep(590,7), rep(711,7)),
  source_name = c(rep("Urban Fabric",7), 
                  rep("Complex Agriculture",7), 
                  rep("Agriculture & Vegetation",7),
                  rep("Forests",7), 
                  rep("Moors, Heath & Grass",7),
                  rep("Transitional Woodland Shrub",7),
                  rep("Sparse Vegetation",7)),
  target_number = c(rep(c(1,80,103,250,380,590,711), 7)),
  target_name = c(rep(c("Urban Fabric", "Complex Agriculture", "Agriculture & Vegetation",
                        "Forests", "Moors, Heath & Grass", "Transitional Woodland Shrub",
                        "Sparse Vegetation"), 7)),
  difference = c(rep(0,7), rep(79,7), rep(102,7), rep(249,7), rep(379,7), 
                 rep(589,7), rep(710,7)))

# Calculate and visualise changes for each period
periods <- list(
  list(start = 1, end = 2, source_year = "2000", target_year = "2006"),
  list(start = 3, end = 4, source_year = "2006", target_year = "2012"),
  list(start = 5, end = 6, source_year = "2012", target_year = "2018"),
  list(start = 1, end = 6, source_year = "2000", target_year = "2018"))

for (period in periods) {
  change_layer <- norway_corine_change_modified_stack[[period$start]] - norway_corine_change_modified_stack[[period$end]]
  
  sankey_data <- prepare_sankey_data(change_layer, class_meaning, period$source_year, period$target_year)
  
  # Plot Sankey data outside the function
  sankey_data_long <- sankey_data %>%
    pivot_longer(cols = c(source, target), names_to = "key", values_to = "value") %>%
    group_by(key) %>%
    mutate(node = value) %>%
    ungroup() %>%
    arrange(key) %>%
    mutate(x = as.integer(factor(key, levels = c("source", "target")))) %>%
    group_by(node) %>%
    mutate(next_node = lead(node, order_by = x)) %>%
    ungroup()
  
  unique_nodes <- unique(c(sankey_data_long$node, sankey_data_long$next_node))
  num_nodes <- length(unique_nodes)
  palette <- hue_pal()(num_nodes)
  
  ggplot(sankey_data_long, aes(x = x, next_x = x + 1, node = node, next_node = next_node, fill = factor(node), label = node)) +
    geom_sankey(flow.alpha = 0.5, show.legend = FALSE) +
    geom_sankey_label(size = 3, color = "black", hjust = 0.5) +
    theme_void() +
    scale_fill_manual(values = palette) +
    theme(legend.position = "none")
    #ggsave(here("figures", paste0("network_", period$source_year, ".", period$target_year, "_all_classes.svg")), width = 10, height = 5.37)
  
  # Calculate and prepare data for bar plots
  change_meaning <- merge(as.data.frame(freq(change_layer)), class_meaning, by.x = "value", by.y = "difference")
  
  loss_data <- change_meaning |>
    filter(value != 0) |>
    mutate(count = count * (-0.01),
           focus = source_name,
           transition = target_name) |>
    select(focus, transition, count)
  
  gain_data <- change_meaning |>
    filter(value != 0) |>
    mutate(count = count * 0.01,
           focus = target_name,
           transition = source_name) |>
    select(focus, transition, count)
  
  gain_loss_data <- rbind(loss_data, gain_data)
  
  create_bar_plot(gain_loss_data, paste0("gain_loss_", period$source_year, ".", period$target_year, "_dual_y_axis.svg"), 
                  paste0("Land Cover Transitions ", period$source_year, " - ", period$target_year))
}



# 3. LAND COVER TRANSITIONS 2000 - 2006 ----

# Calculate change between 2000 and 2006
corine_2000_2006_df <- as.data.frame(freq(norway_corine_change_modified_stack[[1]] -
                                            norway_corine_change_modified_stack[[2]])) |>
  mutate(source_year = "2000",
         target_year = "2006",
         difference = value) |>
  select(-layer)

## 3.1. Define land cover categories ----

# Create dataframe to map numerical values to land cover categories
 # source numbers
source_number <- c(rep(1,7), rep(80,7), rep(103,7),
                   rep(250,7), rep(380,7), rep(590,7),
                   rep(711,7))
 # source name column 
source_name <- c(rep("Urban Fabric",7), rep("Complex Agriculture",7), 
                 rep("Agriculture & Vegetation",7), rep("Forests",7), 
                 rep("Moors, Heath & Grass",7), rep("Transitional Woodland Shrub",7),
                 rep("Sparse Vegetation",7))
 # target "value" column
target_number <- c(rep(c(1,80,103,250,380,590,711), 7))
 # target name column
target_name <- c(rep(c("Urban Fabric", "Complex Agriculture", "Agriculture & Vegetation",
                       "Forests", "Moors, Heath & Grass", "Transitional Woodland Shrub",
                       "Sparse Vegetation"), 7))

# Combine vectors in df  
corine_class_meaning <- data.frame(source_number, source_name,
                                   target_number, target_name) |>
  mutate(difference = source_number - target_number)

## 3.2. Prepare data for Sankey Plots ----

# Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning <- corine_class_meaning |>
  filter(difference %in% corine_2000_2006_df$value) |>
  # change column names
  rename(value = difference)

# Merge norway_corine_class_meaning df and corine_2000_2006_df into one
corine_2000_2006_change_meaning <- merge(corine_2000_2006_df,
                                         norway_corine_class_meaning,
                                         by = "value")
# Prepare dataframe for sankey plot
corine_2000_2006_sankey <- corine_2000_2006_change_meaning |>
  # merge columns "source_year" with "source_name" and "target_year" with "target_name" to differentiate the transitions
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE) |>
  # remove the rows that show "no change" (i.e. value = 0)
  filter(value != 0) |>
  # remove columns value, layer, source_year, target_year, source_number, source_name, target_number, target_name
  select(count, source, target) |>
  # re-arrange columns in the order: source, target, count
  relocate(source, target, count) |>
  # change values in target so that they are different from the source ones (add a space at the end)
  mutate(target = paste(target, " ", sep = ""))

## 3.3. Sankey plot for transitions between 2000 and 2006 - all classes ----

# Create node dataframe
nodes2000_2006 <- data.frame(name = c(as.character(corine_2000_2006_sankey$source),
                                      as.character(corine_2000_2006_sankey$target)) |>
                               unique())

# Create ID to provide connection for networkD3
corine_2000_2006_sankey$IDsource=match(corine_2000_2006_sankey$source, nodes2000_2006$name)-1 
corine_2000_2006_sankey$IDtarget=match(corine_2000_2006_sankey$target, nodes2000_2006$name)-1

# Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = corine_2000_2006_sankey, Nodes = nodes2000_2006,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

# Save the Network
svg(here("figures", "network_2000.2006_all_classes.svg"))
dev.off()

## 3.4. Sankey Plot for transitions between 2000 and 2006 (excluding coniferous forest to transitional woodland shrub) ----

# The transitions conigerous forest -> transitional woodland shrub (and vice versa) were removed to allow better visualisation of the other transitions (which are not dominant)
# Remove rows 3 and 26
forestless_2000_2006_sankey <- corine_2000_2006_sankey |>
  filter(!row_number() %in% c(3, 12))

# Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Create node dataframe
nodes_forestless <- data.frame(name = c(as.character(forestless_2000_2006_sankey$source),
                                        as.character(forestless_2000_2006_sankey$target)) |>
                                 unique())

# Reformat for ID
forestless_2000_2006_sankey$IDsource = match(forestless_2000_2006_sankey$source,
                                             nodes_forestless$name) - 1
forestless_2000_2006_sankey$IDtarget = match(forestless_2000_2006_sankey$target,
                                             nodes_forestless$name) - 1

# Make Network
sankeyNetwork(Links = forestless_2000_2006_sankey, Nodes = nodes_forestless,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

# Save the Network
svg(here("figures", "network_2000.2006_forestless.svg"))

dev.off()

## 3.5. Barplots of land cover transitions between 2000 and 2006 ----

# Create dataframe of "transition to" values
 # this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
 # the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2000_2006 <- corine_2000_2006_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

# Create dataframe of "transitions from" values
 # this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
 # the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2000_2006 <- corine_2000_2006_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


# Merge the gain and loss dataframes into a single df
gain_loss_2000_2006 <- rbind(loss_2000_2006, gain_2000_2006)

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_2000_2006$scaled_count <- ifelse(abs(gain_loss_2000_2006$count) > 90,
                                           gain_loss_2000_2006$count/scaling_factor,
                                           gain_loss_2000_2006$count)

# Plot the data
ggplot(gain_loss_2000_2006, aes(x = focus, y = scaled_count,
                                fill = transition))+
  geom_bar(stat="identity", position="stack")+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                               "#6A3D9A", "#FF7F00",
                               "gold1","maroon"),
                    name = "Land Cover Classes",
                    labels = c("Agriculture & Vegetation", "Complex Agriculture",
                               "Forests", "Moors, Heathland & Grassland",
                               "Sparse Vegetation", "Transitional Woodland Shrub",
                               "Urban Fabric"))+
  scale_x_discrete(labels = c("Agriculture & Vegetation", "Complex Agriculture",
                              "Forests", "Moors, Heathland & Grassland",
                              "Sparse Vegetation", "Transitional Woodland Shrub",
                              "Urban Fabric"))+
  ggtitle("Land Cover Transitions 2000 - 2006")+
  geom_hline(yintercept = 0)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1))

# Save Plot
ggsave(here("figures", "gain_loss_2000.2006_dual_y_axis.svg"),
       width = 10, height = 5.37)

# 4. LAND COVER TRANSITIONS BETWEEN 2006 AND 2012 ----

## 4.1. Calculate change between 2006 and 2012 ----

# Extract values for the difference between 2000 and 2006 as df
corine_2006_2012_df <- as.data.frame(freq(norway_corine_change_modified_stack[[3]] -
                                            norway_corine_change_modified_stack[[4]])) |>
  mutate(source_year = "2006",
         target_year = "2012",
         difference = value) |>
  select(-layer)

## 4.2. Get values for source and target land cover in the change layer from the score meaning data frame ----

# Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2006_2012 <- corine_class_meaning |>
  filter(difference %in% corine_2006_2012_df$value)

# Change column names
colnames(norway_corine_class_meaning_2006_2012) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


# Merge norway_corine_class_meaning_2006_2012 df and corine_2006_2012_df into one
corine_2006_2012_change_meaning <- merge(corine_2006_2012_df,
                                         norway_corine_class_meaning_2006_2012,
                                         by = "value")

## 4.3. Prepare data for Sankey Plots ----

# Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2006_2012_sankey <- corine_2006_2012_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE) |>
  # remove the rows that show "no change" (i.e. value = 0)
  filter(value != 0) |>
  # remove columns value, layer, source_year, target_year, source_number, source_name, target_number, target_name
  select(count, source, target) |>
  # re-arrange columns in the order: source, target, count
  relocate(source, target, count) |>
  # change values in target so that they are different from the source ones (add a space at the end)
  mutate(target = paste(target, " ", sep = ""))

## 4.4. Sankey Plot for transitions between 2006 and 2012 (all classes included) ----

# Create node dataframe
nodes2006_2012 <- data.frame(name = c(as.character(corine_2006_2012_sankey$source),
                                      as.character(corine_2006_2012_sankey$target)) |>
                               unique())

# Create ID to provide connection for networkD3
corine_2006_2012_sankey$IDsource=match(corine_2006_2012_sankey$source, nodes2006_2012$name)-1 

corine_2006_2012_sankey$IDtarget=match(corine_2006_2012_sankey$target, nodes2006_2012$name)-1

# Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = corine_2006_2012_sankey, Nodes = nodes2006_2012,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

# Save the Network
svg(here("figures", "network_2006.2012_all_classes.svg"))

dev.off()

## 4.5. Sankey Plot for transitions between 2006 and 2012 (excluding coniferous forest to transitional woodland shrub) ----

# Remove rows 25 and 90
forestless_2006_2012_sankey <- corine_2006_2012_sankey |>
  filter(!row_number() %in% c(6, 29))

# Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Create node dataframe
nodes_forestless2006_2012 <- data.frame(name = c(as.character(forestless_2006_2012_sankey$source),
                                                 as.character(forestless_2006_2012_sankey$target)) |>
                                          unique())

# Reformat for ID
forestless_2006_2012_sankey$IDsource = match(forestless_2006_2012_sankey$source,
                                             nodes_forestless2006_2012$name) - 1

forestless_2006_2012_sankey$IDtarget = match(forestless_2006_2012_sankey$target,
                                             nodes_forestless2006_2012$name) - 1

# Make Network
sankeyNetwork(Links = forestless_2006_2012_sankey, Nodes = nodes_forestless2006_2012,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

# Save the Network
svg(here("figures", "network_2006.2012_forestless.svg"))
dev.off()

## 4.6. Barplots of land cover transitions between 2006 and 2012 ----

# Create dataframe of "transition to" values
  # this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
  # the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2006_2012 <- corine_2006_2012_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

# Create dataframe of "transitions from" values
  # this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
  # the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2006_2012 <- corine_2006_2012_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)

# Merge the gain and loss dataframes into a single df
gain_loss_2006_2012 <- rbind(loss_2006_2012, gain_2006_2012)

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_2006_2012$scaled_count <- ifelse(abs(gain_loss_2006_2012$count) > 90,
                                           gain_loss_2006_2012$count/scaling_factor,
                                           gain_loss_2006_2012$count)

# Plot the data
ggplot(gain_loss_2006_2012, aes(x = focus, y = scaled_count,
                                fill = transition))+
  geom_bar(stat="identity", position="stack")+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                               "#6A3D9A", "#FF7F00",
                               "gold1","maroon"),
                    name = "Land Cover Classes",
                    labels = c("Agriculture & Vegetation", "Complex Agriculture",
                               "Forests", "Moors, Heathland & Grassland",
                               "Sparse Vegetation", "Transitional Woodland Shrub",
                               "Urban Fabric"))+
  scale_x_discrete(labels = c("Agriculture & Vegetation", "Complex Agriculture",
                              "Forests", "Moors, Heathland & Grassland",
                              "Sparse Vegetation", "Transitional Woodland Shrub",
                              "Urban Fabric"))+
  ggtitle("Land Cover Transitions 2006 - 2012")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1))

# Save Plot
ggsave(here("figures", "gain_loss_2006.2012_dual_y_axis.svg"),
       width = 10, height = 5.37)

# 5. LAND COVER TRANSITIONS BETWEEN 2012 AND 2018 ----
## 5.1. Calculate change between 2012 and 2018 ----

# Extract values for the difference between 2012 and 2018 as df
corine_2012_2018_df <- as.data.frame(freq(norway_corine_change_modified_stack[[5]] -
                                            norway_corine_change_modified_stack[[6]])) |>
  mutate(source_year = "2012",
         target_year = "2018",
         difference = value) |>
  select(-layer)

## 5.2. Get values for source and target land cover in the change layer from the score meaning data frame ----

# Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2012_2018 <- corine_class_meaning |>
  filter(difference %in% corine_2012_2018_df$value)

# Change column names
colnames(norway_corine_class_meaning_2012_2018) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


#Merge norway_corine_class_meaning_2012_2018 df and corine_2012_2018_df into one
corine_2012_2018_change_meaning <- merge(corine_2012_2018_df,
                                         norway_corine_class_meaning_2012_2018,
                                         by = "value")

## 5.3. Prepare data for Sankey Plots ----

# Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2012_2018_sankey <- corine_2012_2018_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE) |>
  # remove the rows that show "no change" (i.e. value = 0)
  filter(value != 0) |>
  # remove columns value, layer, source_year, target_year, source_number, source_name, target_number, target_name
  select(count, source, target) |>
  # re-arrange columns in the order: source, target, count
  relocate(source, target, count) |>
  # change values in target so that they are different from the source ones (add a space at the end)
  mutate(target = paste(target, " ", sep = ""))

## 5.4. Sankey Plot for transitions between 2012 and 2018 (all classes included) ----

# Create node dataframe
nodes2012_2018 <- data.frame(name = c(as.character(corine_2012_2018_sankey$source),
                                      as.character(corine_2012_2018_sankey$target)) |>
                               unique())

# Create ID to provide connection for networkD3
corine_2012_2018_sankey$IDsource=match(corine_2012_2018_sankey$source, 
                                       nodes2012_2018$name)-1 

corine_2012_2018_sankey$IDtarget=match(corine_2012_2018_sankey$target, 
                                       nodes2012_2018$name)-1

# Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = corine_2012_2018_sankey, Nodes = nodes2012_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

# Save the Network
svg(here("figures", "network_2012.2018_all_classes.svg"))

dev.off()

## 5.5. Sankey Plot for transitions between 2012 and 2018 (excluding coniferous forest to transitional woodland shrub) ----

# Remove rows 16 and 76
forestless_2012_2018_sankey <- corine_2012_2018_sankey |>
  filter(!row_number() %in% c(4, 26))

# Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Create node dataframe
nodes_forestless2012_2018 <- data.frame(name = c(as.character(forestless_2012_2018_sankey$source),
                                                 as.character(forestless_2012_2018_sankey$target)) |>
                                          unique())

# Reformat for ID
forestless_2012_2018_sankey$IDsource = match(forestless_2012_2018_sankey$source,
                                             nodes_forestless2012_2018$name) - 1

forestless_2012_2018_sankey$IDtarget = match(forestless_2012_2018_sankey$target,
                                             nodes_forestless2012_2018$name) - 1

# Make Network
sankeyNetwork(Links = forestless_2012_2018_sankey, Nodes = nodes_forestless2012_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

# Save the Network
svg(here("figures", "network_2012.2018_forestless.svg"))

dev.off()

## 5.6. Barplots of land cover transitions between 2012 and 2018 ----

# Create dataframe of "transition to" values
  # this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
  # the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2012_2018 <- corine_2012_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

# Create dataframe of "transitions from" values
  # this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
  # the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2012_2018 <- corine_2012_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


# Merge the gain and loss dataframes into a single df
gain_loss_2012_2018 <- rbind(loss_2012_2018, gain_2012_2018)

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_2012_2018$scaled_count <- ifelse(abs(gain_loss_2012_2018$count) > 90,
                                           gain_loss_2012_2018$count/scaling_factor,
                                           gain_loss_2012_2018$count)

# Plot the data
ggplot(gain_loss_2012_2018, aes(x = focus, y = scaled_count,
                                fill = transition))+
  geom_bar(stat="identity", position="stack")+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                               "#6A3D9A", "#FF7F00",
                               "#a67b5b","maroon"),
                    name = "Land Cover Classes",
                    labels = c("Agriculture & Vegetation", "Complex Agriculture",
                               "Forests", "Moors, Heathland & Grassland",
                               "Sparse Vegetation", "Transitional Woodland Shrub",
                               "Urban Fabric"))+
  scale_x_discrete(labels = c("Agriculture & Vegetation", "Complex Agriculture",
                              "Forests", "Moors, Heathland & Grassland",
                              "Sparse Vegetation", "Transitional Woodland Shrub",
                              "Urban Fabric"))+
  ggtitle("Land Cover Transitions 2012 - 2018")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1))

# Save Plot
ggsave(here("figures", "gain_loss_2012.2018_dual_y_axis.svg"),
       width = 10, height = 5.37)

# 6. LAND COVER TRANSITIONS BETWEEN 2000 AND 2018 ----
## 6.1. Calculate change between 2000 and 2018 ----
corine_2000_2018_df <- as.data.frame(freq(norway_corine[[1]] - 
                                            norway_corine[[4]])) |>
  mutate(source_year = "2000",
         target_year = "2018",
         difference = value) |>
  select(-layer)

## 6.2. Get values for source and target land cover in the change layer from the score meaning data frame ----

# Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning_2000_2018 <- corine_class_meaning |>
  filter(difference %in% corine_2000_2018_df$value)

# Change column names
colnames(norway_corine_class_meaning_2000_2018) <- c("source_number", "source_name",
                                                     "target_number", "target_name",
                                                     "value")


# Merge norway_corine_class_meaning_2012_2018 df and corine_2012_2018_df into one
corine_2000_2018_change_meaning <- merge(corine_2000_2018_df,
                                         norway_corine_class_meaning_2000_2018,
                                         by = "value")

## 6.3. Prepare data for Sankey Plot ----

# Merge columns "source_year" with "source_name" and "target_year" with "target_name" so that we can differentiate the transitions
corine_2000_2018_sankey <- corine_2000_2018_change_meaning |>
  unite(source, c(source_year, source_name), sep = ".",
        remove = FALSE) |>
  unite(target, c(target_year, target_name), sep = ".",
        remove = FALSE) |>
  # remove the rows that show "no change" (i.e. value = 0)
  filter(value != 0) |>
  # remove columns value, layer, source_year, target_year, source_number, source_name, target_number, target_name
  select(count, source, target) |>
  # re-arrange columns in the order: source, target, count
  relocate(source, target, count) |>
  # change values in target so that they are different from the source ones (add a space at the end)
  mutate(target = paste(target, " ", sep = ""))

## 6.4. Sankey Plot for transitions between 2006 and 2018 (all classes included) ----

# Create node dataframe
nodes2000_2018 <- data.frame(name = c(as.character(corine_2000_2018_sankey$source),
                                      as.character(corine_2000_2018_sankey$target)) |>
                               unique())

# Create ID to provide connection for networkD3
corine_2000_2018_sankey$IDsource=match(corine_2000_2018_sankey$source, nodes2000_2018$name)-1 

corine_2000_2018_sankey$IDtarget=match(corine_2000_2018_sankey$target, nodes2000_2018$name)-1

# Prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Make the Network
sankeyNetwork(Links = corine_2000_2018_sankey, Nodes = nodes2000_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=40, fontSize=11, nodePadding=20)

# Save the Network
svg(here("figures", "network_2000.2018_all_classes.svg"))

dev.off()

## 6.5. Sankey Plot for transitions between 2006 and 2018 (excluding coniferous forest to transitional woodland shrub) ----

# Remove rows 27 and 99
forestless_2000_2018_sankey <- corine_2000_2018_sankey |>
  filter(!row_number() %in% c(6, 29))

# Colour scale
ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'

# Create node dataframe
nodes_forestless2000_2018 <- data.frame(name = c(as.character(forestless_2000_2018_sankey$source),
                                                 as.character(forestless_2000_2018_sankey$target)) |>
                                          unique())

# Reformat for ID
forestless_2000_2018_sankey$IDsource = match(forestless_2000_2018_sankey$source,
                                             nodes_forestless2000_2018$name) - 1

forestless_2000_2018_sankey$IDtarget = match(forestless_2000_2018_sankey$target,
                                             nodes_forestless2000_2018$name) - 1

# Make Network
sankeyNetwork(Links = forestless_2000_2018_sankey, Nodes = nodes_forestless2000_2018,
              Source = "IDsource", Target = "IDtarget",
              Value = "count", NodeID = "name", 
              colourScale=ColourScal, nodeWidth=20, fontSize=11, nodePadding=25)

# Save the Network
svg(here("figures", "network_2000.2018_forestless.svg"))

dev.off()

## 6.6. Barplots of land cover transitions between 2000 and 2018 ----

# Create dataframe of "transition to" values
  # this dataframe will have the "focus" cover class in the "source" column and the cover class it transitions to in the "target" column
  # the values will be negative because for every source class, this is the area "lost" that is converted to the "target" cover class
loss_2000_2018 <- corine_2000_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * (-0.01),
         focus = source_name,
         transition = target_name) |>
  select (focus, transition, count)

# Create dataframe of "transitions from" values
  # this dataframe will have "focus" cover class in the "target" column and the cover class it transitions from in the "target" column
  # the values will be positive because for every source class, this is the area "gained" that is converted from the "source" cover clas
gain_2000_2018 <- corine_2000_2018_change_meaning |>
  filter(value != 0) |>
  mutate(count = count * 0.01,
         focus = target_name,
         transition = source_name) |>
  select (focus, transition, count)


# Merge the gain and loss dataframes into a single df
gain_loss_2000_2018 <- rbind(loss_2000_2018, gain_2000_2018)

# Set scaling factor
scaling_factor <- 10

# Create new column in df to scale down the large values
gain_loss_2000_2018$scaled_count <- ifelse(abs(gain_loss_2000_2018$count) > 90,
                                           gain_loss_2000_2018$count/scaling_factor,
                                           gain_loss_2000_2018$count)

# Plot the data
ggplot(gain_loss_2000_2018, aes(x = focus, y = scaled_count,
                                fill = transition))+
  geom_bar(stat="identity", position="stack")+
  scale_y_continuous(
    name = bquote("Area changes"~("km"^2)),
    sec.axis = sec_axis(~ . * scaling_factor, name = bquote("Area changes"~("km"^2)))
  )+
  xlab("Land Cover Classes")+
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C","green4",
                               "#6A3D9A", "#FF7F00",
                               "gold1","maroon"),
                    name = "Land Cover Classes",
                    labels = c("Agriculture & Vegetation", "Complex Agriculture",
                               "Forests", "Moors, Heathland & Grassland",
                               "Sparse Vegetation", "Transitional Woodland Shrub",
                               "Urban Fabric"))+
  scale_x_discrete(labels = c("Agriculture & Vegetation", "Complex Agriculture",
                              "Forests", "Moors, Heathland & Grassland",
                              "Sparse Vegetation", "Transitional Woodland Shrub",
                              "Urban Fabric"))+
  ggtitle("Land Cover Transitions 2000 - 2018")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30,
                                   hjust = 1))

# Save Plot
ggsave(here("figures", "gain_loss_2000.2018_dual_y_axis.svg"),
       width = 10, height = 5.37)

# END OF SCRIPT ----