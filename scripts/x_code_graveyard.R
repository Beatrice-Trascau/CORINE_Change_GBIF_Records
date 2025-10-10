##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# x_code_graveyard
# This script contains discarded code that may still be useful later
# and should not be set to run
##----------------------------------------------------------------------------##

# 1. CODE FROM SCRIPT 1.2_CORINE_LAYERS_EXPLORATION ----------------------------


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

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_corine_change_layer_and_ssb_grids
# This script contains code which combines the CORINE land cover CHANGE layers
# with the ssb administrative grids for future analyses and GBIF occurrences
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. LOAD DATA ----

# Add download link from box
# norway_corine_change_stack <- ("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif")
# ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")
# occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")

# Download files
# download.file(norway_corine_change_stack, here("data", 
# "norway_corine_change_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
# "ruter500m_Norge.zip"))

# download.file(cleaned_occurrences, here("data","cleaned_occurrences.txt"))

# Read in the data
norway_corine_change_modified_stack <- rast(here("data", 
                                                 "norway_corine_change_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data", "ruter500m_Norge.shp"))

occurrences_norway <- fread(here("data", "cleaned_occurrences.txt"))

# 2. EXPLORE LAYERS ----

# Plot SSB grids
plot(ssb_grids)

# Check CORINE values
levels(as.factor(as.data.frame(corine_2000_wgs84)$U2006_CHA0006_00_V2020_20u1))

## 2.1. Re-project layers to match occurrences ----

# Check projection of SSB grids
crs(ssb_grids, proj = TRUE) # "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

# Re-project SSB grids to match the projection of occurrences
norway_ssb_grids <- terra::project(ssb_grids,
                                   "+proj=longlat +datum=WGS84 +no_defs")

# Re-project CORINE to match the projection of occurrences
corine_wgs84 <- project(norway_corine_change_modified_stack, 
                        "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Check that projections match
crs(norway_ssb_grids, proj = TRUE) # "+proj=longlat +datum=WGS84 +no_defs"
crs(corine_wgs84[[1]], proj = TRUE) #"+proj=longlat +datum=WGS84 +no_defs"

# Check values for reprojected CORINE layer
levels(as.factor(as.data.frame(corine_wgs84[[1]])$U2006_CHA0006_00_V2020_20u1))


# 3. ADD GBIF RECORDS ----

# Extract cell ID, SSB grid value and land cover value for each occurrence

## 3.1. Convert occurrences to spatial dataframe ----
occurrences_sp <- st_as_sf(occurrences_norway, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(corine_wgs84))

# Convert occurrences to spatial vector
occurrences_vect <- vect(occurrences_sp)

## 3.2. Extract cell information for each occurrence ----

#Create additional layer with unique ID for the CORINE layers
ID_raster <- corine_wgs84[[1]]
values(ID_raster) <- 1:ncell(corine_wgs84[[1]])

# Combine ID raster with CORINE
corine_ID <- c(corine_wgs84[[1]], ID_raster)
corine_ID_all_layers <- c(corine_wgs84, ID_raster)

# Extract raster values for occurrences and SSB IDs
corine_ID_occurrences <- terra::extract(corine_ID, occurrences_vect)
corine_ID_SSBs <- terra::extract(corine_ID, norway_ssb_grids)

# Add extracted values to spatial dataframe
occurrences_vect$land_cover <- corine_ID_occurrences[,2]
occurrences_vect$cell_ID <- corine_ID_occurrences[,3]
occurrences_vect$SSBs <- corine_ID_SSBs[,2]

# Write dataframe
saveRDS(occurrences_vect, here("data", "occurrences_vect.rds"))

# Do extraction for all corine layers at a time and make into df
corine_ID_all_layers_occurrences <- terra::extract(corine_ID_all_layers, occurrences_vect)
corine_ID_all_layers_SSBs <- terra::extract(corine_ID_all_layers, norway_ssb_grids)

saveRDS(corine_ID_all_layers_occurrences, here("data", "corine_ID_all_layers_occurrences.rds"))

corine_ID_all_layers_occurrences_df <- terra::extract(corine_ID_all_layers, occurrences_vect,
                                                      df =  TRUE)
write.csv(corine_ID_all_layers_occurrences_df, here("data",
                                                    "corine_ID_all_layers_occurrences_df.csv"))

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.1_compare_occurrences_corine_status
# This script compares the number of occurrence records in CORINE land cover
# STATUS pixels that change land cover through time to those that do not change
##----------------------------------------------------------------------------##

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

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.2_corine_change_layer_and_ssb_grids
# This script contains code which compares the number of 
# species richness between CORINE pixels that are (not)changing and SSB IDs
##----------------------------------------------------------------------------##

# 2. COMPARE SPECIES RICHNESS BETWEEN CHANGED AND UNCHANGED PIXELS ----

## 2.1. 2000-2006 ----

### 2.1.1. Before change (1997-2000) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_1997_2000 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate species richness for each SSB id for the period 1997-2000 (before change)
occ_df_1997_2000_richness <- occ_df_1997_2000 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_1997_2000 = n_distinct(species),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2000.2006 <- occ_df_1997_2000_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2000.2006 <- ssb_ids_with_both_before_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2000.2006 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2000_2006_richness <- occ_df_1997_2000_richness |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2000_2006_richness$land_cover2000 <- factor(filtered_occ_df_before2000_2006_richness$land_cover2000,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2000_2006 <- filtered_occ_df_before2000_2006_richness |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2000_2006, 
       aes(x = cover_change, y = species_richness_1997_2000, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.1.2. After change (2006-2009) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2006_2009 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2006_2009_richness <- occ_df_2006_2009 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2006_2009 = n_distinct(species),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2000.2006 <- occ_df_2006_2009_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2000.2006 <- ssb_ids_with_both_after_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2000.2006 <- ssbid_counts_after_2000.2006 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2000_2006_richness <- occ_df_2006_2009_richness |>
  filter(SSBid %in% top_ssbids_after_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2000_2006_richness$land_cover2000 <- factor(filtered_occ_df_after2000_2006_richness$land_cover2000,
                                                                 levels = names(land_cover_mapping),
                                                                 labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2000_2006 <- filtered_occ_df_after2000_2006_richness |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2000_2006, 
       aes(x = cover_change, y = species_richness_2006_2009, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

## 2.2. 2006-2012 ----

### 2.2.1. Before change (2003-2006) for biodiversity records ----

# Prep dataframe: subset for 2000-2006, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2003_2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate species richness for each SSB id for the period 2003-2006 (before change)
occ_df_2003_2006_richness <- occ_df_2003_2006 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2003_2006 = n_distinct(species),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2006.2012 <- occ_df_2003_2006_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2006.2012 <- ssb_ids_with_both_before_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2006.2012 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2006_2012_richness <- occ_df_2003_2006_richness |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2006_2012_richness$land_cover2006 <- factor(filtered_occ_df_before2006_2012_richness$land_cover2006,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2006_2012 <- filtered_occ_df_before2006_2012_richness |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2006_2012, 
       aes(x = cover_change, y = species_richness_2003_2006, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.2.2. After change (2012-2015) for biodiversity records ----

# Prep dataframe: subset for 2006-2012, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2012_2015 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2012_2015_richness <- occ_df_2012_2015 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2012_2015 = n_distinct(species),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2006.2012 <- occ_df_2012_2015_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2006.2012 <- ssb_ids_with_both_after_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2006.2012 <- ssbid_counts_after_2006.2012 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2006_2012_richness <- occ_df_2012_2015_richness |>
  filter(SSBid %in% top_ssbids_after_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2006_2012_richness$land_cover2006 <- factor(filtered_occ_df_after2006_2012_richness$land_cover2006,
                                                                 levels = names(land_cover_mapping),
                                                                 labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2006_2012 <- filtered_occ_df_after2006_2012_richness |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2006_2012, 
       aes(x = cover_change, y = species_richness_2012_2015, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

## 2.3. 2012-2018 ----

### 2.3.1. Before change (2009-2012) for biodiversity records ----

# Prep dataframe: subset for 2012-2018, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2009_2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate species richness for each SSB id for the period 2003-2006 (before change)
occ_df_2009_2012_richness <- occ_df_2009_2012 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2009_2012 = n_distinct(species),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2012.2018 <- occ_df_2009_2012_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_before_2012.2018 <- ssb_ids_with_both_before_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_before_2012.2018 <- ssbid_counts_before_2012.2018 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_before2012_2018_richness <- occ_df_2009_2012_richness |>
  filter(SSBid %in% top_ssbids_before_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_df_before2012_2018_richness$land_cover2012 <- factor(filtered_occ_df_before2012_2018_richness$land_cover2012,
                                                                  levels = names(land_cover_mapping),
                                                                  labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before2012_2018 <- filtered_occ_df_before2012_2018_richness |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before2012_2018, 
       aes(x = cover_change, y = species_richness_2009_2012, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

### 2.3.2. After change (2015-2018) for biodiversity records ----

# Prep dataframe: subset for 2012-2018, exclude NA land cover, remove unnecessary columns, add cover change? column,
occ_df_2015_2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate species richness for each SSB id for the period 2006-2009 (after change)
occ_df_2015_2018_richness <- occ_df_2015_2018 |>
  group_by(cell_ID) |>
  summarise(
    species_richness_2015_2018 = n_distinct(species),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid))

# Identify SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2012.2018 <- occ_df_2015_2018_richness |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique corine_cell_IDs for each SSBid
ssbid_counts_after_2012.2018 <- ssb_ids_with_both_after_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_corine_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique corine_cell_ID values
top_ssbids_after_2012.2018 <- ssbid_counts_after_2012.2018 |>
  arrange(desc(unique_corine_count)) |>
  slice_head(n = 5)

# Filter dataframe to only includes rows corresponding to the top 10 SSBids
filtered_occ_df_after2012_2018_richness <- occ_df_2015_2018_richness |>
  filter(SSBid %in% top_ssbids_after_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_df_after2012_2018_richness$land_cover2012 <- factor(filtered_occ_df_after2012_2018_richness$land_cover2012,
                                                                 levels = names(land_cover_mapping),
                                                                 labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after2012_2018 <- filtered_occ_df_after2012_2018_richness |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after2012_2018, 
       aes(x = cover_change, y = species_richness_2015_2018, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs( x = "Cover Change", 
        y = "Species Richness") +
  theme_minimal()

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.3_corine_change_layer_and_ssb_grids
# This script contains code which compares the number of occurrence records 
# between CORINE pixels that are (not)changing and SSB IDs
##----------------------------------------------------------------------------##

# 0. PACKAGES ----
library(here)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. LOAD DATA ----

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

# Map land cover values to their names
land_cover_mapping <- setNames(c("Urban", "Complex Agriculture", "Agriculture & Natural Vegetation", 
                                 "Forests", "Moors & Grasslands", "Transitional Woodland Shrub", 
                                 "Sparsely Vegetated Areas"), 
                               c(1, 80, 103, 250, 380, 590, 711))

# 2. COMPARE THE NUMBER OF OCCURRENCES BETWEEN CHANGED AND UNCHANGED PIXELS ----

## 2.1. 2000-2006 ----

### 2.1.1. Before change (1997-2000) for biodiversity records ----

# Prepare data for the period 1997-2000
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    num_records_1997_2000 = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2000.2006 <- occ_df_before_2000.2006_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2000.2006 <- ssb_ids_with_both_before_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2000.2006 <- ssbid_counts_before_2000.2006 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2000.2006 <- occ_df_before_2000.2006_records |>
  filter(SSBid %in% top_ssbids_before_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2000.2006$land_cover2000 <- factor(filtered_occ_records_before_2000.2006$land_cover2000,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2000.2006 <- filtered_occ_records_before_2000.2006 |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2000.2006, 
       aes(x = cover_change, y = num_records_1997_2000, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (1997-2000)") +
  theme_minimal()

### 2.1.2. After change (2006-2009) for biodiversity records ----

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2000.2006_records <- occ_df_after_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2006_2009 = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2000.2006 <- occ_df_after_2000.2006_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2000.2006 <- ssb_ids_with_both_after_2000.2006 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2000.2006 <- ssbid_counts_after_2000.2006 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2000.2006 <- occ_df_after_2000.2006_records |>
  filter(SSBid %in% top_ssbids_after_2000.2006$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2000.2006$land_cover2000 <- factor(filtered_occ_records_after_2000.2006$land_cover2000,
                                                              levels = names(land_cover_mapping),
                                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2000.2006 <- filtered_occ_records_after_2000.2006 |>
  group_by(land_cover2000, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2000.2006, 
       aes(x = cover_change, y = num_records_2006_2009, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2000 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (1997-2000)") +
  theme_minimal()

## 2.2. 2006-2012 ----

### 2.2.1. Before change (2003-2006) for biodiversity records ----

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2003_2006 = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2006.2012 <- occ_df_before_2006.2012_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2006.2012 <- ssb_ids_with_both_before_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2006.2012 <- ssbid_counts_before_2006.2012 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2006.2012 <- occ_df_before_2006.2012_records |>
  filter(SSBid %in% top_ssbids_before_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2006.2012$land_cover2006 <- factor(filtered_occ_records_before_2006.2012$land_cover2006,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2006.2012 <- filtered_occ_records_before_2006.2012 |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2006.2012, 
       aes(x = cover_change, y = num_records_2003_2006, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2003-2006)") +
  theme_minimal()

### 2.2.2. After change (2012-2015) for biodiversity records ----

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2006.2012_records <- occ_df_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2012_2015 = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2006.2012 <- occ_df_after_2006.2012_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2006.2012 <- ssb_ids_with_both_after_2006.2012 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2006.2012 <- ssbid_counts_after_2006.2012 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2006.2012 <- occ_df_after_2006.2012_records |>
  filter(SSBid %in% top_ssbids_after_2006.2012$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2006.2012$land_cover2006 <- factor(filtered_occ_records_after_2006.2012$land_cover2006,
                                                              levels = names(land_cover_mapping),
                                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2006.2012 <- filtered_occ_records_after_2006.2012 |>
  group_by(land_cover2006, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2006.2012, 
       aes(x = cover_change, y = num_records_2012_2015, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2006 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2012-2015)") +
  theme_minimal()

## 2.3. 2012-2018 ----

### 2.3.1. Before change (2009-2012) for biodiversity records ----

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2009_2012 = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_before_2012.2018 <- occ_df_before_2012.2018_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_before_2012.2018 <- ssb_ids_with_both_before_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_before_2012.2018 <- ssbid_counts_before_2012.2018 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_before_2012.2018 <- occ_df_before_2012.2018_records |>
  filter(SSBid %in% top_ssbids_before_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_records_before_2012.2018$land_cover2012 <- factor(filtered_occ_records_before_2012.2018$land_cover2012,
                                                               levels = names(land_cover_mapping),
                                                               labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_before_2012.2018 <- filtered_occ_records_before_2012.2018 |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_before_2012.2018, 
       aes(x = cover_change, y = num_records_2009_2012, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2009-2012)") +
  theme_minimal()

### 2.3.2. After change (2015-2018) for biodiversity records ----

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBid, cell_ID, TARGET_FID) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_after_2012.2018_records <- occ_df_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    num_records_2015_2018 = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBid = first(SSBid)
  )

# Filter for SSB IDs that have values for both cover_change = Y and cover_change = N
ssb_ids_with_both_after_2012.2018 <- occ_df_after_2012.2018_records |>
  group_by(SSBid) |>
  filter(all(c("Y", "N") %in% cover_change)) |>
  ungroup()

# Calculate the number of unique cell_IDs for each SSBid
ssbid_counts_after_2012.2018 <- ssb_ids_with_both_after_2012.2018 |>
  group_by(SSBid) |>
  summarise(unique_cell_count = n_distinct(cell_ID)) |>
  ungroup()

# Identify top 5 SSBids with most unique cell_ID values
top_ssbids_after_2012.2018 <- ssbid_counts_after_2012.2018 |>
  arrange(desc(unique_cell_count)) |>
  slice_head(n = 5)

# Filter dataframe to only include rows corresponding to the top SSBids
filtered_occ_records_after_2012.2018 <- occ_df_after_2012.2018_records |>
  filter(SSBid %in% top_ssbids_after_2012.2018$SSBid)

# Change values in df based on mapping
filtered_occ_records_after_2012.2018$land_cover2012 <- factor(filtered_occ_records_after_2012.2018$land_cover2012,
                                                              levels = names(land_cover_mapping),
                                                              labels = land_cover_mapping)

# Subset data based on unique combination of land cover in 2000 and SSB ID 
filtered_occ_after_2012.2018 <- filtered_occ_records_after_2012.2018 |>
  group_by(land_cover2012, SSBid) |>
  filter(n() > 1) |>
  ungroup()

# Plot comparison for each combination
ggplot(filtered_occ_after_2012.2018, 
       aes(x = cover_change, y = num_records_2015_2018, fill = cover_change)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~ land_cover2012 + SSBid, scales = "free_y", ncol = 5) +
  labs(x = "Cover Change", 
       y = "Number of Records (2015-2018)") +
  theme_minimal()

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.2_Y_N_cover_change_occ_model_offset_setup
# This script contains code which runs the models looking at the effect 
# of land cover change (Y/N) and offset on the number of occurrences in a pixel
##----------------------------------------------------------------------------##

# 0. LOAD PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(lattice)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(mgcv)
library(gamm4)
library(ggplot2)

# 1. PREPARE DATA FOR MODELS ---------------------------------------------------

# Load data
load(here("data", "derived_data", 
          "occurrences_SSB_municipalities_land_cover.rda"))

# Rename df (to make it easier to work with)
occ_SSB_land_cover <- occurrence_municipalities_df

## 1.1. First period of change: 2000-2006 --------------------------------------

# Prepare data for the period 2006-2009
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 2006-2009
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 1.2. Second period of change: 2006 - 2012 -----------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 2003-2006
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 1.3. Third period of change: 2012 - 2018 ------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 1.4. Combine the above 3 into a single df -----------------------------------

occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)


## 1.5. Combine occ_df_before_records wtih occ_df_after_records ----------------

# Read in dataframe with occurrences after a change
load(here("data", "derived_data", 
          "occ_y_n_cover_change_after_records_for_model.rda"))

# Full join of the dfs on SSBid and time_period
occ_df_before_after <- full_join(occ_y_n_cover_change_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_y_n_cover_change_before_after_for_modell <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before))


## 1.6. Write dataframe to file ------------------------------------------------
save(occ_y_n_cover_change_before_after_for_modell, 
     file = here::here("data", "derived_data",
                       "occ_y_n_cover_change_before_after_for_modell.rda"))

# 2. MODEL 1: OCC ~ COVER CHANGE + OFFSET  -------------------------------------

## 2.1. Negative binomial with glmer.nb ----------------------------------------

# Run negative binomial model
# model2.1_nb <- glmer.nb(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                         data = occ_y_n_cover_change_before_after_for_modell)
# 
# Save model output to file to save time next time
# save(model2.1_nb, file = here::here("data", "models", "model2.1_nb.RData"))

# Create a 10% subset of the data
subset_data <- occ_y_n_cover_change_before_after_for_modell |>
  sample_frac(0.1)

# Save subset
save(subset_data, 
     file = here::here("data", "derived_data", "subset_data.rda"))

# Load data subset
load(here("data", "derived_data", 
          "subset_data.rda"))

## 2.2. Negative binomial with glmer.nb on subset of data ----------------------

# Run negative binomial model
# model2.2_nb <- glmer.nb(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                         data = subset_data)

# Save model output to file to save time next time
#save(model2.2_nb, file = here::here("data", "models", "model2.2_nb.RData"))

## 2.3. N binomial with glmmTMB, family nbinom 1, by SSB ID on data subset -----

# Run negative binomial model
# model2.3_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                        family = nbinom1,
#                        data = subset_data)

# Save model output to file to save time next time
#save(model2.3_nb, file = here::here("data", "models", "model2.3_nb.RData"))


## 2.4. N binomial glmmTMB, family nbinom1, by Municipality on data subset -----

# Run negative binomial model
# model2.3_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | Municipality),
#                        family = nbinom1,
#                        data = subset_data)

# Save model output to file to save time next time
#save(model2.3_nb, file = here::here("data", "models", "model2.3_nb.RData"))

## 2.5.N binomial glmmTMB, nbinom2, SSBID on data subset -----------------------

# Run negative binomial model
model2.5_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
                       family = nbinom2,
                       data = subset_data)

# Save model output to file to save time next time
save(model2.5_nb, file = here::here("data", "models", "model2.5_nb.RData"))

# END OF SCRIPT ----------------------------------------------------------------

##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.5_cover_change_types_occ_model_offset_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change types and offset on the number of occurrences in a pixel
##----------------------------------------------------------------------------##

# 0. LOAD PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(lattice)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(mgcv)
library(gamm4)
library(ggplot2)

# 1. PREPARE DATA FOR MODELS ---------------------------------------------------

# Load data
load(here("data", "derived_data", 
          "occurrences_SSB_municipalities_land_cover.rda"))

# Rename df (to make it easier to work with)
occ_SSB_land_cover <- occurrence_municipalities_df

## 1.1. First period of change: 2000-2006 --------------------------------------

# Change column names for easier df manipulation
occ_SSB_land_cover <- occ_SSB_land_cover |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# Prepare data for the period 2006-2009
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = land_cover2000 - land_cover2006)

# Calculate number of records for the period 2006-2009
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 1.2. Second period of change: 2006 - 2012 -----------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = land_cover2006 - land_cover2012)

# Calculate number of records for the period 2003-2006
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 1.3. Third period of change: 2012 - 2018 ------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = land_cover2012 - land_cover2018)

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 1.4. Combine the above 3 into a single df -----------------------------------

occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

## 1.5. Combine occ_df_before_records wtih occ_df_after_records ----------------

# Read in dataframe with occurrences after a change
load(here("data", "derived_data", 
          "occ_cover_change_types_after_records_for_model.rda"))

# Full join of the dfs on SSBid and time_period
occ_df_before_after <- full_join(occ_cover_change_types_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_cover_change_types_before_after_for_model <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before))

## 1.6. Write dataframe to file ------------------------------------------------
save(occ_cover_change_types_before_after_for_model, 
     file = here::here("data", "derived_data",
                       "occ_cover_change_types_before_after_for_model.rda"))

# 2. MODEL 2: OCC ~ COVER CHANGE + OFFSET --------------------------------------

## 2.1. N binomial glmmTMB, nbinom 2, SSBID on data subset ---------------------

# Create 10% subset of the data
subset_data_cover_change_types_before_after <- occ_cover_change_types_before_after_for_model |>
  sample_frac(0.1)

# Save subset
save(subset_data_cover_change_types_before_after, 
     file = here::here("data", "derived_data", 
                       "subset_data_cover_change_types_before_after.rda"))

# Load data subset
# load(here("data", "derived_data", 
#           "subset_data_cover_change_types_before_after.rda"))

# Run model
model6.1 <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
                    family = nbinom2,
                    data = subset_data_cover_change_types_before_after)

# Save model output to file to save time next time
save(model6.1, file = here::here("data", "models", "model6.1.RData"))

# END OF SCRIPT ----------------------------------------------------------------

# Code from when I was trialing how to plot the heatmap with the geom_points on top of oit
# Create a named vector for label mapping
label_mapping <- c(
  "Agriculture & Vegetation" = "ASNV",
  "Complex Agriculture" = "CA",
  "Forests" = "Forests",
  "Moors, Heathland & Grassland" = "MHG",
  "Sparse Vegetation" = "SV",
  "Transitional Woodland Shrub" = "TWS",
  "Urban Fabric" = "UF"
)

# Calculate the overall y-axis limits for consistency across subplots
y_min <- min(cover_effect_no_year_interaction$Estimate - 
               cover_effect_no_year_interaction$`Std. Error`, na.rm = TRUE)
y_max <- max(cover_effect_no_year_interaction$Estimate + 
               cover_effect_no_year_interaction$`Std. Error`, na.rm = TRUE)

# Update factor levels with new labels
new_order <- c("ASNV", "CA", "Forests", "MHG", "SV", "TWS", "UF")

# Create base plot with faceting
a <- ggplot(cover_effect_no_year_interaction, 
            aes(x = 1, y = Estimate)) +
  # Create facets for each combination
  facet_grid(intial_cover ~ cover_change, switch = "y") +
  # Add horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dotted") +
  # Add points and error bars for significant estimates
  geom_pointrange(data = cover_effect_no_year_interaction %>% 
                    filter(!is.na(Significant) & !is_same),
                  aes(ymin = Estimate - Std..Error,
                      ymax = Estimate + Std..Error,
                      color = Significant),
                  size = 1) +
  # Add black background for same initial/cover change
  geom_rect(data = cover_effect_no_year_interaction %>% filter(is_same),
            aes(xmin = -Inf, xmax = Inf, 
                ymin = -Inf, ymax = Inf),
            fill = "black") +
  # Custom scale for point colors
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0, na.value = "grey90", name = "Estimate",
                        breaks = legend_breaks_cover,
                        labels = scales::label_number()) +
  # Set consistent y-axis limits
  coord_cartesian(ylim = c(y_min, y_max)) +
  # Theme customization
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_text(angle = 15, hjust = 0, size = 14),
    strip.text.y.left = element_text(size = 14, angle = 0, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)) +
  labs(y = "Estimate")


##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 6.1_supplementary_information
# This script contains code which plots and extracts information used in the
# supplementary information of the manuscript
##----------------------------------------------------------------------------##

# 1. LAND COVER CATEGORIES AS % OF TOTAL AREA ----------------------------------

# Load data
norway_corine_status_stack <- rast(here("data", "derived_data",
                                        "norway_corine_status_stack.tif"))
# Extract 2018 layer
norway_2018 <- norway_corine_status_stack[[4]]

# Get frequency table of all values
freq_table <- freq(norway_2018)

# Calculate percentages
freq_table$percentage <- round((freq_table$count / sum(freq_table$count)) * 100, 4)

# View results
print(freq_table)

# Create better table
freq_table |>
  kable(col.names = c("layer", "clc value", "pixel count", "percentage")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# Save frequency table
write.csv(freq_table, here("data", "derived_data",
                           "norway_2018_pixel_percentages.csv"), 
          row.names = FALSE)

# 2. Y/N LC CHANGE SUMMARY -----------------------------------------------------

# Load data
load(here("data", "derived_data",
          "occ_y_n_cover_change_before_after_for_modell.rda"))

## 2.1. Get summary statistics -------------------------------------------------

# Summarise data
summary_stats_y_n <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)  
  ) |>
  ungroup()

# Create a table from the data
summary_stats_y_n |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 2.2. Summary figures --------------------------------------------------------

# First panel with original data
p1 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Log-transform values
occ_y_n_cover_change_before_after_for_modell <- occ_y_n_cover_change_before_after_for_modell |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Second panel with logged data
p2 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = occ_log, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Combine plots
combined_violins_y_n <- plot_grid(p1, p2,
                                  labels = c("a)", "b)"),
                                  ncol = 2,
                                  align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_y_n_FigureS4.png"),
       width=20, height=13)

# 3. INTENSIFICATION/EXTENSIFICATION SUMMARY -----------------------------------

# Load data
load(here("data", "derived_data",
          "occ_intens_extens_before_after_for_model.rda"))

## 3.1. Summary statistics -----------------------------------------------------

# Summarise data
summary_stats_intens_extens <-  occ_intens_extens_before_after_for_model |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)) |>
  ungroup()

# Create table
summary_stats_intens_extens |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 3.2. Summary figures --------------------------------------------------------

# First panel with original data
q1 <- ggplot(occ_intens_extens_before_after_for_model, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Log-transform values
occ_intens_extens_before_after_for_model <- occ_intens_extens_before_after_for_model |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Second panel with logged data
q2 <- ggplot(occ_intens_extens_before_after_for_model, 
             aes(x = time_period, y = occ_log, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("Extensification" = "#66c2a5", 
                               "Intensification" = "sienna",
                               "No_change" = "#0072B2")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Combine plots
combined_violins_intens_extens <- plot_grid(q1, q2,
                                            labels = c("a)", "b)"),
                                            ncol = 2,
                                            align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_intens_extens_FigureS5.png"),
       width=20, height=13)

# 4. COVER CHANGE TYPES SUMMARY -------------------------------------------------

# Load data
load(here("data", "derived_data", 
          "occ_cover_change_types_before_after_for_model.rda"))

## 4.1. Summary statistics -----------------------------------------------------

# Summarise data
summary_stats_change_types <-  occ_cover_change_types_before_after_for_model |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)) |>
  ungroup()

# Create table
summary_stats_change_types |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 4.2. Summary figures --------------------------------------------------------

# Check what categories there are in the df
unique(occ_cover_change_types_before_after_for_model$cover_change)

# Log-transform values
occ_cover_change_types_before_after_for_model <- occ_cover_change_types_before_after_for_model |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Create mapping for initial land cover mappings
cover_mapping <- c(
  "urban" = "Urban",
  "complex" = "Complex Agricultural Cover",
  "agri" = "Agriculture with Significant Natural Vegetation",
  "forests" = "Forest",
  "moors" = "Moors, Heathland & Grassland",
  "woodland" = "Transitional Woodland Shrub",
  "sparse" = "Sparse Vegetation")

# Get initial land cover by splitting the cover_change column
transformed_df <- occ_cover_change_types_before_after_for_model |>
  # Split cover_change and take first element
  mutate(initial_cover = str_split_fixed(cover_change, "_", n = 2)[,1]) |>
  # Recode values using the mapping
  mutate(initial_cover = recode(initial_cover, !!!cover_mapping)) |>
  # Remove rows with "other"
  filter(!str_detect(cover_change, "other"))

# Define list of cover categories
unique_covers <- unique(transformed_df$initial_cover)

# Define list to store plots
plot_list <- list()

# Loop to create violin plots for each cover category
for(cover in unique_covers){
  plot_list[[cover]] <- transformed_df |>
    filter(initial_cover == cover) |>
    ggplot(aes(x = time_period, y = occ_log)) +
    geom_violin(fill = "#66c2a5") +
    geom_point(position = position_jitter(width = 0.1),
               alpha = 0.3, size = 1) +
    labs(x = "Time Period",
         y = "Number of Occurrences") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
}

# Calculate number of rows needed for grid
n_plots <- length(plot_list)
n_cols <- 3
n_rows <- ceiling(n_plots / n_cols)

# Create labels for plots
plot_labels <- paste0(letters[1:n_plots], ")")

# Combine plots
combined_violins_change_types <- plot_grid(plot_list[[1]], plot_list[[2]],
                                           plot_list[[3]], plot_list[[4]],
                                           plot_list[[5]], plot_list[[6]],
                                           plot_list[[7]],
                                           labels = c("a)", "b)", "c)", "d)",
                                                      "e)", "f)", "g)"),
                                           ncol = 3,
                                           align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_change_types_FigureS6.png"),
       width=20, height=13)

# 5. DATASET NAME EXPLORATIONS -------------------------------------------------
# Load data
load(here("data", "derived_data",
          "occ_y_n_cover_change_before_after_extra_info_Jan2025.rda"))

# Check number of publishers
length(unique(occ_y_n_cover_change_before_after_for_modell$publisher.x))

# Rename data for easier work
df <- occ_y_n_cover_change_before_after_for_modell


## 5.1. Plot 1 -----------------------------------------------------------------

# For cover_change = "Y"
plot_Y <- df %>%
  filter(cover_change == "Y") %>%
  group_by(publisher.x) %>%
  summarize(total_occurrences = sum(ocurrences_after)) %>%
  arrange(desc(total_occurrences)) %>%
  slice_head(n = 15) %>%  # Top 15 publishers
  ggplot(aes(x = reorder(publisher.x, total_occurrences), 
             y = total_occurrences)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Publisher",
       y = "Total Occurrences (Cover Change)") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5))

# For cover_change = "N"
plot_N <- df |>
  filter(cover_change == "N") |>
  group_by(publisher.x) |>
  summarize(total_occurrences = sum(ocurrences_after)) %>%
  arrange(desc(total_occurrences)) %>%
  slice_head(n = 15) %>%  # Top 15 publishers
  ggplot(aes(x = reorder(publisher.x, total_occurrences), 
             y = total_occurrences)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # This line removes scientific notation
  labs(x = "Publisher",
       y = "Total Occurrences (No Cover Change)") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5))

# Display plots
plot_Y
plot_N

## 5.2. Plot 2 --------------------------------------------------------------------

#Prepare the data
plot_data <- df %>%
  # Group by cover_change and publisher, get counts
  group_by(cover_change, publisher.x) %>%
  summarize(counts = sum(ocurrences_after)) %>%
  # Calculate percentage within each cover_change group
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  # Keep top publishers for each category (adjust n as needed)
  arrange(desc(percentage)) %>%
  slice_head(n = 15) %>%
  # Add a category for remaining publishers if desired
  mutate(publisher.x = factor(publisher.x, levels = rev(unique(publisher.x))))

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = publisher.x)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Paired") +  # You can change the color palette
  labs(x = "Cover Change",
       y = "Percentage",
       fill = "Publisher"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

## 5.3. Plot 3 - by dataset name -----------------------------------------------

# Prepare the data
plot_data <- df %>%
  # Group by cover_change and datasetName.x, get counts
  group_by(cover_change, datasetName.x) %>%
  summarize(counts = sum(ocurrences_after)) %>%
  # Calculate percentage within each cover_change group
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  # Keep top datasets for each category (adjust n as needed)
  arrange(desc(percentage)) %>%
  slice_head(n = 10) %>%
  # Add a category for remaining datasets if desired
  mutate(datasetName.x = factor(datasetName.x, levels = rev(unique(datasetName.x))))

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = datasetName.x)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Dataset Distribution in Change vs No-Change Records",
    x = "Cover Change",
    y = "Percentage",
    fill = "Dataset Name"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5))

## 5.4. Plot 4 - by publisher and dataset --------------------------------------

# Create a named vector for publisher name mapping
publisher_map <- c(
  "The Norwegian Biodiversity Information Centre (NBIC)" = "NBIC",
  "Cornell Lab of Ornithology" = "Cornell",
  "Norwegian University of Life Sciences (NMBU)" = "NMBU",
  "University of Oslo" = "UiO",
  "Norwegian Institute for Nature Research" = "NINA",
  "The Inernational Barcode of Life Consortium" = "Barcode of Life",
  "Norwegian University of Science and Technology" = "NTNU")

# Prepare the data with shortened publisher names
plot_data <- df %>%
  # First, replace the long publisher names
  mutate(publisher.x = case_when(
    publisher.x %in% names(publisher_map) ~ publisher_map[publisher.x],
    TRUE ~ publisher.x
  )) %>%
  # Continue with the rest of the data preparation
  group_by(cover_change, datasetName.x, publisher.x) %>%
  summarize(counts = sum(ocurrences_after), .groups = 'drop') %>%
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n = 10) %>%
  mutate(combined_label = paste0(datasetName.x, " (", publisher.x, ")")) %>%
  mutate(combined_label = factor(combined_label, levels = rev(unique(combined_label))))

# Create custom color palette
custom_colors <- c(
  brewer.pal(8, "Set2"),
  brewer.pal(8, "Set1"),
  brewer.pal(4, "Dark2")
)

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = combined_label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Dataset Distribution in Change vs No-Change Records",
    x = "Cover Change",
    y = "Percentage",
    fill = "Dataset (Publisher)"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "cm"),
    legend.nrow = 5
  )

# 6. SUMMARY STATISTICS --------------------------------------------------------

## 6.1. Y/N Cover Change -------------------------------------------------------

# Load data
load(here::here("data", "derived_data",
                "occ_y_n_cover_change_before_after_for_modell.rda"))

# Add a row for "All Periods" to the original time period statistics
all_periods_stats <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(cover_change) |>
  summarise(time_period = "All Periods",
            count = n(),
            zeros = sum(ocurrences_after == 0, na.rm = TRUE),
            pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
            median = median(ocurrences_after, na.rm = TRUE),
            q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
            q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
            max = max(ocurrences_after, na.rm = TRUE),
            .groups = "drop")

# Get the period-specific statistics with the added columns
period_stats <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(time_period, cover_change) |>
  summarise(count = n(),
            zeros = sum(ocurrences_after == 0, na.rm = TRUE),
            pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
            median = median(ocurrences_after, na.rm = TRUE),
            q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
            q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
            max = max(ocurrences_after, na.rm = TRUE),
            .groups = "drop")

# Combine the statistics
combined_stats <- bind_rows(period_stats, all_periods_stats) |>
  arrange(desc(time_period == "All Periods"), time_period, cover_change)

# Format with kableExtra for a comprehensive view
combined_stats |>
  kable(format = "html",
        col.names = c("Time Period", "Cover Change", "Count", 
                      "Zeros", "% Zeros", "Median", "75th Percentile", 
                      "90th Percentile", "Maximum"),
        digits = c(0, 0, 0, 0, 1, 1, 1, 1, 0)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) |>
  add_header_above(c(" " = 2, "Sample Size" = 1, "Zero Values" = 2, "Distribution Statistics" = 4)) |>
  row_spec(0, bold = TRUE, background = "#F2F2F2") |>
  row_spec(which(combined_stats$time_period == "All Periods"), 
           background = "#E6E6E6", bold = TRUE) |>
  column_spec(1:2, bold = TRUE)


## 6.2. Intens/Extens Cover Change ---------------------------------------------

# Load data
load(here::here("data", "derived_data",
                "occ_intens_extens_before_after_for_model.rda"))

# Add a row for "All Periods" to the original time period statistics
all_periods_stats <- occ_intens_extens_before_after_for_model |>
  group_by(cover_change) |>
  summarise(time_period = "All Periods",
            count = n(),
            zeros = sum(ocurrences_after == 0, na.rm = TRUE),
            pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
            median = median(ocurrences_after, na.rm = TRUE),
            q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
            q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
            max = max(ocurrences_after, na.rm = TRUE),
            .groups = "drop")

# Get the period-specific statistics with the added columns
period_stats <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(time_period, cover_change) |>
  summarise(count = n(),
            zeros = sum(ocurrences_after == 0, na.rm = TRUE),
            pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
            median = median(ocurrences_after, na.rm = TRUE),
            q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
            q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
            max = max(ocurrences_after, na.rm = TRUE),
            .groups = "drop")

# Combine the statistics
combined_stats <- bind_rows(period_stats, all_periods_stats) |>
  arrange(desc(time_period == "All Periods"), time_period, cover_change)

# Format with kableExtra for a comprehensive view
combined_stats |>
  kable(format = "html",
        col.names = c("Time Period", "Cover Change", "Count", 
                      "Zeros", "% Zeros", "Median", "75th Percentile", 
                      "90th Percentile", "Maximum"),
        digits = c(0, 0, 0, 0, 1, 1, 1, 1, 0)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) |>
  add_header_above(c(" " = 2, "Sample Size" = 1, "Zero Values" = 2, "Distribution Statistics" = 4)) |>
  row_spec(0, bold = TRUE, background = "#F2F2F2") |>
  row_spec(which(combined_stats$time_period == "All Periods"), 
           background = "#E6E6E6", bold = TRUE) |>
  column_spec(1:2, bold = TRUE)

# END OF SCRIPT ----------------------------------------------------------------
