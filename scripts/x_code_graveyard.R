##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# x_code_graveyard
# This script contains discarded code that may still be useful later
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