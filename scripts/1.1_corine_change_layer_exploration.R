##------------------------------------------------##
##---------  CODE FOR DATA ANALYSIS FOR ----------##
##---  "Paper 1: CORINE Land Cover Changes  ------##
##-------  and GBIF Biodiversity Records  --------##
##----- 1.1_corine_change_layer_exploration ------##
##------------------------------------------------##

# This script contains code which explores the CORINE land cover CHANGE layers and creates summary statistics

# 0. PACKAGES ----
library(here)
library(terra)
library(mapview)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(networkD3)
library(gt)
library(cowplot)

# 1. READ IN MODIFIED CORINE CHANGE LAYERS ----

## 1.1. Download layers (if needed) ----

# Add download link from box
# norway_corine_change_stack <- ("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif")

# Download the files
# download.file(norway_corine_change_stack, "norway_corine_change_stack.tif")

## 1.2. Read in layers -----
norway_corine_change_modified_stack <- rast(here("data", 
                                        "norway_corine_change_modified_stack.tif"))

# 2. EXPLORE LAYERS ----

# This section is a placeholder for when there is time to plot maps of the layers individually

## 2.1. Plot maps of the years ----

# Plot the layer
mapview(norway_corine_change_modified_stack[[1]])

## 2.2. Extract number of pixels where change is detected ----
freq(norway_corine_change_modified_stack[[1]]) #197 443 pixels with change
# the Land Cover Status Layers (2000 - 2006) had 176 833 pixels with change

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

## 3.4. Sankey plot for transitions between 2000 and 2006 ----

### 3.4.1. Sankey 2000 - 2006 with all classes ----

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