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

## 2.2 Extract number of pixels where change is detected ----
freq(norway_corine_change_modified_stack[[1]]) #197 443 pixels with change
# the Land Cover Status Layers (2000 - 2006) had 176 833 pixels with change

# 3. LAND COVER TRANSITIONS 2000 - 2006

# Calculate change between 2000 and 2006
corine_2000_2006_df <- as.data.frame(freq(norway_corine_change_modified_stack[[1]] -
                                            norway_corine_change_modified_stack[[2]])) |>
  mutate(source_year = "2000",
         target_year = "2006",
         difference = value) |>
  select(-layer)

## 3.1. Create score meaning data frame ----

# Source "value" column
source_number <- c(rep(1,7), rep(80,7), rep(103,7),
                   rep(250,7), rep(380,7), rep(590,7),
                   rep(711,7))

# Source name column 
source_name <- c(rep("urban.fabric",7), rep("complex.agriculture",7), 
                 rep("agriculture.and.vegetation",7), rep("forests",7), 
                 rep("moors.heath.grass",7), rep("transitional.woodland",7),
                 rep("sparse.vegetation",7))

# Target "value" column
target_number <- c(rep(c(1,80,103,250,380,590,711), 7))

# Target name column
target_name <- c(rep(c("urban.fabric", "complex.agriculture", "agriculture.and.vegetation",
                       "forests", "moors.heath.grass", "transitional.woodland",
                       "sparse.vegetation"), 7))

# Combine vectors in df  
corine_class_meaning <- data.frame(source_number, source_name,
                                   target_number, target_name)

# Add a column for the difference between the two years
corine_class_meaning <- corine_class_meaning |>
  mutate(difference = source_number - target_number)

## 3.2. Get values for source and target land cover in change layer from the score meaning dataframe ----
# Subset score meaning df to only contain the "differences" which are found across Norway
norway_corine_class_meaning <- corine_class_meaning |>
  filter(difference %in% corine_2000_2006_df$value)



