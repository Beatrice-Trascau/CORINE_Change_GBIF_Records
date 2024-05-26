##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.4_corine_status_layer_count_pixels_in_ssb_grids
# This script contains code which counts the number of CORINE status pixels
# which are (not)changing in each SSB ID
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
# norway_corine_status_modified_stack <- ("https://ntnu.box.com/shared/static/z1751qp8epnqkmjs8mex9vdl29tjjr5i.tif")
# ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")
# Download files
# download.file(norway_corine_status_modified_stack, here("data", 
# "norway_corine_status_modified_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
# "ruter500m_Norge.zip"))

# download.file(cleaned_occurrences, here("data","cleaned_occurrences.txt"))

# Read in the data
norway_corine_status_modified_stack <- rast(here("data", 
                                                 "norway_corine_status_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data",
                       "SSB010KM", "Ruter_10km_Norge.shp"))

# 2. PLOT THE NUMBER OF PIXELS CHANGING/NOT CHANGING IN EACH SSB ID: 2000-2006 ----

# Calculate difference in land cover change between 2000 and 2006
change_raster <- norway_corine_status_modified_stack[[1]] != norway_corine_status_modified_stack[[2]]

# Extract raster values by SSB grid
ssb_values <- terra::extract(change_raster, ssb_grid, fun=function(x) {
  c(changing = sum(x, na.rm=TRUE), not_changing = sum(!x, na.rm=TRUE))
})

# Convert to data frame to use for plotting
ssb_values_df <- as.data.frame(ssb_values) 
ssb_grid$changing <- ssb_values_df$changing
ssb_grid$not_changing <- ssb_values_df$not_changing

# Classify SSB grid based on changes
ssb_grid$classification <- ifelse(ssb_grid$changing > ssb_grid$not_changing, 
                                  "Changing", "Not Changing")

# Change to ggplot suitable format
ssb_grid_df <- as.data.frame(ssb_grid, xy=TRUE)
ssb_grid_df <- ssb_grid_df %>% 
  group_by(ID) %>% 
  summarise(long = mean(x), lat = mean(y), classification = first(classification))

# Plot map
ggplot(ssb_grid_df) +
  geom_tile(aes(x = long, y = lat, fill = classification), color = "black", size = 0.2) +
  scale_fill_manual(values = c("Changing" = "#800080", "Not Changing" = "#FFD700")) +
  theme_minimal() +
  labs(title = "Land Cover Changes in Norway (2000-2006)",
       fill = "Change Classification")
