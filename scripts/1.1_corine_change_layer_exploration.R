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
library(sf)
library(geodata)
library(mapview)

# 1. READ IN MODIFIED CORINE CHANGE LAYERS ----

## 1.1. Download layers (if needed) ----

# Add download link from box
# norway_corine_change_stack <- ("https://ntnu.box.com/shared/static/97g9x4839ij4lnlldji2wh8e0e2lm5bf.tif")

# Download the files
# download.file(norway_corine_change_stack, "norway_corine_change_stack.tif")

## 1.2. Read in layers -----
norway_corine_change_stack <- rast(here("data", 
                                        "norway_corine_change_stack.tif"))