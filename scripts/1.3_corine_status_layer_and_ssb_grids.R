##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 1.2_corine_change_layer_and_ssb_grids
# This script contains code which combines the CORINE land cover STATUS layers
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
# norway_corine_status_modified_stack <- ("https://ntnu.box.com/shared/static/z1751qp8epnqkmjs8mex9vdl29tjjr5i.tif")
# ruter500m_Norge <- ("https://ntnu.box.com/shared/static/p8896x2epq4bcmfhorsb5qn2m8mxo5ko.zip")
# occurrences <- ("https://ntnu.box.com/shared/static/cgjbsfs24m31uov6ir4vkkmw6cs7ga36.txt")

# Download files
# download.file(norway_corine_status_modified_stack, here("data", 
# "norway_corine_status_modified_stack.tif"))

# download.file(ruter500m_Norge, here("data", "raw_data",
# "ruter500m_Norge.zip"))

# download.file(cleaned_occurrences, here("data","cleaned_occurrences.txt"))

# Read in the data
norway_corine_status_modified_stack <- rast(here("data", 
                                                 "norway_corine_status_modified_stack.tif"))

ssb_grids <- vect(here("data", "raw_data", "ruter500m_Norge.shp"))

occurrences_norway <- fread(here("data", "cleaned_occurrences.txt"))
