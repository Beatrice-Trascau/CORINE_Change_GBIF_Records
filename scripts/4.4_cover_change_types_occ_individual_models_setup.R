##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.4_cover_change_types_occ_individual_models_setup
# This script contains code which runs models for each initial land cover and
# its changes on the number of occurrences in a pixel
##----------------------------------------------------------------------------##

# 1. LOAD AND PREPARE DATA FOR ANALYSIS ----------------------------------------

# Load data
load(here("data", "derived_data", 
          "occ_cover_change_types_before_after_for_model.rda"))

# Give the data an easier name to work with
occ_cover_types <- occ_cover_change_types_before_after_for_model

# Check data structure
glimpse(occ_cover_types)

# Split the "cover_change" column into two columns: initial_lc and changed_lc


# 2. MODELS FOR EACH INTIAL LAND COVER TYPE ------------------------------------

## 2.1. 