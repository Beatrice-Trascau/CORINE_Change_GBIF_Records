##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 5.1_cover_change_combine_model_outputs_in_figure4
# This script contains code which combines the outputs of the individual land
# cover models into a single figure for the paper (Figure 4)
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------
load(here("data", "models", "model4.1_urban.RData"))
load(here("data", "models", "model4.2_complex_agri.RData"))
load(here("data", "models", "model4.3_agri_veg.RData"))
load(here("data", "models", "model4.4_forests.RData"))
load(here("data", "models", "model4.5_moors.RData"))
load(here("data", "models", "model4.6_woodland.RData"))
load(here("data", "models", "model4.7_sparse.RData"))