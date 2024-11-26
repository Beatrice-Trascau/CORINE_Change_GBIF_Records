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

# 2. EXTRACT MODEL OUTPUTS -----------------------------------------------------

# Create list of models
model_list <- list(model4.1_urban, model4.2_complex_agri, model4.3_agri_veg,
                   model4.4_forests, model4.5_moors, model4.6_woodland,
                   model4.7_sparse)

# Apply function to extract model summary to df
models_results <-lapply(model_list, extract_summary_as_df)

# Combine all extracted results in a single df
combined_results <- bind_rows(models_results, .id = "model_id")

# 3. SHAPE DF FOR PLOTTING -----------------------------------------------------

# Add new column with the initial land cover (from model id)
combined_results1 <- combined_results |>
  mutate(intial_cover = case_when(model_id == 1 ~ "Urban Fabric",
                                  model_id == 2 ~ "Complex Agriculture",
                                  model_id == 3 ~ "Agriculture & Vegetation",
                                  model_id == 4 ~ "Forests",
                                  model_id == 5 ~ "Moors, Heathland & Grassland",
                                  model_id == 6 ~ "Transitional Woodland Shrub",
                                  model_id == 7 ~ "Sparse Vegetation"))

# Add new column with the cover change
combined_results2 <- combined_results1 |>
  mutate(term = ifelse(grepl("cover_change", term), gsub("_", ".", term), term))