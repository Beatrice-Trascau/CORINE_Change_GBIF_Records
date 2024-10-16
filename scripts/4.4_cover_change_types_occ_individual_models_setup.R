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
occ_cover_types <- occ_cover_types |>
  # 1. replace "_" in values like "sparse_veg_sparse_veg" with "."
  mutate(cover_change_split = str_replace_all(cover_change,  
                                              c("sparse_veg" = "sparse.veg",
                                                "moors_heath_grass" = "moors.heath.grass",
                                                "agri_sig_veg" = "agri.sig.veg",
                                                "complex_agri" = "complex.agri",
                                                "woodland_shrub" = "woodland.shrub"))) |>
  # 2. split on "_" in two columns
  separate_wider_delim(cover_change_split, delim = "_", 
                       names = c("lc_change_from", "lc_change_to")) |>
  
  # 3. replace "." in values with "_"
  mutate(lc_change_from = str_replace_all(lc_change_from, "\\.", "_"),
         lc_change_to = str_replace_all(lc_change_to, "\\.", "_"))



# 2. MODELS FOR EACH INTIAL LAND COVER TYPE ------------------------------------

## 2.1. Urban cover (1) --------------------------------------------------------

# Filter out other land cover change
occ_urban <- occ_cover_types |>
  filter(lc_change_from == "urban")

# Relevel cover_change to have 'urban_urban' as the reference
occ_urban$cover_change <- relevel(occ_urban$cover_change, ref = "urban_urban")

# Create 10% subset of data
set.seed(64687)
occ_urban_subset <- occ_urban |> 
  sample_frac(0.1)

# Run model
model4.1_urban <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                        family = nbinom2,
                        data = occ_urban)

# Save model output to file to save time next time
save(model4.1_urban, file = here::here("data", "models", 
                                       "model4.1_urban.RData"))

## 2.2. Complex agricultural cover (80) ----------------------------------------

# Filter out other land cover change
occ_complex_agri <- occ_cover_types |>
  filter(lc_change_from == "complex_agri")

# Relevel cover_change to have 'urban_urban' as the reference
occ_complex_agri$cover_change <- relevel(occ_complex_agri$cover_change, 
                                         ref = "complex_agri_complex_agri")

# Create 10% subset of data
set.seed(76468333)
occ_complex_agri_subset <- occ_complex_agri |> 
  sample_frac(0.1)

# Run model
model4.2_complex_agri <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                          family = nbinom2,
                          data = occ_complex_agri_subset)

# Save model output to file to save time next time
save(model4.2_complex_agri, file = here::here("data", "models", 
                                       "model4.2_complex_agri.RData"))
