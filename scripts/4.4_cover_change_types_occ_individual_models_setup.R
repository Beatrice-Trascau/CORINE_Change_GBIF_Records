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
# occ_urban <- occ_cover_types |>
#   filter(lc_change_from == "urban")

# Relevel cover_change to have 'urban_urban' as the reference
#occ_urban$cover_change <- relevel(occ_urban$cover_change, ref = "urban_urban")

# Create 10% subset of data
# set.seed(64687)
# occ_urban_subset <- occ_urban |> 
#   sample_frac(0.1)

# Run model
# model4.1_urban <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
#                         family = nbinom2,
#                         data = occ_urban)

# Save model output to file to save time next time
# save(model4.1_urban, file = here::here("data", "models", 
#                                        "model4.1_urban.RData"))

## 2.2. Complex agricultural cover (80) ----------------------------------------

# Filter out other land cover change
occ_complex_agri <- occ_cover_types |>
  filter(lc_change_from == "complex_agri")

# Relevel cover_change to have 'urban_urban' as the reference
occ_complex_agri$cover_change <- relevel(occ_complex_agri$cover_change, 
                                         ref = "complex_agri_complex_agri")

# Run model
model4.2_complex_agri <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                          family = nbinom2,
                          data = occ_complex_agri)

# Save model output to file to save time next time
save(model4.2_complex_agri, file = here::here("data", "models", 
                                       "model4.2_complex_agri.RData"))

## 2.3. Agriculture and significant vegetation (103) ---------------------------

# Filter out other land cover change
occ_agri_veg <- occ_cover_types |>
  filter(lc_change_from == "agri_sig_veg")

# Relevel cover_change to have 'urban_urban' as the reference
occ_agri_veg$cover_change <- relevel(occ_agri_veg$cover_change, 
                                         ref = "agri_sig_veg_agri_sig_veg")


# Run model
model4.3_agri_veg <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                                 family = nbinom2,
                                 data = occ_agri_veg)

# Save model output to file to save time next time
save(model4.3_agri_veg, file = here::here("data", "models", 
                                              "model4.3_agri_veg.RData"))

## 2.4. Forests (250) ----------------------------------------------------------

# Filter out other land cover change
occ_forests <- occ_cover_types |>
  filter(lc_change_from == "forests")

# Relevel cover_change to have 'urban_urban' as the reference
occ_forests$cover_change <- relevel(occ_forests$cover_change, 
                                     ref = "forests_forests")


# Run model
model4.4_forests <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                             family = nbinom2,
                             data = occ_forests)

# Save model output to file to save time next time
save(model4.4_forests, file = here::here("data", "models", 
                                          "model4.4_forests.RData"))

## 2.5. Moors, Heathland and Grassland (380) -----------------------------------

# Filter out other land cover change
occ_moors <- occ_cover_types |>
  filter(lc_change_from == "moors_heath_grass")

# Relevel cover_change to have 'urban_urban' as the reference
occ_moors$cover_change <- relevel(occ_moors$cover_change, 
                                    ref = "moors_heath_grass_moors_heath_grass")


# Run model
model4.5_moors <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                            family = nbinom2,
                            data = occ_moors)

# Save model output to file to save time next time
save(model4.5_moors, file = here::here("data", "models", 
                                         "model4.5_moors.RData"))

## 2.6. Woodland shrub (590) ---------------------------------------------------

# Filter out other land cover change
occ_woodland <- occ_cover_types |>
  filter(lc_change_from == "woodland_shrub")

# Relevel cover_change to have 'urban_urban' as the reference
occ_woodland$cover_change <- relevel(occ_woodland$cover_change, 
                                  ref = "woodland_shrub_woodland_shrub")


# Run model
model4.6_woodland <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                          family = nbinom2,
                          data = occ_woodland)

# Save model output to file to save time next time
save(model4.6_woodland, file = here::here("data", "models", 
                                       "model4.6_woodland.RData"))

## 2.7. Sparse vegetation (711) ------------------------------------------------

# Filter out other land cover change
occ_sparse <- occ_cover_types |>
  filter(lc_change_from == "sparse_veg")

# Relevel cover_change to have 'urban_urban' as the reference
occ_sparse$cover_change <- relevel(occ_sparse$cover_change, 
                                     ref = "sparse_veg_sparse_veg")


# Run model
model4.7_sparse <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + (1 | SSBID),
                             family = nbinom2,
                             data = occ_sparse)

# Save model output to file to save time next time
save(model4.7_sparse, file = here::here("data", "models", 
                                          "model4.7_sparse.RData"))