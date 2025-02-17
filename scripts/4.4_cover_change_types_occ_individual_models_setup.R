# Define function
install_load_package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

# Define list of packages
package_vec <- c("here", "terra", "sf", "geodata", "mapview",
                 "tidyverse", "dplyr", "ggplot2", "ggalluvial",
                 "networkD3", "gt", "cowplot", "data.table",
                 "tidyterra", "patchwork", "styler", "scales",
                 "plotly", "lme4", "DHARMa", "glmmTMB", "mgcv",
                 "tidyterra", "ggspatial", "htmlwidgets",
                 "htmltools", "patchwork", "webshot2",
                 "rgbif", "CoordinateCleaner", "DHARMa") # specify packages

# Execute the function
sapply(package_vec, install_load_package)

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


# 2. MODELS FOR URBAN ----------------------------------------------------------

# Filter out other land cover change
occ_urban <- occ_cover_types |>
  filter(lc_change_from == "urban")

# Relevel cover_change to have 'urban_urban' as the reference
occ_urban$cover_change <- relevel(occ_urban$cover_change, ref = "urban_urban")

## 2.1 Interaction -------------------------------------------------------------

# Run model
urban_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                         family = nbinom2, data = occ_urban)

# Save model output to file to save time next time
save(urban_model1_SSB_interaction, file = here::here("data", "models", 
                                                     "urban_model1_SSB_interaction.RData"))

## 2.2 No Interaction ----------------------------------------------------------

# Re-run model without interaction
urban_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                         family = nbinom2, data = occ_urban)

# Save model output to file to save time next time
save(urban_model2_SSB_no_interaction, file = here::here("data", "models", 
                                                      "urban_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(urban_model1_SSB_interaction, urban_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 2.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
urban_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                  family = nbinom2, data = occ_urban)

# Save model output to file to save time next time
save(urban_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                               "urban_model3_SSB_interaction_0.1_offset.RData"))

## 2.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
urban_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                   family = nbinom2, data = occ_urban)

# Save model output to file to save time next time
save(urban_model4_SSB_interaction_0.01_offset, file = here::here("data", "models", 
                                                "urban_model4_SSB_interaction_0.01_offset.RData"))

# 3. COMPLEX AGRICULTURAL COVER ------------------------------------------------

# Filter out other land cover change
occ_complex_agri <- occ_cover_types |>
  filter(lc_change_from == "complex_agri")

# # Relevel cover_change to have 'complex_agri_complex_agri' as the reference
occ_complex_agri$cover_change <- relevel(occ_complex_agri$cover_change,
                                         ref = "complex_agri_complex_agri")

## 3.1. Interaction ------------------------------------------------------------

# # Run model
complex_agri_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                          family = nbinom2,
                          data = occ_complex_agri)

# # Save model output to file to save time next time
save(complex_agri_model1_SSB_interaction, file = here::here("data", "models",
                                       "complex_agri_model1_SSB_interaction.RData"))

## 3.2. No interaction ---------------------------------------------------------

# Re-run model without interaction
complex_agri_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                                family = nbinom2,
                                                data = occ_complex_agri)

# # Save model output to file to save time next time
save(complex_agri_model2_SSB_no_interaction, file = here::here("data", "models",
                                                             "complex_agri_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(complex_agri_model1_SSB_interaction, complex_agri_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 3.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
complex_agri_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                   family = nbinom2, data = occ_complex_agri)

# Save model output to file to save time next time
save(complex_agri_model3_SSB_interaction_0.1_offset, file = here::here("data", "models", 
                                                                "complex_agri_model3_SSB_interaction_0.1_offset.RData"))

## 3.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
complex_agri_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                    family = nbinom2, data = occ_complex_agri)

# Save model output to file to save time next time
save(complex_agri_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                 "complex_agri_model4_SSB_interaction_0.01_offset.RData"))



# 4. AGRICULTURE WITH SIGNIFICANT NATURAL VEGETATION ---------------------------

# Filter out other land cover change
occ_agri_veg <- occ_cover_types |>
  filter(lc_change_from == "agri_sig_veg")

# Relevel cover_change to have 'agri_sig_veg_agri_sig_veg' as the reference
occ_agri_veg$cover_change <- relevel(occ_agri_veg$cover_change, 
                                     ref = "agri_sig_veg_agri_sig_veg")

## 4.1. Interaction ------------------------------------------------------------

# Run model
agri_veg_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                  family = nbinom2,
                                  data = occ_agri_veg)

# Save model output to file to save time next time
save(agri_veg_model1_SSB_interaction, file = here::here("data", "models",
                                               "agri_veg_model1_SSB_interaction.RData"))

## 4.2. No interaction ---------------------------------------------------------

# Re-run model without interaction
agri_veg_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                            family = nbinom2,
                                            data = occ_agri_veg)

# Save model output to file to save time next time
save(agri_veg_model2_SSB_no_interaction, file = here::here("data", "models", 
                                                         "agri_veg_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(agri_veg_model1_SSB_interaction, agri_veg_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 4.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
agri_veg_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                          family = nbinom2, data = occ_agri_veg)

# Save model output to file to save time next time
save(agri_veg_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                                                       "agri_veg_model3_SSB_interaction_0.1_offset.RData"))

## 4.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
agri_veg_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                           family = nbinom2, data = occ_agri_veg)

# Save model output to file to save time next time
save(agri_veg_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                    "agri_veg_model4_SSB_interaction_0.01_offset.RData"))

# 5. FORESTS -------------------------------------------------------------------

# Filter out other land cover change
occ_forests <- occ_cover_types |>
  filter(lc_change_from == "forests")

# Relevel cover_change to have 'urban_urban' as the reference
occ_forests$cover_change <- relevel(occ_forests$cover_change, 
                                    ref = "forests_forests")

## 5.1. Interaction ------------------------------------------------------------

# Run model
forests_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                              family = nbinom2,
                              data = occ_forests)

# Save model output to file to save time next time
save(forests_model1_SSB_interaction, file = here::here("data", "models",
                                         "forests_model1_SSB_interaction.RData"))

## 5.2. No interaction ---------------------------------------------------------

# Re-run model without interaction
forests_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                           family = nbinom2,
                                           data = occ_forests)

# Save model output to file to save time next time
save(forests_model2_SSB_no_interaction, file = here::here("data", "models", 
                                                        "forests_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(forests_model1_SSB_interaction, forests_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 5.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
forests_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                      family = nbinom2, data = occ_forests)

# Save model output to file to save time next time
save(forests_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                                                   "forests_model3_SSB_interaction_0.1_offset.RData"))

## 5.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
forests_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                       family = nbinom2, data = occ_forests)

# Save model output to file to save time next time
save(forests_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                    "forests_model4_SSB_interaction_0.01_offset.RData"))


# 6. MOORS, HEATHLAND AND GRASSLAND --------------------------------------------

# Filter out other land cover change
occ_moors <- occ_cover_types |>
  filter(lc_change_from == "moors_heath_grass")

# # Relevel cover_change to have 'urban_urban' as the reference
occ_moors$cover_change <- relevel(occ_moors$cover_change,
                                  ref = "moors_heath_grass_moors_heath_grass")

## 6.1. Interaction ------------------------------------------------------------

# Run model
moors_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                            family = nbinom2,
                            data = occ_moors)

# # Save model output to file to save time next time
save(moors_model1_SSB_interaction, file = here::here("data", "models",
                                         "moors_model1_SSB_interaction.RData"))

## 6.2. No Interaction ---------------------------------------------------------

# Re-run model without interaction
moors_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                         family = nbinom2,
                                         data = occ_moors)

# Save model output to file to save time next time
save(moors_model2_SSB_no_interaction, file = here::here("data", "models",
                                                      "moors_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(moors_model1_SSB_interaction, moors_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 6.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
moors_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                     family = nbinom2, data = occ_moors)

# Save model output to file to save time next time
save(moors_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                                                  "moors_model3_SSB_interaction_0.1_offset.RData"))

## 6.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
moors_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                      family = nbinom2, data = occ_moors)

# Save model output to file to save time next time
save(moors_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                   "moors_model4_SSB_interaction_0.01_offset.RData"))

# 7. TRANSITIONAL WOODLAND SHRUB -----------------------------------------------

# Filter out other land cover change
occ_woodland <- occ_cover_types |>
  filter(lc_change_from == "woodland_shrub")

# # Relevel cover_change to have 'urban_urban' as the reference
occ_woodland$cover_change <- relevel(occ_woodland$cover_change,
                                     ref = "woodland_shrub_woodland_shrub")

## 7.1. Interaction ------------------------------------------------------------

# Run model
woodland_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                          family = nbinom2,
                          data = occ_woodland)

# # Save model output to file to save time next time
save(woodland_model1_SSB_interaction, file = here::here("data", "models",
                                       "woodland_model1_SSB_interaction.RData"))

## 7.2. No interaction ---------------------------------------------------------

# Re-run model without interaction
woodland_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                            family = nbinom2,
                                            data = occ_woodland)

# # Save model output to file to save time next time
save(woodland_model2_SSB_no_interaction, file = here::here("data", "models",
                                                         "woodland_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(woodland_model1_SSB_interaction, woodland_model2_SSB_no_interaction, base = TRUE)
#deltaAIC =

## 7.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
woodland_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                   family = nbinom2, data = occ_woodland)

# Save model output to file to save time next time
save(woodland_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                                                "woodland_model3_SSB_interaction_0.1_offset.RData"))

## 7.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
woodland_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                    family = nbinom2, data = occ_woodland)

# Save model output to file to save time next time
save(woodland_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                 "woodland_model4_SSB_interaction_0.01_offset.RData"))


# 8. SPARSE VEGETATION ---------------------------------------------------------

# Filter out other land cover change
occ_sparse <- occ_cover_types |>
  filter(lc_change_from == "sparse_veg")

# # Relevel cover_change to have 'urban_urban' as the reference
occ_sparse$cover_change <- relevel(occ_sparse$cover_change,
                                   ref = "sparse_veg_sparse_veg")

# 8.1. No Interaction ----------------------------------------------------------

# Run model
sparse_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                             family = nbinom2,
                             data = occ_sparse)

# Save model output to file to save time next time
save(sparse_model1_SSB_interaction, file = here::here("data", "models",
                                          "sparse_model1_SSB_interaction.RData"))

## 8.2. No interaction ---------------------------------------------------------

# Re-run model without interaction
sparse_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                          family = nbinom2,
                                          data = occ_sparse)

# # Save model output to file to save time next time
save(sparse_model2_SSB_no_interaction, file = here::here("data", "models",
                                                       "sparse_model2_SSB_no_interaction.RData"))

# Compare models
AICtab(sparse_model1_SSB_interaction, sparse_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

## 8.3. 0.1 offset -------------------------------------------------------------

# Run 0.1 model
sparse_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                      family = nbinom2, data = occ_sparse)

# Save model output to file to save time next time
save(sparse_model3_SSB_interaction_0.1_offset, file = here::here("data", "models",
                                                                   "sparse_model3_SSB_interaction_0.1_offset.RData"))

## 7.4. 0.01 offset ------------------------------------------------------------

# Run 0.1 model
sparse_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                       family = nbinom2, data = occ_sparse)

# Save model output to file to save time next time
save(sparse_model4_SSB_interaction_0.01_offset, file = here::here("data", "models",
                                                                    "sparse_model4_SSB_interaction_0.01_offset.RData"))

# END OF SCRIPT ----------------------------------------------------------------