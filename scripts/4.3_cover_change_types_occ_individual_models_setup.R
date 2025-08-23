##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.3_cover_change_types_occ_individual_models_setup
# This script runs zero-inflated models for each initial land cover type
# and examines how different transitions affect occurrence patterns
##----------------------------------------------------------------------------##

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
                 "rgbif", "CoordinateCleaner", "DHARMa",
                 "writexl", "bbmle", "kableExtra", "googledrive")

# Execute the function
sapply(package_vec, install_load_package)

# 1. LOAD AND PREPARE DATA FOR ANALYSIS ----------------------------------------

# Load data from script 3.1
load(here("data", "derived_data", 
          "modeling_data_combined_corine_gbif_ssb_august2025.rda"))

# Convert to wide format (same as in scripts 4.1 and 4.2)
modeling_data_wide <- modeling_data_filtered |>
  select(cell_ID, SSBID, x, y,
         land_cover_start, land_cover_end, land_cover_start_name, 
         land_cover_end_name, cover_change, transition_type, intens_extens,
         time_period, n_occurrences, n_species, analysis_period,
         species_list, kingdom_list, phylum_list, class_list, order_list, 
         family_list, publisher_list, datasetName_list) |>
  pivot_wider(id_cols = c(cell_ID, SSBID, x, y, land_cover_start, land_cover_end,
                          land_cover_start_name, land_cover_end_name, cover_change,
                          transition_type, intens_extens, analysis_period),
              names_from = time_period,
              values_from = c(n_occurrences, n_species, species_list, kingdom_list,
                              phylum_list, class_list, order_list, family_list,
                              publisher_list, datasetName_list),
              names_sep = "_") |>
  mutate(occurrences_before = case_when(analysis_period == "2000_2006" ~ n_occurrences_before_2000_2006,
                                        analysis_period == "2006_2012" ~ n_occurrences_before_2006_2012,
                                        analysis_period == "2012_2018" ~ n_occurrences_before_2012_2018,
                                        TRUE ~ NA_real_),
         occurrences_after = case_when(analysis_period == "2000_2006" ~ n_occurrences_after_2000_2006,
                                       analysis_period == "2006_2012" ~ n_occurrences_after_2006_2012,
                                       analysis_period == "2012_2018" ~ n_occurrences_after_2012_2018,
                                       TRUE ~ NA_real_),
         time_period = case_when(analysis_period == "2000_2006" ~ "2000_2006",
                                 analysis_period == "2006_2012" ~ "2006_2012",
                                 analysis_period == "2012_2018" ~ "2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(occurrences_before) & !is.na(occurrences_after)) |>
  mutate(cell_ID = as.factor(cell_ID), 
         SSBID = as.factor(SSBID),
         cover_change = as.factor(cover_change),
         intens_extens = as.factor(intens_extens),
         time_period = as.factor(time_period),
         land_cover_start_name = as.factor(land_cover_start_name),
         land_cover_end_name = as.factor(land_cover_end_name),
         transition_type = as.factor(transition_type))

# 2. MODELS FOR URBAN ----------------------------------------------------------

# Filter for urban starting land cover
urban_data <- modeling_data_wide |>
  filter(land_cover_start_name == "urban")

# Set reference level to "no change" (urban to urban)
urban_data$transition_type <- relevel(urban_data$transition_type, ref = "urban_to_urban")

cat("Urban data: ", nrow(urban_data), " observations\n")
cat("Urban transitions: ", paste(levels(urban_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for urban data
cat("Checking zero-inflation necessity for urban data:\n")
zero_prop_urban <- sum(urban_data$occurrences_after == 0) / nrow(urban_data)
cat("Proportion of zeros: ", round(zero_prop_urban, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
urban_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         family = nbinom2,
                         data = urban_data)

# Test for zero-inflation
urban_zi_test <- simulateResiduals(urban_nb_test)
urban_zi_result <- testZeroInflation(urban_zi_test)
cat("Zero-inflation test p-value: ", urban_zi_result$p.value, "\n")
urban_needs_zi <- urban_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(urban_needs_zi, "YES", "NO"), "\n\n")

## 2.1. Urban Model 1: With Interaction ----------------------------------------
if(urban_needs_zi) {
  cat("Using zero-inflated model for urban data (interaction)\n")
  urban_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                        offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                      zi = ~transition_type * time_period + (1 | SSBID),
                                      family = nbinom2,
                                      data = urban_data)
} else {
  cat("Using standard negative binomial model for urban data (interaction)\n")
  urban_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                        offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                      family = nbinom2,
                                      data = urban_data)
}

## 2.2. Urban Model 2: No Interaction ------------------------------------------
if(urban_needs_zi) {
  cat("Using zero-inflated model for urban data (no interaction)\n")
  urban_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         zi = ~transition_type * time_period + (1 | SSBID),
                                         family = nbinom2,
                                         data = urban_data)
} else {
  cat("Using standard negative binomial model for urban data (no interaction)\n")
  urban_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         family = nbinom2,
                                         data = urban_data)
}

# Save models
model_type_urban <- ifelse(urban_needs_zi, "zi", "nb")
save(urban_model1_interaction, file = here("data", "models", 
                                           paste0("urban_model1_", model_type_urban, "_interaction.RData")))
save(urban_model2_no_interaction, file = here("data", "models", 
                                              paste0("urban_model2_", model_type_urban, "_no_interaction.RData")))

# Compare models
urban_aic_comparison <- AICtab(urban_model1_interaction, urban_model2_no_interaction, base = TRUE)
cat("Urban model comparison:\n")
print(urban_aic_comparison)

# 3. MODELS FOR COMPLEX AGRICULTURAL -------------------------------------------

cat("\nRunning models for COMPLEX AGRICULTURAL land cover...\n")

# Filter for complex agricultural starting land cover
complex_agri_data <- modeling_data_wide |>
  filter(land_cover_start_name == "complex_agri")

# Set reference level to "no change" (complex_agri to complex_agri)
complex_agri_data$transition_type <- relevel(complex_agri_data$transition_type, 
                                             ref = "complex_agri_to_complex_agri")

cat("Complex agri data: ", nrow(complex_agri_data), " observations\n")
cat("Complex agri transitions: ", paste(levels(complex_agri_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for complex agricultural data
cat("Checking zero-inflation necessity for complex agricultural data:\n")
zero_prop_complex_agri <- sum(complex_agri_data$occurrences_after == 0) / nrow(complex_agri_data)
cat("Proportion of zeros: ", round(zero_prop_complex_agri, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
complex_agri_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                family = nbinom2,
                                data = complex_agri_data)

# Test for zero-inflation
complex_agri_zi_test <- simulateResiduals(complex_agri_nb_test)
complex_agri_zi_result <- testZeroInflation(complex_agri_zi_test)
cat("Zero-inflation test p-value: ", complex_agri_zi_result$p.value, "\n")
complex_agri_needs_zi <- complex_agri_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(complex_agri_needs_zi, "YES", "NO"), "\n\n")

## 3.1. Complex Agri Model 1: With Interaction ---------------------------------
if(complex_agri_needs_zi) {
  cat("Using zero-inflated model for complex agricultural data (interaction)\n")
  complex_agri_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                               offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                             zi = ~transition_type * time_period + (1 | SSBID),
                                             family = nbinom2,
                                             data = complex_agri_data)
} else {
  cat("Using standard negative binomial model for complex agricultural data (interaction)\n")
  complex_agri_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                               offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                             family = nbinom2,
                                             data = complex_agri_data)
}

## 3.2. Complex Agri Model 2: No Interaction -----------------------------------
if(complex_agri_needs_zi) {
  cat("Using zero-inflated model for complex agricultural data (no interaction)\n")
  complex_agri_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                                zi = ~transition_type * time_period + (1 | SSBID),
                                                family = nbinom2,
                                                data = complex_agri_data)
} else {
  cat("Using standard negative binomial model for complex agricultural data (no interaction)\n")
  complex_agri_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                                family = nbinom2,
                                                data = complex_agri_data)
}

# Save models
model_type_complex_agri <- ifelse(complex_agri_needs_zi, "zi", "nb")
save(complex_agri_model1_interaction, file = here("data", "models", 
                                                  paste0("complex_agri_model1_", model_type_complex_agri, "_interaction.RData")))
save(complex_agri_model2_no_interaction, file = here("data", "models", 
                                                     paste0("complex_agri_model2_", model_type_complex_agri, "_no_interaction.RData")))

# Compare models
complex_agri_aic_comparison <- AICtab(complex_agri_model1_interaction, complex_agri_model2_no_interaction, base = TRUE)
cat("Complex agri model comparison:\n")
print(complex_agri_aic_comparison)

# 4. MODELS FOR AGRICULTURE WITH SIGNIFICANT VEGETATION ------------------------

cat("\nRunning models for AGRICULTURE WITH SIGNIFICANT VEGETATION land cover...\n")

# Filter for agri_sig_veg starting land cover
agri_sig_veg_data <- modeling_data_wide |>
  filter(land_cover_start_name == "agri_sig_veg")

# Set reference level to "no change" (agri_sig_veg to agri_sig_veg)
agri_sig_veg_data$transition_type <- relevel(agri_sig_veg_data$transition_type, 
                                             ref = "agri_sig_veg_to_agri_sig_veg")

cat("Agri sig veg data: ", nrow(agri_sig_veg_data), " observations\n")
cat("Agri sig veg transitions: ", paste(levels(agri_sig_veg_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for agri_sig_veg data
cat("Checking zero-inflation necessity for agri_sig_veg data:\n")
zero_prop_agri_sig_veg <- sum(agri_sig_veg_data$occurrences_after == 0) / nrow(agri_sig_veg_data)
cat("Proportion of zeros: ", round(zero_prop_agri_sig_veg, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
agri_sig_veg_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                family = nbinom2,
                                data = agri_sig_veg_data)

# Test for zero-inflation
agri_sig_veg_zi_test <- simulateResiduals(agri_sig_veg_nb_test)
agri_sig_veg_zi_result <- testZeroInflation(agri_sig_veg_zi_test)
cat("Zero-inflation test p-value: ", agri_sig_veg_zi_result$p.value, "\n")
agri_sig_veg_needs_zi <- agri_sig_veg_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(agri_sig_veg_needs_zi, "YES", "NO"), "\n\n")

## 4.1. Agri Sig Veg Model 1: With Interaction ---------------------------------
if(agri_sig_veg_needs_zi) {
  cat("Using zero-inflated model for agri_sig_veg data (interaction)\n")
  agri_sig_veg_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                               offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                             zi = ~transition_type * time_period + (1 | SSBID),
                                             family = nbinom2,
                                             data = agri_sig_veg_data)
} else {
  cat("Using standard negative binomial model for agri_sig_veg data (interaction)\n")
  agri_sig_veg_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                               offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                             family = nbinom2,
                                             data = agri_sig_veg_data)
}

## 4.2. Agri Sig Veg Model 2: No Interaction -----------------------------------
if(agri_sig_veg_needs_zi) {
  cat("Using zero-inflated model for agri_sig_veg data (no interaction)\n")
  agri_sig_veg_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                                zi = ~transition_type * time_period + (1 | SSBID),
                                                family = nbinom2,
                                                data = agri_sig_veg_data)
} else {
  cat("Using standard negative binomial model for agri_sig_veg data (no interaction)\n")
  agri_sig_veg_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                                family = nbinom2,
                                                data = agri_sig_veg_data)
}

# Save models
model_type_agri_sig_veg <- ifelse(agri_sig_veg_needs_zi, "zi", "nb")
save(agri_sig_veg_model1_interaction, file = here("data", "models", 
                                                  paste0("agri_sig_veg_model1_", model_type_agri_sig_veg, "_interaction.RData")))
save(agri_sig_veg_model2_no_interaction, file = here("data", "models", 
                                                     paste0("agri_sig_veg_model2_", model_type_agri_sig_veg, "_no_interaction.RData")))

# Compare models
agri_sig_veg_aic_comparison <- AICtab(agri_sig_veg_model1_interaction, agri_sig_veg_model2_no_interaction, base = TRUE)
cat("Agri sig veg model comparison:\n")
print(agri_sig_veg_aic_comparison)

# 5. MODELS FOR FORESTS --------------------------------------------------------

cat("\nRunning models for FORESTS land cover...\n")

# Filter for forests starting land cover
forests_data <- modeling_data_wide |>
  filter(land_cover_start_name == "forests")

# Set reference level to "no change" (forests to forests)
forests_data$transition_type <- relevel(forests_data$transition_type, 
                                        ref = "forests_to_forests")

cat("Forests data: ", nrow(forests_data), " observations\n")
cat("Forests transitions: ", paste(levels(forests_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for forests data
cat("Checking zero-inflation necessity for forests data:\n")
zero_prop_forests <- sum(forests_data$occurrences_after == 0) / nrow(forests_data)
cat("Proportion of zeros: ", round(zero_prop_forests, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
forests_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                           family = nbinom2,
                           data = forests_data)

# Test for zero-inflation
forests_zi_test <- simulateResiduals(forests_nb_test)
forests_zi_result <- testZeroInflation(forests_zi_test)
cat("Zero-inflation test p-value: ", forests_zi_result$p.value, "\n")
forests_needs_zi <- forests_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(forests_needs_zi, "YES", "NO"), "\n\n")

## 5.1. Forests Model 1: With Interaction ---------------------------------------
if(forests_needs_zi) {
  cat("Using zero-inflated model for forests data (interaction)\n")
  forests_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                          offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                        zi = ~transition_type * time_period + (1 | SSBID),
                                        family = nbinom2,
                                        data = forests_data)
} else {
  cat("Using standard negative binomial model for forests data (interaction)\n")
  forests_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                          offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                        family = nbinom2,
                                        data = forests_data)
}

## 5.2. Forests Model 2: No Interaction -----------------------------------------
if(forests_needs_zi) {
  cat("Using zero-inflated model for forests data (no interaction)\n")
  forests_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                           zi = ~transition_type * time_period + (1 | SSBID),
                                           family = nbinom2,
                                           data = forests_data)
} else {
  cat("Using standard negative binomial model for forests data (no interaction)\n")
  forests_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                           family = nbinom2,
                                           data = forests_data)
}

# Save models
model_type_forests <- ifelse(forests_needs_zi, "zi", "nb")
save(forests_model1_interaction, file = here("data", "models", 
                                             paste0("forests_model1_", model_type_forests, "_interaction.RData")))
save(forests_model2_no_interaction, file = here("data", "models", 
                                                paste0("forests_model2_", model_type_forests, "_no_interaction.RData")))

# Compare models
forests_aic_comparison <- AICtab(forests_model1_interaction, forests_model2_no_interaction, base = TRUE)
cat("Forests model comparison:\n")
print(forests_aic_comparison)

# 6. MODELS FOR MOORS, HEATHLAND AND GRASSLAND ---------------------------------

cat("\nRunning models for MOORS, HEATHLAND AND GRASSLAND land cover...\n")

# Filter for moors_heath_grass starting land cover
moors_data <- modeling_data_wide |>
  filter(land_cover_start_name == "moors_heath_grass")

# Set reference level to "no change" (moors_heath_grass to moors_heath_grass)
moors_data$transition_type <- relevel(moors_data$transition_type, 
                                      ref = "moors_heath_grass_to_moors_heath_grass")

cat("Moors data: ", nrow(moors_data), " observations\n")
cat("Moors transitions: ", paste(levels(moors_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for moors data
cat("Checking zero-inflation necessity for moors data:\n")
zero_prop_moors <- sum(moors_data$occurrences_after == 0) / nrow(moors_data)
cat("Proportion of zeros: ", round(zero_prop_moors, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
moors_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         family = nbinom2,
                         data = moors_data)

# Test for zero-inflation
moors_zi_test <- simulateResiduals(moors_nb_test)
moors_zi_result <- testZeroInflation(moors_zi_test)
cat("Zero-inflation test p-value: ", moors_zi_result$p.value, "\n")
moors_needs_zi <- moors_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(moors_needs_zi, "YES", "NO"), "\n\n")

## 6.1. Moors Model 1: With Interaction -----------------------------------------
if(moors_needs_zi) {
  cat("Using zero-inflated model for moors data (interaction)\n")
  moors_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                        offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                      zi = ~transition_type * time_period + (1 | SSBID),
                                      family = nbinom2,
                                      data = moors_data)
} else {
  cat("Using standard negative binomial model for moors data (interaction)\n")
  moors_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                        offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                      family = nbinom2,
                                      data = moors_data)
}

## 6.2. Moors Model 2: No Interaction -------------------------------------------
if(moors_needs_zi) {
  cat("Using zero-inflated model for moors data (no interaction)\n")
  moors_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         zi = ~transition_type * time_period + (1 | SSBID),
                                         family = nbinom2,
                                         data = moors_data)
} else {
  cat("Using standard negative binomial model for moors data (no interaction)\n")
  moors_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         family = nbinom2,
                                         data = moors_data)
}

# Save models
model_type_moors <- ifelse(moors_needs_zi, "zi", "nb")
save(moors_model1_interaction, file = here("data", "models", 
                                           paste0("moors_model1_", model_type_moors, "_interaction.RData")))
save(moors_model2_no_interaction, file = here("data", "models", 
                                              paste0("moors_model2_", model_type_moors, "_no_interaction.RData")))

# Compare models
moors_aic_comparison <- AICtab(moors_model1_interaction, moors_model2_no_interaction, base = TRUE)
cat("Moors model comparison:\n")
print(moors_aic_comparison)

# 7. MODELS FOR TRANSITIONAL WOODLAND SHRUB ------------------------------------

cat("\nRunning models for TRANSITIONAL WOODLAND SHRUB land cover...\n")

# Filter for woodland_shrub starting land cover
woodland_data <- modeling_data_wide |>
  filter(land_cover_start_name == "woodland_shrub")

# Set reference level to "no change" (woodland_shrub to woodland_shrub)
woodland_data$transition_type <- relevel(woodland_data$transition_type, 
                                         ref = "woodland_shrub_to_woodland_shrub")

cat("Woodland data: ", nrow(woodland_data), " observations\n")
cat("Woodland transitions: ", paste(levels(woodland_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for woodland data
cat("Checking zero-inflation necessity for woodland data:\n")
zero_prop_woodland <- sum(woodland_data$occurrences_after == 0) / nrow(woodland_data)
cat("Proportion of zeros: ", round(zero_prop_woodland, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
woodland_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                              offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                            family = nbinom2,
                            data = woodland_data)

# Test for zero-inflation
woodland_zi_test <- simulateResiduals(woodland_nb_test)
woodland_zi_result <- testZeroInflation(woodland_zi_test)
cat("Zero-inflation test p-value: ", woodland_zi_result$p.value, "\n")
woodland_needs_zi <- woodland_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(woodland_needs_zi, "YES", "NO"), "\n\n")

## 7.1. Woodland Model 1: With Interaction --------------------------------------
if(woodland_needs_zi) {
  cat("Using zero-inflated model for woodland data (interaction)\n")
  woodland_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         zi = ~transition_type * time_period + (1 | SSBID),
                                         family = nbinom2,
                                         data = woodland_data)
} else {
  cat("Using standard negative binomial model for woodland data (interaction)\n")
  woodland_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                         family = nbinom2,
                                         data = woodland_data)
}

## 7.2. Woodland Model 2: No Interaction ----------------------------------------
if(woodland_needs_zi) {
  cat("Using zero-inflated model for woodland data (no interaction)\n")
  woodland_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                              offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                            zi = ~transition_type * time_period + (1 | SSBID),
                                            family = nbinom2,
                                            data = woodland_data)
} else {
  cat("Using standard negative binomial model for woodland data (no interaction)\n")
  woodland_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                              offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                            family = nbinom2,
                                            data = woodland_data)
}

# Save models
model_type_woodland <- ifelse(woodland_needs_zi, "zi", "nb")
save(woodland_model1_interaction, file = here("data", "models", 
                                              paste0("woodland_model1_", model_type_woodland, "_interaction.RData")))
save(woodland_model2_no_interaction, file = here("data", "models", 
                                                 paste0("woodland_model2_", model_type_woodland, "_no_interaction.RData")))

# Compare models
woodland_aic_comparison <- AICtab(woodland_model1_interaction, woodland_model2_no_interaction, base = TRUE)
cat("Woodland model comparison:\n")
print(woodland_aic_comparison)

# 8. MODELS FOR SPARSE VEGETATION -----------------------------------------------

cat("\nRunning models for SPARSE VEGETATION land cover...\n")

# Filter for sparse_veg starting land cover
sparse_data <- modeling_data_wide |>
  filter(land_cover_start_name == "sparse_veg")

# Set reference level to "no change" (sparse_veg to sparse_veg)
sparse_data$transition_type <- relevel(sparse_data$transition_type, 
                                       ref = "sparse_veg_to_sparse_veg")

cat("Sparse data: ", nrow(sparse_data), " observations\n")
cat("Sparse transitions: ", paste(levels(sparse_data$transition_type), collapse = ", "), "\n")

# Check if zero-inflation is needed for sparse data
cat("Checking zero-inflation necessity for sparse data:\n")
zero_prop_sparse <- sum(sparse_data$occurrences_after == 0) / nrow(sparse_data)
cat("Proportion of zeros: ", round(zero_prop_sparse, 3), "\n")

# Fit simple negative binomial model first to test for zero-inflation
sparse_nb_test <- glmmTMB(occurrences_after ~ transition_type * time_period +
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          family = nbinom2,
                          data = sparse_data)

# Test for zero-inflation
sparse_zi_test <- simulateResiduals(sparse_nb_test)
sparse_zi_result <- testZeroInflation(sparse_zi_test)
cat("Zero-inflation test p-value: ", sparse_zi_result$p.value, "\n")
sparse_needs_zi <- sparse_zi_result$p.value < 0.05
cat("Zero-inflation needed: ", ifelse(sparse_needs_zi, "YES", "NO"), "\n\n")

## 8.1. Sparse Model 1: With Interaction ----------------------------------------
if(sparse_needs_zi) {
  cat("Using zero-inflated model for sparse data (interaction)\n")
  sparse_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                         offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                       zi = ~transition_type * time_period + (1 | SSBID),
                                       family = nbinom2,
                                       data = sparse_data)
} else {
  cat("Using standard negative binomial model for sparse data (interaction)\n")
  sparse_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                         offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                       family = nbinom2,
                                       data = sparse_data)
}

## 8.2. Sparse Model 2: No Interaction ------------------------------------------
if(sparse_needs_zi) {
  cat("Using zero-inflated model for sparse data (no interaction)\n")
  sparse_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                          zi = ~transition_type * time_period + (1 | SSBID),
                                          family = nbinom2,
                                          data = sparse_data)
} else {
  cat("Using standard negative binomial model for sparse data (no interaction)\n")
  sparse_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                          family = nbinom2,
                                          data = sparse_data)
}

# Save models
model_type_sparse <- ifelse(sparse_needs_zi, "zi", "nb")
save(sparse_model1_interaction, file = here("data", "models", 
                                            paste0("sparse_model1_", model_type_sparse, "_interaction.RData")))
save(sparse_model2_no_interaction, file = here("data", "models", 
                                               paste0("sparse_model2_", model_type_sparse, "_no_interaction.RData")))

# Compare models
sparse_aic_comparison <- AICtab(sparse_model1_interaction, sparse_model2_no_interaction, base = TRUE)
cat("Sparse model comparison:\n")
print(sparse_aic_comparison)

# 9. SUMMARY OF ALL MODEL RESULTS ----------------------------------------------

cat("\n=== SUMMARY OF ALL MODEL RESULTS ===\n")

# Create summary table
summary_results <- data.frame(
  Land_Cover_Type = c("Urban", "Complex Agricultural", "Agriculture with Sig Veg", 
                      "Forests", "Moors/Heath/Grass", "Woodland Shrub", "Sparse Vegetation"),
  N_Observations = c(nrow(urban_data), nrow(complex_agri_data), nrow(agri_sig_veg_data),
                     nrow(forests_data), nrow(moors_data), nrow(woodland_data), nrow(sparse_data)),
  Proportion_Zeros = c(round(zero_prop_urban, 3), round(zero_prop_complex_agri, 3), 
                       round(zero_prop_agri_sig_veg, 3), round(zero_prop_forests, 3),
                       round(zero_prop_moors, 3), round(zero_prop_woodland, 3), 
                       round(zero_prop_sparse, 3)),
  ZI_Test_PValue = c(round(urban_zi_result$p.value, 4), round(complex_agri_zi_result$p.value, 4),
                     round(agri_sig_veg_zi_result$p.value, 4), round(forests_zi_result$p.value, 4),
                     round(moors_zi_result$p.value, 4), round(woodland_zi_result$p.value, 4),
                     round(sparse_zi_result$p.value, 4)),
  ZI_Needed = c(ifelse(urban_needs_zi, "YES", "NO"), ifelse(complex_agri_needs_zi, "YES", "NO"),
                ifelse(agri_sig_veg_needs_zi, "YES", "NO"), ifelse(forests_needs_zi, "YES", "NO"),
                ifelse(moors_needs_zi, "YES", "NO"), ifelse(woodland_needs_zi, "YES", "NO"),
                ifelse(sparse_needs_zi, "YES", "NO")),
  Model_Type = c(model_type_urban, model_type_complex_agri, model_type_agri_sig_veg,
                 model_type_forests, model_type_moors, model_type_woodland, model_type_sparse),
  Best_Model = c(
    ifelse(urban_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(complex_agri_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(agri_sig_veg_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(forests_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(moors_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(woodland_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction"),
    ifelse(sparse_aic_comparison[1, "dAIC"] == 0, "Interaction", "No Interaction")
  ),
  Delta_AIC = c(
    round(max(urban_aic_comparison$dAIC), 2),
    round(max(complex_agri_aic_comparison$dAIC), 2),
    round(max(agri_sig_veg_aic_comparison$dAIC), 2),
    round(max(forests_aic_comparison$dAIC), 2),
    round(max(moors_aic_comparison$dAIC), 2),
    round(max(woodland_aic_comparison$dAIC), 2),
    round(max(sparse_aic_comparison$dAIC), 2)
  )
)

cat("\n=== SUMMARY TABLE ===\n")
print(summary_results)

# Save summary table
write.csv(summary_results, here("data", "derived_data", "individual_landcover_model_summary.csv"), 
          row.names = FALSE)

cat("\n=== INDIVIDUAL MODEL COMPARISONS ===\n")
cat("Urban:\n"); print(urban_aic_comparison)
cat("\nComplex Agricultural:\n"); print(complex_agri_aic_comparison)
cat("\nAgriculture with Significant Vegetation:\n"); print(agri_sig_veg_aic_comparison)
cat("\nForests:\n"); print(forests_aic_comparison)
cat("\nMoors, Heathland, Grassland:\n"); print(moors_aic_comparison)
cat("\nTransitional Woodland Shrub:\n"); print(woodland_aic_comparison)
cat("\nSparse Vegetation:\n"); print(sparse_aic_comparison)

cat("\nScript completed successfully!\n")
cat("All models saved in: data/models/\n")
cat("Summary table saved as: data/derived_data/individual_landcover_model_summary.csv\n")

# Additional diagnostics summary
cat("\n=== ZERO-INFLATION DIAGNOSTICS ===\n")
cat("Land cover types requiring zero-inflation:\n")
zi_needed_types <- summary_results[summary_results$ZI_Needed == "YES", "Land_Cover_Type"]
if(length(zi_needed_types) > 0) {
  cat(paste("-", zi_needed_types, collapse = "\n"), "\n")
} else {
  cat("None\n")
}

cat("\nLand cover types using standard negative binomial:\n")
zi_not_needed_types <- summary_results[summary_results$ZI_Needed == "NO", "Land_Cover_Type"]
if(length(zi_not_needed_types) > 0) {
  cat(paste("-", zi_not_needed_types, collapse = "\n"), "\n")
} else {
  cat("None\n")
}

# END OF SCRIPT ----------------------------------------------------------------