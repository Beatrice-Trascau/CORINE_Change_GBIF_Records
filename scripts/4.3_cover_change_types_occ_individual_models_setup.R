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

# Check if you have enough observations to fit the models
urban_data_check <- urban_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(urban_data_check) 
# too few observations of change from Urban to be able to include it in a model

# Run model with just time
urban_temporal_model <- glmmTMB(occurrences_after ~ time_period +
                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                family = nbinom2,  # or zi if needed
                                data = urban_data)

# Save model output
save(urban_temporal_model, 
     file = here("data", "models","urban_model1_time.RData"))

# Check model output
summary(urban_temporal_model)

# Check model fit
urban_zi_test <- simulateResiduals(urban_temporal_model)
zi_result <- testZeroInflation(urban_zi_test) # fewer 0s than expected

# Plot DHARMa output
plot(urban_zi_test)

# Set up file output
png(here("figures", "FigureS10_UF_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(urban_zi_test, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(urban_zi_test, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 3. MODELS FOR COMPLEX AGRICULTURAL -------------------------------------------

# Filter for complex agricultural starting land cover
complex_agri_data <- modeling_data_wide |>
  filter(land_cover_start_name == "complex_agri")

# Check if you have enough observations to fit the models
complex_agri_data_check <- complex_agri_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(complex_agri_data_check) # enough for almost all transitions

# Set reference level to "no change" (complex_agri to complex_agri)
complex_agri_data$transition_type <- relevel(complex_agri_data$transition_type, 
                                             ref = "complex_agri_to_complex_agri")

# Check number of observations
cat("Complex agri data: ", nrow(complex_agri_data), " observations\n")
cat("Complex agri transitions: ", paste(levels(complex_agri_data$transition_type), collapse = ", "), "\n")

## 3.1. Complex Agri Model 1: With Interaction ---------------------------------

# Run model 
complex_agri_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                family = nbinom2,
                                data = complex_agri_data)

# Save output
save(complex_agri_model1_interaction, file = here("data", "models","complex_agri_model1_interaction.RData"))

## 3.2. Complex Agri Model 2: No Interaction -----------------------------------

# Run model
complex_agri_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                           family = nbinom2,
                                           data = complex_agri_data)

# Save output
save(complex_agri_model2_no_interaction, 
     file = here("data", "models","complex_agri_model2_no_interaction.RData"))

## 3.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(complex_agri_model1_interaction, 
       complex_agri_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
# complex_agri_model1_interaction    617198.0      0.0 16
# complex_agri_model2_no_interaction 617204.5      6.5 9  - interaction model preferred

# Check model with DHARMa
cac_simulation <- simulateResiduals(complex_agri_model2_no_interaction)

# Check DHARMa output
plot(cac_simulation)

# Set up file output
png(here("figures", "FigureS11_CAC_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(cac_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(cac_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 4. MODELS FOR AGRICULTURE WITH SIGNIFICANT VEGETATION ------------------------

# Filter for agri_sig_veg starting land cover
agri_sig_veg_data <- modeling_data_wide |>
  filter(land_cover_start_name == "agri_sig_veg")

# Check if you have enough observations to fit the models
agri_sig_veg_data_check <- agri_sig_veg_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(agri_sig_veg_data_check) 
 
# Set reference level to "no change" (agri_sig_veg to agri_sig_veg)
agri_sig_veg_data$transition_type <- relevel(agri_sig_veg_data$transition_type, 
                                             ref = "agri_sig_veg_to_agri_sig_veg")

# Remove transitions with too few observations and very small sample sizes
agri_sig_veg_data_filtered <- agri_sig_veg_data |>
  group_by(transition_type) |>
  filter(n() >= 25) |>
  ungroup() |>
  droplevels()

# Check number of observations
cat("Agri sig veg data: ", nrow(agri_sig_veg_data_filtered), " observations\n")
cat("Agri sig veg transitions: ", paste(levels(agri_sig_veg_data_filtered$transition_type), collapse = ", "), "\n")

## 4.1. Agri Sig Veg Model 1: With Interaction ---------------------------------

# Run model
agri_sig_veg_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                                  offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                family = nbinom2,
                                data = agri_sig_veg_data_filtered)

# Save output
save(agri_sig_veg_model1_interaction, file = here("data", "models",
                                                  "agri_sig_veg_model1_interaction.RData"))

## 4.2. Agri Sig Veg Model 2: No Interaction -----------------------------------

# Run model
agri_sig_veg_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                           family = nbinom2,
                                           data = agri_sig_veg_data_filtered)

# Save output
save(agri_sig_veg_model2_no_interaction, file = here("data", "models",
                                                     "agri_sig_veg_model2_no_interaction.RData"))

## 4.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(agri_sig_veg_model1_interaction, 
       agri_sig_veg_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
# agri_sig_veg_model1_interaction    781995.2      0.0 14
# agri_sig_veg_model2_no_interaction 782013.6     18.5 8 

# Check output
summary(agri_sig_veg_model1_interaction)

# Check model with DHARMa
asnv_simulation <- simulateResiduals(agri_sig_veg_model1_interaction)

# Check DHARMa output
plot(asnv_simulation)

# Set up file output
png(here("figures", "FigureS12_ASNV_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(asnv_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(asnv_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 5. MODELS FOR FORESTS --------------------------------------------------------

# Filter for forests starting land cover
forests_data <- modeling_data_wide |>
  filter(land_cover_start_name == "forests")

# Check if you have enough observations to fit the models
forests_data_check <- forests_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(forests_data_check) # everything except for F -> SVA areas looks ok

# Set reference level to "no change" (forests to forests)
forests_data$transition_type <- relevel(forests_data$transition_type, 
                                        ref = "forests_to_forests")

# Remove forests_to_sparse_veg transition - 
forests_data_filtered <- forests_data |>
  filter(transition_type != "forests_to_sparse_veg") |>
  droplevels()

# Check observations
cat("Forests data: ", nrow(forests_data_filtered), " observations\n")
cat("Forests transitions: ", paste(levels(forests_data_filtered$transition_type), collapse = ", "), "\n")

## 5.1. Forests Model 1: With Interaction ---------------------------------------

# Run model
forests_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                           family = nbinom2,
                           data = forests_data_filtered)

# Save output
save(forests_model1_interaction, file = here("data", "models", 
                                             "forests_model1_interaction.RData"))

## 5.2. Forests Model 2: No Interaction -----------------------------------------

# Run model
forests_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                        offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                      family = nbinom2,
                                      data = forests_data_filtered)

# Save output
save(forests_model2_no_interaction, file = here("data", "models",
                                                "forests_model2_no_interaction.RData"))

## 5.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(forests_model1_interaction, 
       forests_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
# forests_model1_interaction    2619915.5       0.0 20
# forests_model2_no_interaction 2619939.3      23.7 10

# Check output
summary(forests_model1_interaction)

# Check model with DHARMa
forest_simulation <- simulateResiduals(forests_model1_interaction)

# Check DHARMa output
plot(forest_simulation)

# Set up file output
png(here("figures", "FigureS13_Forest_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(forest_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(forest_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 6. MODELS FOR MOORS, HEATHLAND AND GRASSLAND ---------------------------------

# Filter for moors_heath_grass starting land cover
moors_data <- modeling_data_wide |>
  filter(land_cover_start_name == "moors_heath_grass")

# Check if you have enough observations to fit the models
moors_data_check <- moors_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(moors_data_check) # ok

# Set reference level to "no change" (moors_heath_grass to moors_heath_grass)
moors_data$transition_type <- relevel(moors_data$transition_type, 
                                      ref = "moors_heath_grass_to_moors_heath_grass")

# Check observations
cat("Moors data: ", nrow(moors_data), " observations\n")
cat("Moors transitions: ", paste(levels(moors_data$transition_type), collapse = ", "), "\n")


## 6.1. Moors Model 1: With Interaction -----------------------------------------

# Run model
moors_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         family = nbinom2,
                         data = moors_data)

# Save output
save(moors_model1_interaction, file = here("data", "models","moors_model1_interaction.RData"))

## 6.2. Moors Model 2: No Interaction -------------------------------------------

# Run model
moors_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                      offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                    family = nbinom2,
                                    data = moors_data)

# Save output
save(moors_model2_no_interaction, file = here("data", "models","moors_model2_no_interaction.RData"))

## 6.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(moors_model1_interaction, 
       moors_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
# moors_model1_interaction    374322.4      0.0 21
# moors_model2_no_interaction 374323.6      1.2 11

# Check output
summary(moors_model1_interaction) # major model failure
summary(moors_model2_no_interaction) # looks much better, lack of interaction term means the model pools all time periods together
# and is better able to estimate effects, also dAIC is only 1.2

# Check model with DHARMa
moors_simulation <- simulateResiduals(moors_model2_no_interaction)

# Check DHARMa output
plot(moors_simulation)

# Set up file output
png(here("figures", "FigureS14_MHG_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(moors_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(moors_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 7. MODELS FOR TRANSITIONAL WOODLAND SHRUB ------------------------------------

# Filter for woodland_shrub starting land cover
woodland_data <- modeling_data_wide |>
  filter(land_cover_start_name == "woodland_shrub")

# Check if you have enough observations to fit the models
woodland_data_check <- woodland_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(woodland_data_check) # only forests look ok

# Set reference level to "no change" (woodland_shrub to woodland_shrub)
woodland_data$transition_type <- relevel(woodland_data$transition_type, 
                                         ref = "woodland_shrub_to_woodland_shrub")

# Remove transitions with too few observations
woodland_shrub_data_filtered <- woodland_data |>
  filter(transition_type %in% c("woodland_shrub_to_woodland_shrub",
                                "woodland_shrub_to_forests", 
                                "woodland_shrub_to_urban")) |>
  droplevels()

# Check the number of observations
cat("Woodland data: ", nrow(woodland_shrub_data_filtered), " observations\n")
cat("Woodland transitions: ", paste(levels(woodland_shrub_data_filtered$transition_type), collapse = ", "), "\n")

## 7.1. Woodland Model 1: With Interaction --------------------------------------

# Run model
woodland_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                              offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                            family = nbinom2,
                            data = woodland_shrub_data_filtered)

# Save model
save(woodland_model1_interaction, file = here("data", "models","woodland_model1_interaction.RData"))

## 7.2. Woodland Model 2: No Interaction ----------------------------------------

# Run model
woodland_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                         offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                       family = nbinom2,
                                       data = woodland_shrub_data_filtered)

# Save model
save(woodland_model2_no_interaction, file = here("data", "models","woodland_model2_no_interaction.RData"))


## 7.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(woodland_model1_interaction, 
       woodland_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
# woodland_model2_no_interaction 127692.8      0.0 7 
# woodland_model1_interaction    127762.6     69.8 19

# Check output
summary(woodland_model2_no_interaction)

# Check model with DHARMa
tws_simulation <- simulateResiduals(woodland_model2_no_interaction)

# Check DHARMa output
plot(tws_simulation)

# Set up file output
png(here("figures", "FigureS15_TWS_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(tws_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(tws_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 8. MODELS FOR SPARSE VEGETATION -----------------------------------------------

# Filter for sparse_veg starting land cover
sparse_data <- modeling_data_wide |>
  filter(land_cover_start_name == "sparse_veg")

# Check if you have enough observations to fit the models
sparse_data_check <- sparse_data |>
  count(transition_type, time_period) |>
  spread(time_period, n, fill = 0)

# Check output
print(sparse_data_check) # only urban look ok

# Set reference level to "no change" (sparse_veg to sparse_veg)
sparse_data$transition_type <- relevel(sparse_data$transition_type, 
                                       ref = "sparse_veg_to_sparse_veg")

# Remove transitions with too few observations
sparse_data_filtered <- sparse_data |>
  filter(transition_type %in% c("sparse_veg_to_sparse_veg",
                                "sparse_veg_to_agri_sig_veg", 
                                "sparse_veg_to_urban")) |>
  droplevels()

# Check number of observations
cat("Sparse data: ", nrow(sparse_data_filtered), " observations\n")
cat("Sparse transitions: ", paste(levels(sparse_data_filtered$transition_type), collapse = ", "), "\n")

## 8.1. Sparse Model 1: With Interaction ----------------------------------------

# Run model
sparse_model1_interaction <- glmmTMB(occurrences_after ~ transition_type * time_period +
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          family = nbinom2,
                          data = sparse_data_filtered)

# Save model
save(sparse_model1_interaction, file = here("data", "models",
                                            "sparse_model1_interaction.RData"))


## 8.2. Sparse Model 2: No Interaction ------------------------------------------

# Run model
sparse_model2_no_interaction <- glmmTMB(occurrences_after ~ transition_type + time_period +
                                       offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                                     family = nbinom2,
                                     data = sparse_data_filtered)

# Save model
save(sparse_model2_no_interaction, file = here("data", "models",
                                            "sparse_model2_no_interaction.RData"))

## 8.3. Compare and validate models --------------------------------------------

# Compare models
AICtab(sparse_model1_interaction, 
       sparse_model2_no_interaction, base = TRUE)

# AIC      dAIC     df
#     335466.9      0.0 11
# sparse_model2_no_interaction 335467.9      1.0 7

# Check output
summary(sparse_model1_interaction)

# Check model with DHARMa
sva_simulation <- simulateResiduals(sparse_model1_interaction)

# Check DHARMa output
plot(sva_simulation)

# Set up file output
png(here("figures", "FigureS16_SVA_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(sva_simulation, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(sva_simulation, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

# 9. SUPPLEMENTARY FIGURE - VIOLINS --------------------------------------------

# Create clean labels for facet order
lc_pretty <- c("urban" = "Urban fabric", "complex_agri" = "Complex agricultural",
               "agri_sig_veg" = "Agriculture w/ significant vegetation",
               "forests" = "Forests", 
               "moors_heath_grass" = "Moors, heath & grassland",
               "woodland_shrub" = "Transitional woodland/shrub",
               "sparse_veg" = "Sparse vegetation")

# Create the panel order we want
cover_order <- unname(lc_pretty)

# Prepare dataframe for plotting
plot_df <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after,
         time_period = forcats::fct_recode(time_period, 
                                           `2000-2006` = "2000_2006",
                                           `2006-2012` = "2006_2012",
                                           `2012-2018` = "2012_2018"),
         initial_cover = forcats::fct_relabel(land_cover_start_name, ~ lc_pretty[.x]))

# Intialise list of plot
plot_list <- list()
available_covers <- intersect(cover_order, unique(plot_df$initial_cover))

# Loop to create violin plots for each cover category
for (cover in available_covers) {
  p <- plot_df |>
    filter(initial_cover == cover) |>
    ggplot(aes(x = time_period, y = total_occurrences + 0.1)) +
    geom_violin(fill = "#66c2a5", color = NA, trim = TRUE) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.08, size = 0.4) +
    scale_y_log10(breaks = scales::log_breaks(),
                  labels = scales::label_number(big.mark = ",", trim = TRUE)) +
    labs(x = "Time Period", y = "Total Occurrences (log)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 11))
  plot_list[[cover]] <- p
}

# Create labels for plot
labels_panels <- c("a)", "b)", "c)", "d)", "e)", "f)", "g)")[seq_along(plot_list)]

# Combine panels
combined_violins_change_types <- plot_grid(plotlist = plot_list[available_covers],
                                           labels = labels_panels,
                                           ncol = 3, align = "hv")

# Save plot
ggsave(here("figures", "FigureS9_occs_in_cover_change_types.png"),
       plot = combined_violins_change_types,
       width = 16, height = 10, dpi = 300)

# 10. SUPPLEMENTARY TABLE - SUMMARY STATISTICS ---------------------------------

# Re-format dataframe to fit for the table
tbl_df <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after,
         time_period = forcats::fct_recode(time_period,
                                           `2000-2006` = "2000_2006",
                                           `2006-2012` = "2006_2012",
                                           `2012-2018` = "2012_2018"),
         initial_cover = recode(as.character(land_cover_start_name), !!!lc_pretty, .default = as.character(land_cover_start_name)),
         end_cover = recode(as.character(land_cover_end_name),   !!!lc_pretty, .default = as.character(land_cover_end_name)))

# Create summary statistics per period, intial land cover and end land cover
by_period <- tbl_df |>
  group_by(initial_cover, end_cover, time_period) |>
  summarise(Count = n(),
            Zeros = sum(total_occurrences == 0, na.rm = TRUE),
            Percent_zeros = round(100 * Zeros / Count, 1),
            Total_occurrences = sum(total_occurrences, na.rm = TRUE),
            Median = stats::median(total_occurrences, na.rm = TRUE),
            Pct_75 = as.numeric(stats::quantile(total_occurrences, 0.75, na.rm = TRUE)),
            Pct_90 = as.numeric(stats::quantile(total_occurrences, 0.90, na.rm = TRUE)),
            Maximum = max(total_occurrences, na.rm = TRUE),
            .groups = "drop")

# Summary statistics for all periods
all_periods <- tbl_df |>
  group_by(initial_cover, end_cover) |>
  summarise(time_period = "All periods",
            Count = dplyr::n(),
            Zeros = sum(total_occurrences == 0, na.rm = TRUE),
            Percent_zeros = round(100 * Zeros / Count, 1),
            Total_occurrences = sum(total_occurrences, na.rm = TRUE),
            Median = stats::median(total_occurrences, na.rm = TRUE),
            Pct_75 = quantile(occurrences_after, 0.75),
            Pct_90 = quantile(occurrences_after, 0.90),
            Maximum = max(total_occurrences, na.rm = TRUE),
            .groups = "drop")

# Combine and order the tables
summary_lc_types <- bind_rows(by_period, all_periods) |>
  mutate(time_period = factor(time_period, 
                              levels = c("All periods","2000-2006","2006-2012","2012-2018"))) |>
  arrange(initial_cover, end_cover, time_period)

# Save as csv
write_csv(summary_lc_types, 
          here("figures", "TableS11_occs_land_in_cover_change_types.csv"))

# 11. MODEL SUMMARY REPORTING FOR MANUSCRIPT -----------------------------------

# Extract the coefficient for cover change
cover_change_coef <- 2.900857  # Get it from the table

# Calculate the Incidence Rate Ratio (IRR)
irr <- exp(cover_change_coef)
cat("Incidence Rate Ratio (IRR):", round(irr, 3), "\n")

# Calculate percentage change
percent_change <- (irr - 1) * 100
cat("Percentage change:", round(percent_change, 1), "%\n")

# Calculate confidence intervals using standard error from the model summary
se <- 0.01522
ci_lower <- exp(cover_change_coef - 1.96 * se)
ci_upper <- exp(cover_change_coef + 1.96 * se)
cat("95% CI for IRR:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n")

# END OF SCRIPT ----------------------------------------------------------------