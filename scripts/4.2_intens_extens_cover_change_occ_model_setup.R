##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.2_intens_extens_cover_change_occ_model_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (intensification/extensification) on the number of 
# occurrences in a pixel
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
                 "writexl", "bbmle", "kableExtra", "googledrive") # specify packages

# Execute the function
sapply(package_vec, install_load_package)

# 1. LOAD AND PREPARE DATA -----------------------------------------------------

# Load data
load(here("data","derived_data", 
          "modeling_data_combined_corine_gbif_ssb_august2025.rda"))


# Convert data from long to wide format for modeling
modeling_data_wide <- modeling_data_filtered |>
  # Select the variables we need for modeling
  select(cell_ID, SSBID, land_cover_start, land_cover_end, land_cover_start_name, 
         land_cover_end_name, cover_change, transition_type, intens_extens,
         time_period, n_occurrences, n_species, analysis_period,
         # Include the taxonomic/metadata lists if you want them in the final model data
         species_list, kingdom_list, phylum_list, class_list, order_list, 
         family_list, publisher_list, datasetName_list) |>
  
  # Reshape from long to wide format
  pivot_wider(
    id_cols = c(cell_ID, SSBID, land_cover_start, land_cover_end, 
                land_cover_start_name, land_cover_end_name, cover_change, 
                transition_type, intens_extens, analysis_period),
    names_from = time_period,
    values_from = c(n_occurrences, n_species, species_list, kingdom_list, 
                    phylum_list, class_list, order_list, family_list, 
                    publisher_list, datasetName_list),
    names_sep = "_"
  ) |>
  
  # Create the before/after columns needed for your model
  mutate(
    # Extract before occurrences (works for all three analysis periods)
    occurrences_before = case_when(
      analysis_period == "2000_2006" ~ n_occurrences_before_2000_2006,
      analysis_period == "2006_2012" ~ n_occurrences_before_2006_2012,
      analysis_period == "2012_2018" ~ n_occurrences_before_2012_2018,
      TRUE ~ NA_real_
    ),
    
    # Extract after occurrences (works for all three analysis periods)  
    occurrences_after = case_when(
      analysis_period == "2000_2006" ~ n_occurrences_after_2000_2006,
      analysis_period == "2006_2012" ~ n_occurrences_after_2006_2012,
      analysis_period == "2012_2018" ~ n_occurrences_after_2012_2018,
      TRUE ~ NA_real_
    ),
    
    # Create a simplified time_period variable for the model
    time_period = case_when(
      analysis_period == "2000_2006" ~ "2000_2006",
      analysis_period == "2006_2012" ~ "2006_2012", 
      analysis_period == "2012_2018" ~ "2012_2018",
      TRUE ~ NA_character_
    )
  ) |>
  
  # Remove rows with missing before/after data (shouldn't happen but just in case)
  filter(!is.na(occurrences_before) & !is.na(occurrences_after)) |>
  
  # Convert factors back for modeling
  mutate(
    cell_ID = as.factor(cell_ID),
    SSBID = as.factor(SSBID),
    cover_change = as.factor(cover_change),
    intens_extens = as.factor(intens_extens),
    time_period = as.factor(time_period)
  )

# 2. MODEL 1: OCC ~ COVER CHANGE + OFFSET --------------------------------------

# Relevel cover_change to have 'No_change' as the reference
modeling_data_wide$intens_extens <- relevel(modeling_data_wide$intens_extens,
                                            ref = "No_change")

## 2.1. N binomial glmmTMB, nbinom 2, SSBID + Interaction ----------------------

IntensExtens_model1 <- glmmTMB(occurrences_after ~ intens_extens * time_period +
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         zi = ~intens_extens + time_period,
                         family = nbinom2,
                         data = modeling_data_wide)


# Save model output to file to save time next time
save(IntensExtens_model1, file = here::here("data", "models",
                                            "IntensExtens_model1_zero_inflated_interaction.RData"))


## 2.2. Zero inflated no interaction -------------------------------------------

# Run model
IntensExtens_model2 <- glmmTMB(occurrences_after ~ intens_extens + time_period +
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          zi = ~intens_extens + time_period,
                          family = nbinom2,
                          data = modeling_data_wide)

# Save model output to file 
save(IntensExtens_model2, file = here::here("data", "models",
                                       "IntensExtens_model2_zero_inflated_no_interaction.RData"))

## 2.3. Zero inflated - more complex structure ---------------------------------

# Run model
IntensExtens_model3 <- glmmTMB(occurrences_after ~ intens_extens * time_period + 
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          zi = ~intens_extens * time_period + (1 | SSBID), 
                          family = nbinom2,
                          data = modeling_data_wide)

# Save model output to file 
save(IntensExtens_model3, file = here::here("data", "models",
                                       "IntensExtens_model3_zero_inflated_interaction.RData"))

## 2.4. Zero inflated - complex structure + interaction ------------------------

# Run model
IntensExtens_model4 <- glmmTMB(occurrences_after ~ intens_extens + time_period + 
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          zi = ~intens_extens * time_period + (1 | SSBID), 
                          family = nbinom2,
                          data = modeling_data_wide)

# Save model output to file 
save(IntensExtens_model4, file = here::here("data", "models",
                                       "IntensExtens_model4_zero_inflated_nointeraction.RData"))

## 2.5. Logged occurrences after + interaction ---------------------------------

# Log transform the occurrences after
modeling_data_wide$log_occurrences_after <- log(modeling_data_wide$occurrences_after + 1)

# Run model
IntensExtens_model5 <- glmmTMB(log_occurrences_after ~ intens_extens * time_period + 
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         family = gaussian,
                         data = modeling_data_wide)

# Save model output
save(IntensExtens_model5, file = here::here("data", "models",
                                      "IntensExtens_model5_logged_interaction.RData"))

## 2.6. Logges occurrences after no interaction --------------------------------

# Run model
IntensExtens_model6 <- glmmTMB(log_occurrences_after ~ intens_extens + time_period + 
                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                         family = gaussian,
                         data = modeling_data_wide)

# Save model output
save(IntensExtens_model6, file = here::here("data", "models",
                                      "IntensExtens_model6_logged_nointeraction.RData"))