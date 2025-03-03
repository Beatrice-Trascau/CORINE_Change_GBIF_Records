##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.2_intens_extens_cover_change_occ_model_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (intensification/extensification) on the number of 
# occurrences in a pixel
##----------------------------------------------------------------------------##

# 1. LOAD AND PREPARE DATA FOR ANALYSIS ----------------------------------------

# Load data
load(here("data","derived_data", 
          "occurrences_SSB_municipalities_land_cover.rda"))

# Rename df (to make it easier to work with)
occ_SSB_land_cover <- occurrence_municipalities_df

# 2. CALCULATE NUMBER OF RECORDS FOR EACH PERIOD -------------------------------

# Change column names for easier df manipulation
occ_SSB_land_cover <- occ_SSB_land_cover |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

## 2.1. First period of change: 2000-2006 --------------------------------------

### 2.1.1. Before land cover change --------------------------------------------

# Prepare data for the period 2006-2009
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = land_cover2000 - land_cover2006) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 2006-2009
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

### 2.1.2. After land cover change ---------------------------------------------

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = land_cover2000 - land_cover2006) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 2006-2009
occ_df_after_2000.2006_records <- occ_df_after_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 2.2. Second period of change: 2006 - 2012 -----------------------------------

### 2.2.1. Before land cover change --------------------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = land_cover2006 - land_cover2012) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 2003-2006
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

### 2.2.2. After land cover change ---------------------------------------------

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = land_cover2006 - land_cover2012) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 2012-2015
occ_df_after_2006.2012_records <- occ_df_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 2.3. Third period of change: 2012 - 2018 ------------------------------------

### 2.3.1. Before land cover change --------------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = land_cover2012 - land_cover2018) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

### 2.3.2. After land cover change ---------------------------------------------

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = land_cover2012 - land_cover2018) |>
  mutate(cover_change = case_when(
    cover_change %in% c(79, 102, 249, 379, 589, 710,
                        23, 147, 170, 608, 631, -102,
                        -79, 487, 510, 277, 300, -340,
                        -331, -461, -130) ~ "Intensification",
    cover_change %in% c(-249, -379, -589, -170, -300, -510,
                        -147, -277, -487, 130, -210, 461, 331,
                        121, 340, -23, -710, -121, -608) ~ "Extensification",
    cover_change == 0 ~ "No_change"))

# Calculate number of records for the period 1997-2000
occ_df_after_2012.2018_records <- occ_df_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

# 3. COMBINE DATAFRAMES  -------------------------------------------------------

## 3.1. Combine before and after dfs -------------------------------------------

# Before land cover change
occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-NAME_2)

# After land cover change
occ_intens_extens_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                       occ_df_after_2006.2012_records,
                                                       occ_df_after_2012.2018_records) |>
  select(-NAME_2)


## 3.2. Check for SSBID differences --------------------------------------------

cell_ssbid_mapping <- full_join(
  select(occ_df_before_records, cell_ID, time_period, SSBID_before = SSBID),
  select(occ_intens_extens_after_records_for_model, cell_ID, time_period, SSBID_after = SSBID),
  by = c("cell_ID", "time_period")) |>
  # Flag cells with actual discrepancies (both values present but different)
  mutate(has_discrepancy = !is.na(SSBID_before) & !is.na(SSBID_after) & 
           SSBID_before != SSBID_after)

# Extract only the truly discrepant cells
discrepant_cells <- cell_ssbid_mapping |>
  filter(has_discrepancy) |>
  select(cell_ID, time_period)

# Count discrepancies for documentation
n_discrepant_cells <- nrow(discrepant_cells)
message("Removing ", n_discrepant_cells, " cell/time_period combinations with inconsistent SSBID assignments")

# Remove only the discrepant cells
occ_df_before_records_consistent <- occ_df_before_records |>
  anti_join(discrepant_cells, by = c("cell_ID", "time_period"))

occ_df_after_records_consistent <- occ_intens_extens_after_records_for_model |>
  anti_join(discrepant_cells, by = c("cell_ID", "time_period"))

## 3.3. Join datasets ----------------------------------------------------------

# Join datasets using only essential identifiers
occ_df_before_after <- full_join(
  occ_df_before_records_consistent,
  occ_df_after_records_consistent,
  by = c("cell_ID", "time_period", "SSBID"))

# Process the joined dataset to create final model-ready data
occ_intens_extens_before_after_for_model <- occ_df_before_after |>
  mutate(
    # Reconcile cover_change values
    cover_change = case_when(
      !is.na(cover_change.x) & !is.na(cover_change.y) ~ cover_change.x, 
      !is.na(cover_change.x) & is.na(cover_change.y) ~ cover_change.x,
      is.na(cover_change.x) & !is.na(cover_change.y) ~ cover_change.y,
      TRUE ~ NA_character_),
    
    # Handle occurrence counts
    ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before),
    ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
    
    # Consolidate gbifID
    gbifID = coalesce(gbifID.x, gbifID.y),
    
    # Convert to factors for modeling
    SSBID = as.factor(SSBID),
    cell_ID = as.factor(cell_ID)) |>
  # Remove redundant columns
  select(-matches("\\.x$|\\.y$"))

# Verify no duplicates exist in the final dataset
duplicate_check <- occ_intens_extens_before_after_for_model |>
  group_by(cell_ID, time_period) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1)

if(nrow(duplicate_check) > 0) {
  warning("Duplicate cell_ID/time_period combinations found in final dataset!")
  print(head(duplicate_check))
} else {
  message("Verified: No duplicate cell_ID/time_period combinations in final dataset")
}

# Write dataframe to file
save(occ_intens_extens_before_after_for_model,
     file = here::here("data", "derived_data",
                       "occ_intens_extens_before_after_for_model.rda"))

# 4. MODEL 2: OCC ~ COVER CHANGE + OFFSET --------------------------------------

# Relevel cover_change to have 'No_change' as the reference
occ_intens_extens_before_after_for_model$cover_change <- as.factor(occ_intens_extens_before_after_for_model$cover_change)
occ_intens_extens_before_after_for_model$cover_change <- relevel(occ_intens_extens_before_after_for_model$cover_change, 
                                                                 ref = "No_change")

## 4.1. N binomial glmmTMB, nbinom 2, SSBID + Interaction ----------------------

# Run model
IntensExtens_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                               family = nbinom2,
                                               data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(IntensExtens_model1_SSB_interaction, file = here::here("data", "models",
                                                            "IntensExtens_model1_SSB_interaction.RData"))


## 4.2. N binomial glmmTMB, nbinom 2, SSBID, no interaction --------------------

# Run model
IntensExtens_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                                  family = nbinom2,
                                                  data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(IntensExtens_model2_SSB_no_interaction, file = here::here("data", "models",
                                                               "IntensExtens_model2_SSB_no_interaction.RData"))

## 4.3. Compare models ------------------------------------------------------------

# Offset
# AICctab(IntensExtens_model1_SSB_interaction, IntensExtens_model2_SSB_no_interaction, 
#         base = TRUE)
#deltaAIC = 

# 5. CHECK MARGINAL VALUE IMPACT -----------------------------------------------

## 5.1 Model with 0.1 offset ---------------------------------------------------

# Run model
IntensExtens_model3_SSB_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                              family = nbinom2,
                                              data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(IntensExtens_model3_SSB_0.1_offset, file = "IntensExtens_model3_SSB_0.1_offset.RData")

## 5.2 Model with 0.01 offset --------------------------------------------------

# Run negative binomial model
IntensExtens_model4_SSB_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                               family = nbinom2,
                                               data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(IntensExtens_model4_SSB_0.01_offset, file = "IntensExtens_model4_SSB_0.01_offset.RData")

## 5.3. Model with 0.1 offset and no interaction -------------------------------

# Run model
IntensExtens_model5_SSB_no_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                             family = nbinom2,
                                                             data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(IntensExtens_model5_SSB_no_interaction_0.1_offset, file = here::here("data", "models", 
                                                                          "IntensExtens_model5_SSB_no_interaction_0.1_offset.RData"))

# END OF SCRIPT ----------------------------------------------------------------