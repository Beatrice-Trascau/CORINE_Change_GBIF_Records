##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.5_cover_change_types_occ_model_simple_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change types on the number of occurrences in a pixel
##----------------------------------------------------------------------------##

# 0. LOAD PACKAGES -------------------------------------------------------------
library(here)
library(dplyr)
library(lattice)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(mgcv)
library(gamm4)
library(ggplot2)

# 1. PREPARE DATA FOR MODELS ---------------------------------------------------

# Load data
load(here("data", "derived_data", "occurrences_SSB_municipalities_land_cover.rda"))

# Rename df (to make it easier to work with)
occ_SSB_land_cover <- occurrence_municipalities_df

## 1.1. First period of change: 2000-2006 --------------------------------------

# Change column names for easier df manipulation
occ_SSB_land_cover <- occ_SSB_land_cover |>
  rename(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  mutate(cover_change = land_cover2000 - land_cover2006)

# Calculate number of records for the period 2006-2009
occ_df_after_2000.2006_records <- occ_df_after_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 1.2. Second period of change: 2006 - 2012 -----------------------------------

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  mutate(cover_change = land_cover2006 - land_cover2012)

# Calculate number of records for the period 2012-2015
occ_df_after_2006.2012_records <- occ_df_after_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 1.3. Third period of change: 2012 - 2018 ------------------------------------

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  mutate(cover_change = land_cover2012 - land_cover2018)

# Calculate number of records for the period 1997-2000
occ_df_after_2012.2018_records <- occ_df_after_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_after = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 1.4. Combine the above 3 into a single df -----------------------------------

occ_cover_change_types_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                          occ_df_after_2006.2012_records,
                                                          occ_df_after_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

## 1.5. Write dataframe to file ------------------------------------------------
save(occ_cover_change_types_after_records_for_model, 
     file = here::here("data", "derived_data",
                       "occ_cover_change_types_after_records_for_model.rda"))

# 2. MODEL 1: OCC ~ COVER CHANGE -----------------------------------------------

## 2.1. Negative binomial with glmer.nb ----------------------------------------

# Create a 10% subset of the data
subset_data_cover_change_types <- occ_cover_change_types_after_records_for_model |>
  sample_frac(0.1)

# Save the subset of the data
save(subset_data_cover_change_types, 
     file = here::here("data", "derived_data", "subset_data_cover_change_types.rda"))

## 2.1. N binomial with glmmTMB, nbiom1, SSB ID --------------------------------

# Model 5.1
model5.1_nb_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                                         family = nbinom1, 
                                         data = subset_data_cover_change_types)
# Save output to file
save(model5.1_nb_SSB, file = here::here("data", "models", 
                                                      "model5.1_nb_SSB.RData"))

## 2.2. N binomial with glmmTMB, nbiom1, Municipality --------------------------

# Model 5.2
model5.2_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                         family = nbinom1, 
                                         data = subset_data_cover_change_types)
# Save output to file
save(model5.2_municipality, file = here::here("data", "models", 
                                                      "model5.2_municipality.RData"))

## 2.3. N binomial with glmmTMB, nbiom1 (logit link), SSB ID -------------------

# Model 5.3
model5.3_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                                         family = nbinom1(link = "logit"), 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.3_SSB, file = here::here("data", "models","model5.3_SSB.RData"))

## 2.4. N binomial with glmmTMB, nbiom1 (logit link), Municipality -------------

# Model 5.4
model5.4_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                         family = nbinom1(link = "logit"), 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.4_municipality, file = here::here("data", "models", 
                                                      "model5.4_municipality.RData"))

## 2.5. N binomial with glmmTMB, nbiom2, SSBID ---------------------------------

# Model 5.5
model5.5_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                                         family = nbinom2, 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.5_SSB, file = here::here("data", "models", "model5.5_SSB.RData"))

## 2.6. N binomial with glmmTMB, nbiom2, Municipality --------------------------

# Model 5.6
model5.6_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                         family = nbinom2, 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.6_municipality, file = here::here("data", "models", 
                                                      "model5.6_municipality.RData"))

## 2.7. N binomial with glmmTMB, nbiom2 (logit link), SSBID --------------------

# Model 5.7
model5.7_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                                         family = nbinom2(link = "logit"), 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.7_SSB, file = here::here("data", "models", "model5.7_SSB.RData"))

## 2.8. N binomial with glmmTMB, nbiom2 (logit link), Municipality -------------

# Model 5.8
model5.8_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                         family = nbinom2(link = "logit"), 
                                         data = subset_data_cover_change_types)

# Save output to file
save(model5.8_SSB, file = here::here("data", "models", "model5.8_SSB.RData"))

# END OF SCRIPT ----------------------------------------------------------------