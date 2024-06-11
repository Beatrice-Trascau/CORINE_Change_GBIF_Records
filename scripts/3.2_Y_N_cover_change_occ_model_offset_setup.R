##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.2_Y_N_cover_change_occ_model_offset_setup
# This script contains code which runs the models looking at the effect 
# of land cover change (Y/N) and offset on the number of occurrences in a pixel
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
load(here("data", "derived_data", 
          "occurrences_SSB_municipalities_land_cover.rda"))

# Rename df (to make it easier to work with)
occ_SSB_land_cover <- occurrence_municipalities_df

## 1.1. First period of change: 2000-2006 --------------------------------------

# Prepare data for the period 2006-2009
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 2006-2009
occ_df_before_2000.2006_records <- occ_df_before_2000.2006 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2000 = first(land_cover2000),
    land_cover2006 = first(land_cover2006),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 1.2. Second period of change: 2006 - 2012 -----------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

# Calculate number of records for the period 2003-2006
occ_df_before_2006.2012_records <- occ_df_before_2006.2012 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2006 = first(land_cover2006),
    land_cover2012 = first(land_cover2012),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 1.3. Third period of change: 2012 - 2018 ------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

# Calculate number of records for the period 1997-2000
occ_df_before_2012.2018_records <- occ_df_before_2012.2018 |>
  group_by(cell_ID) |>
  summarise(
    ocurrences_before = n(),
    land_cover2012 = first(land_cover2012),
    land_cover2018 = first(land_cover2018),
    cover_change = first(cover_change),
    SSBID = first(SSBID),
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 1.4. Combine the above 3 into a single df -----------------------------------

occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)


## 1.5. Combine occ_df_before_records wtih occ_df_after_records ----------------

# Read in dataframe with occurrences after a change
load(here("data", "derived_data", 
          "occ_y_n_cover_change_after_records_for_model.rda"))

# Full join of the dfs on SSBid and time_period
occ_df_before_after <- full_join(occ_y_n_cover_change_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_y_n_cover_change_before_after_for_modell <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before))


## 1.6. Write dataframe to file ------------------------------------------------
save(occ_y_n_cover_change_before_after_for_modell, 
     file = here::here("data", "derived_data",
                       "occ_y_n_cover_change_before_after_for_modell.rda"))

# 2. MODEL 1: OCC ~ COVER CHANGE + OFFSET  -------------------------------------

## 2.1. Negative binomial with glmer.nb ----------------------------------------

# Run negative binomial model
# model2.1_nb <- glmer.nb(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                         data = occ_y_n_cover_change_before_after_for_modell)
# 
# Save model output to file to save time next time
# save(model2.1_nb, file = here::here("data", "models", "model2.1_nb.RData"))

# Create a 10% subset of the data
subset_data <- occ_y_n_cover_change_before_after_for_modell |>
  sample_frac(0.1)

# Save subset
save(subset_data, 
     file = here::here("data", "derived_data", "subset_data.rda"))

# Load data subset
load(here("data", "derived_data", 
          "subset_data.rda"))

## 2.2. Negative binomial with glmer.nb on subset of data ----------------------

# Run negative binomial model
# model2.2_nb <- glmer.nb(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                         data = subset_data)

# Save model output to file to save time next time
#save(model2.2_nb, file = here::here("data", "models", "model2.2_nb.RData"))

## 2.3. N binomial with glmmTMB, family nbinom 1, by SSB ID on data subset -----

# Run negative binomial model
# model2.3_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
#                        family = nbinom1,
#                        data = subset_data)

# Save model output to file to save time next time
#save(model2.3_nb, file = here::here("data", "models", "model2.3_nb.RData"))


## 2.4. N binomial glmmTMB, family nbinom1, by Municipality on data subset -----

# Run negative binomial model
# model2.3_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | Municipality),
#                        family = nbinom1,
#                        data = subset_data)

# Save model output to file to save time next time
#save(model2.3_nb, file = here::here("data", "models", "model2.3_nb.RData"))

## 2.5.N binomial glmmTMB, nbinom2, SSBID on data subset -----------------------

# Run negative binomial model
model2.5_nb <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(ocurrences_before) + (1 | SSBID),
                       family = nbinom2,
                       data = subset_data)

# Save model output to file to save time next time
save(model2.5_nb, file = here::here("data", "models", "model2.5_nb.RData"))

# END OF SCRIPT ----------------------------------------------------------------