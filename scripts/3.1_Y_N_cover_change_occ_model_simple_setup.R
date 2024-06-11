##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.1_Y_N_cover_change_occ_model_simple_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (Y/N) on the number of occurrences in a pixel
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
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

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
  mutate(cover_change = if_else(land_cover2006 == land_cover2012, "N", "Y"))

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
  mutate(cover_change = if_else(land_cover2012 == land_cover2018, "N", "Y"))

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

occ_y_n_cover_change_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                          occ_df_after_2006.2012_records,
                                                          occ_df_after_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

## 1.5. Write dataframe to file ------------------------------------------------
save(occ_y_n_cover_change_after_records_for_model, 
     file = here::here("data", "derived_data",
                       "occ_y_n_cover_change_after_records_for_model.rda"))

# 2. MODEL 1: OCC ~ COVER CHANGE -----------------------------------------------

## 2.1. Negative binomial with glmer.nb ----------------------------------------

# Run negative binomial model
model1_nb <- glmer.nb(ocurrences_after ~ cover_change * time_period + (1|municipality), 
                      data = occ_y_n_cover_change_after_records_for_model)

# Save model output to file to save time next time
save(model1_nb, file = here::here("data", "models", "model1_nb.RData"))


## 2.2. Negative binomial with glmmTMB, family nbinom 1, by SSB ID -------------

# Run negative binomial model
model1.2_nb_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                           family = nbinom1, 
                           data = occ_y_n_cover_change_after_records_for_model)

# Save model output to file to save time next time
save(model1.2_nb_SSB, file = here::here("data", "models", 
                                        "model1.2_nb_SSB.RData"))


## 2.3. Negative binomial with glmmTMB, family nbinom 1, by Municipality -------

# Run negative binomial model
model1.3_nb_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                    family = nbinom1, 
                                    data = occ_y_n_cover_change_after_records_for_model)

# Save model output to file to save time next time
save(model1.3_nb_municipality, file = here::here("data", "models", 
                                                 "model1.3_nb_municipality.RData"))

## 2.4. N binomial with glmmTMB, family nbinom 1 (logit link), by SSB ID -------

# Run negative binomial model with logit link
model1.4_nb_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
                           family = nbinom1(link = "logit"), 
                           data = occ_y_n_cover_change_after_records_for_model)

# Save model output to file to save time next time
save(model1.4_nb_SSB, file = here::here("data", "models", 
                                        "model1.4_nb_SSB.RData"))

## 2.5. N binomial glmmTMB, family nbinom 1 (logit link), by Municipality ------

# Run negative binomial model with logit link
model1.5_nb_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
                                    family = nbinom2(link = "logit"), 
                                    data = occ_y_n_cover_change_after_records_for_model)

# Save model output to file to save time next time
save(model1.5_nb_municipality, file = here::here("data", "models", 
                                                 "model1.5_nb_municipality.RData"))


# END OF SCRIPT ----------------------------------------------------------------