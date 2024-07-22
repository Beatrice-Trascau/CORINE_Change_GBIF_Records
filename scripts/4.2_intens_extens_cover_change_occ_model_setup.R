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
                 "rgbif", "CoordinateCleaner") # specify packages

# Execute the function
sapply(package_vec, install_load_package)


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
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

### 2.1.2. After land cover change ---------------------------------------------

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 2.2. Second period of change: 2006 - 2012 -----------------------------------

### 2.2.1. Before land cover change --------------------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

### 2.2.2. After land cover change ---------------------------------------------

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 2.3. Third period of change: 2012 - 2018 ------------------------------------

### 2.3.1. Before land cover change --------------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

### 2.3.2. After land cover change ---------------------------------------------

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
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
    NAME_2 = first(NAME_2)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 2.4. Combine dataframes ------------------------------------------------------

# Before land cover change
occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

# After land cover change
occ_intens_extens_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                       occ_df_after_2006.2012_records,
                                                       occ_df_after_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

# Full join before and after dfs on SSBID and time period
occ_df_before_after <- full_join(occ_intens_extens_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_intens_extens_before_after_for_model <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before))

# Write dataframe to file
save(occ_intens_extens_before_after_for_model, 
     file = here::here("data", "derived_data",
                       "occ_intens_extens_before_after_for_model.rda"))

# 3. MODEL 2: OCC ~ COVER CHANGE -----------------------------------------------

## 3.1. N binomial with glmmTMB, nbiom1, SSB ID --------------------------------

# Model 2.1
# model2.1_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
#                                        family = nbinom1, 
#                                        data = occ_intens_extens_before_after_for_model)
# Save output to file
# save(model2.1_SSB, file = here::here("data", "models", "model2.1_SSB.RData"))

## 3.2. N binomial with glmmTMB, nbiom1, Municipality --------------------------

# Model 2.2
# model2.2_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
#                                        family = nbinom1, 
#                                        data = occ_intens_extens_before_after_for_model)
# Save output to file
# save(model2.2_municipality, file = here::here("data", "models", 
#                                                       "model2.2_municipality.RData"))
## 3.3. N binomial with glmmTMB, nbiom2, SSBID ---------------------------------

# Model 2.3
# model2.3_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
#                                          family = nbinom2, 
#                                          data = occ_intens_extens_before_after_for_model)

# Save output to file
# save(model2.3_SSB, file = here::here("data", "models", "model2.3_SSB.RData"))

## 3.4. N binomial with glmmTMB, nbiom2, Municipality --------------------------

# Model 2.4
# model2.4_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
#                                          family = nbinom2, 
#                                          data = occ_intens_extens_before_after_for_model)

# Save output to file
# save(model2.4_SSB, file = here::here("data", "models","model2.4_SSB.RData"))

# 4. MODEL 2: OCC ~ COVER CHANGE + OFFSET --------------------------------------

## 4.1. N binomial glmmTMB, nbinom 2, SSBID ------------------------------------

# Run model
model2.5_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(log(ocurrences_before + 0.0001)) + (1 | SSBID),
                    family = nbinom2,
                    data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(model2.5_SSB, file = here::here("data", "models", "model2.5_SSB.RData"))

## 4.2. N binomial glmmTMB, nbinom 2, Municipality -----------------------------

# Run model
model2.6_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(log(ocurrences_before + 0.0001)) + (1 | municipality),
                    family = nbinom2,
                    data = occ_intens_extens_before_after_for_model)

# Save model output to file to save time next time
save(model2.6_municipality, file = here::here("data", "models", 
                                              "model2.6_municipality.RData"))

# END OF SCRIPT ----------------------------------------------------------------