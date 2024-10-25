##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.3_cover_change_types_occ_model_simple_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change types on the number of occurrences in a pixel
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
         land_cover2018 = U2018_CLC2018_V2020_20u1) |>
  mutate(land_cover2000 = case_when(land_cover2000 == 1 ~ "urban",
                                    land_cover2000 == 80 ~ "complex_agri",
                                    land_cover2000 == 103 ~ "agri_sig_veg",
                                    land_cover2000 == 250 ~ "forests",
                                    land_cover2000 == 380 ~ "moors_heath_grass",
                                    land_cover2000 == 590 ~ "woodland_shrub",
                                    land_cover2000 == 711 ~ "sparse_veg",
                                    is.na(land_cover2000) ~ "other"),
         land_cover2006 = case_when(land_cover2006 == 1 ~ "urban",
                                    land_cover2006 == 80 ~ "complex_agri",
                                    land_cover2006 == 103 ~ "agri_sig_veg",
                                    land_cover2006 == 250 ~ "forests",
                                    land_cover2006 == 380 ~ "moors_heath_grass",
                                    land_cover2006 == 590 ~ "woodland_shrub",
                                    land_cover2006 == 711 ~ "sparse_veg",
                                    is.na(land_cover2006) ~ "other"),
         land_cover2012 = case_when(land_cover2012 == 1 ~ "urban",
                                    land_cover2012 == 80 ~ "complex_agri",
                                    land_cover2012 == 103 ~ "agri_sig_veg",
                                    land_cover2012 == 250 ~ "forests",
                                    land_cover2012 == 380 ~ "moors_heath_grass",
                                    land_cover2012 == 590 ~ "woodland_shrub",
                                    land_cover2012 == 711 ~ "sparse_veg",
                                    is.na(land_cover2012) ~ "other"),
         land_cover2018 = case_when(land_cover2018 == 1 ~ "urban",
                                    land_cover2018 == 80 ~ "complex_agri",
                                    land_cover2018 == 103 ~ "agri_sig_veg",
                                    land_cover2018 == 250 ~ "forests",
                                    land_cover2018 == 380 ~ "moors_heath_grass",
                                    land_cover2018 == 590 ~ "woodland_shrub",
                                    land_cover2018 == 711 ~ "sparse_veg",
                                    is.na(land_cover2018) ~ "other"))

## 2.1. First period of change: 2000-2006 --------------------------------------

### 2.1.1. Before land cover change --------------------------------------------

# Prepare data for the period 2006-2009
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  unite("cover_change", land_cover2000:land_cover2006, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

### 2.1.2. After land cover change ---------------------------------------------

# Prepare data for the period 2006-2009
occ_df_after_2000.2006 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 2006 & year <= 2009) |>
  unite("cover_change", land_cover2000:land_cover2006, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2000-2006") |>
  select(-c(land_cover2000, land_cover2006))

## 2.2. Second period of change: 2006 - 2012 -----------------------------------

### 2.2.1. Before land cover change --------------------------------------------

# Prepare data for the period 2003-2006
occ_df_before_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2003 & year <= 2006) |>
  unite("cover_change", land_cover2006:land_cover2012, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

### 2.2.2. After land cover change ---------------------------------------------

# Prepare data for the period 2012-2015
occ_df_after_2006.2012 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2006, land_cover2012, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2006) & !is.na(land_cover2012)) |>
  filter(year >= 2012 & year <= 2015) |>
  unite("cover_change", land_cover2006:land_cover2012, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2006-2012") |>
  select(-c(land_cover2006, land_cover2012))

## 2.3. Third period of change: 2012 - 2018 ------------------------------------

### 2.3.1. Before land cover change --------------------------------------------

# Prepare data for the period 2009-2012
occ_df_before_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2009 & year <= 2012) |>
  unite("cover_change", land_cover2012:land_cover2018, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

### 2.3.2. After land cover change ---------------------------------------------

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(V1, gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2, XCOOR, YCOOR) |>
  filter(!is.na(land_cover2012) & !is.na(land_cover2018)) |>
  filter(year >= 2015 & year <= 2018) |>
  unite("cover_change", land_cover2012:land_cover2018, remove= FALSE)

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
    XCOOR = first(XCOOR),
    YCOOR = first(YCOOR)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 2.4. Combine in one df  -----------------------------------------------------

# Before land cover change
occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)


# After land cover change
occ_cover_change_types_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                          occ_df_after_2006.2012_records,
                                                          occ_df_after_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

# Full join of the dfs on SSBid and time_period
occ_df_before_after <- full_join(occ_cover_change_types_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_cover_change_types_before_after_for_model <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before),
         cover_change = as.factor(cover_change))

# Write df to file 
save(occ_cover_change_types_before_after_for_model, 
     file = here::here("data", "derived_data",
                       "occ_cover_change_types_before_after_for_model.rda"))


# 3. MODEL 1: OCC ~ COVER CHANGE -----------------------------------------------

# Create subset of data
occ_subset <- occ_cover_change_types_before_after_for_model |> 
  sample_frac(0.1)


## 3.1. N binomial with glmmTMB, nbiom1, SSB ID --------------------------------

# Model 3.1
# model3.1_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
#                                          family = nbinom1, 
#                                          data = occ_subset)
# Save output to file
# save(model3.1_SSB, file = here::here("data", "models", "model3.1_SSB.RData"))

## 3.2. N binomial with glmmTMB, nbiom1, Municipality --------------------------

# Model 3.2
# model3.2_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
#                                          family = nbinom1,
#                                          data = occ_subset)
# Save output to file
# save(model3.2_municipality, file = here::here("data", "models", 
#                                                       "model3.2_municipality.RData"))

## 3.3. N binomial with glmmTMB, nbiom2, SSBID ---------------------------------

# Model 3.3
# model3.3_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|SSBID),
#                                          family = nbinom2, 
#                                          data = occ_subset)

# Save output to file
# save(model3.3_SSB, file = here::here("data", "models", "model3.3_SSB.RData"))

## 3.4. N binomial with glmmTMB, nbiom2, Municipality --------------------------

# Model 3.4
# model3.4_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period + (1|municipality),
#                                          family = nbinom2, 
#                                          data = occ_subset)

# Save output to file
# save(model3.4_municipality, file = here::here("data", "models", 
#                                                       "model3.4_municipality.RData"))

# 4. MODEL 2: OCC ~ COVER CHANGE + OFFSET --------------------------------------

## 4.1. N binomial glmmTMB, nbinom 2, SSBID on data subset ---------------------

# Run model
model3.5_SSB <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(log(ocurrences_before + 0.0001)) + (1 | SSBID),
                    family = nbinom2,
                    data = occ_subset)

# Save model output to file to save time next time
save(model3.5_SSB, file = here::here("data", "models", "model3.5_SSB.RData"))

## 4.2. N binomial glmmTMB, nbinom 2, Municipality on data subset ---------------------

# Run model
model3.6_municipality <- glmmTMB(ocurrences_after ~ cover_change * time_period * ocurrences_before + offset(log(ocurrences_before + 0.0001)) + (1 | municipality),
                        family = nbinom2,
                        data = occ_subset)

# Save model output to file to save time next time
save(model3.6_municipality, file = here::here("data", "models", 
                                              "model3.6_municipality.RData"))


# END OF SCRIPT ----------------------------------------------------------------