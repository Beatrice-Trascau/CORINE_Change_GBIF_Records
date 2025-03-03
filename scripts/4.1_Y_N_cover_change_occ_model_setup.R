##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.1_Y_N_cover_change_occ_model_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (Y/N) on the number of occurrences in a pixel
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

# Prepare data for the period 1997-2000
occ_df_before_2000.2006 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2000, land_cover2006, 
         SSBID, cell_ID, NAME_2) |>
  filter(!is.na(land_cover2000) & !is.na(land_cover2006)) |>
  filter(year >= 1997 & year <= 2000) |>
  mutate(cover_change = if_else(land_cover2000 == land_cover2006, "N", "Y"))

# Calculate number of records for the period 1997-2000
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
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

### 2.3.2. After land cover change ---------------------------------------------

# Prepare data for the period 2015-2018
occ_df_after_2012.2018 <- occ_SSB_land_cover |>
  select(gbifID, year, species, land_cover2012, land_cover2018, 
         SSBID, cell_ID, NAME_2) |>
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
    NAME_2 = first(NAME_2),
    gbifID = first(gbifID)) |>
  mutate(time_period = "2012-2018") |>
  select(-c(land_cover2012, land_cover2018))

## 2.4. Combine dataframes  ----------------------------------------------------

# Before land cover change
occ_df_before_records <- bind_rows(occ_df_before_2000.2006_records,
                                   occ_df_before_2006.2012_records,
                                   occ_df_before_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)


# After land cover change
occ_y_n_cover_change_after_records_for_model <- bind_rows(occ_df_after_2000.2006_records,
                                                          occ_df_after_2006.2012_records,
                                                          occ_df_after_2012.2018_records) |>
  select(-cell_ID) |>
  rename(municipality = NAME_2)

# Ensure unique combinations of SSBID time_period and cover_change before
#   the full join to prevent duplicates in the fully joined df

# Aggregate before data to ensure uniqueness
occ_df_before_records_unique <- occ_df_before_records |>
  group_by(SSBID, time_period, cover_change, municipality) |>
  summarise(ocurrences_before = sum(ocurrences_before)) |>
  ungroup()

# Aggregate after data to ensure uniqueness
occ_y_n_cover_change_after_records_unique <- occ_y_n_cover_change_after_records_for_model |>
  group_by(SSBID, time_period, cover_change, municipality) |>
  summarise(ocurrences_after = sum(ocurrences_after)) |>
  ungroup()

# Full join of the dfs on SSBid and time_period
occ_df_before_after <- full_join(occ_y_n_cover_change_after_records_for_model,
                                 occ_df_before_records,
                                 by = c("SSBID", "time_period", 
                                        "cover_change", "municipality"))

# Replace NA with 0 for occurrences_before and occurrences_after
occ_y_n_cover_change_before_after_for_modell <- occ_df_before_after |>
  mutate(ocurrences_after = ifelse(is.na(ocurrences_after), 0, ocurrences_after),
         ocurrences_before = ifelse(is.na(ocurrences_before), 0, ocurrences_before),
         SSBID = as.factor(SSBID))

# Check for duplicates - this should return 0 rows if there are no duplicates
duplicate_check <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(SSBID, time_period, cover_change, municipality) |>
  summarise(count = n()) |>
  filter(count > 1)

# Write dataframe to file
save(occ_y_n_cover_change_before_after_for_modell,
     file = here::here("data", "derived_data",
                       "occ_y_n_cover_change_before_after_for_modell.rda"))

# 3. MODEL 1: OCC ~ COVER CHANGE + OFFSET --------------------------------------

## 3.1. N binomial glmmTMB, nbinom2, SSBID -------------------------------------

# Run negative binomial model
YN_model1_SSB_interaction <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                     family = nbinom2,
                                     data = occ_y_n_cover_change_before_after_for_modell)

# Save model output to file to save time next time
save(YN_model1_SSB_interaction, file = here::here("data", "models", 
                                                  "YN_model1_SSB_interaction.RData"))

## 3.2. N binomial glmmTMB, nbinom2, SSBID, no interactio  ---------------------

# Run negative binomial model
YN_model2_SSB_no_interaction <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.001)) + (1 | SSBID),
                                        family = nbinom2,
                                        data = occ_y_n_cover_change_before_after_for_modell)

# Save model output to file to save time next time
save(YN_model2_SSB_no_interaction, file = here::here("data", "models", 
                                                     "YN_model2_SSB_no_interaction.RData"))

## 3.3. Compare models ---------------------------------------------------------

# Get AIC table
AICctab(YN_model1_SSB_interaction, YN_model2_SSB_no_interaction, base = TRUE)
#deltaAIC = 

# 4. CHECK MARGINAL VALUE IMPACT -----------------------------------------------

## 4.1 Model with 0.1 offset ---------------------------------------------------

# Run negative binomial model
YN_model3_SSB_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                family = nbinom2,
                                                data = occ_y_n_cover_change_before_after_for_modell)

# Save model output to file to save time next time
save(YN_model3_SSB_interaction_0.1_offset, file = here::here("data", "models", 
                                                             "YN_model3_SSB_interaction_0.1_offset.RData"))

## 4.2 Model with 0.01 offset --------------------------------------------------

# Run negative binomial model
YN_model4_SSB_interaction_0.01_offset <- glmmTMB(ocurrences_after ~ cover_change * time_period + offset(log(ocurrences_before + 0.01)) + (1 | SSBID),
                                                 family = nbinom2,
                                                 data = occ_y_n_cover_change_before_after_for_modell)

# Save model output to file to save time next time
save(YN_model4_SSB_interaction_0.01_offset, file = here::here("data", "models", 
                                                              "YN_model4_SSB_interaction_0.01_offset.RData"))

## 4.3. Model with 0.1 offset and no interaction -------------------------------

# Run negative binomial model
YN_model5_SSB_no_interaction_0.1_offset <- glmmTMB(ocurrences_after ~ cover_change + time_period + offset(log(ocurrences_before + 0.1)) + (1 | SSBID),
                                                   family = nbinom2,
                                                   data = occ_y_n_cover_change_before_after_for_modell)

# Save model output to file to save time next time
save(YN_model5_SSB_no_interaction_0.1_offset, file = here::here("data", "models",
                                                                "YN_model5_SSB_no_interaction_0.1_offset.RData"))

# 5. EXPLORATORY FIGURES OF DF USED IN MODELS ----------------------------------

## 5.1. Violin plot with log-transformed values --------------------------------

p1 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  # Add points with transparency (alpha) and slight horizontal jitter
  geom_jitter(position = position_jitterdodge(
    jitter.width = 0.2,  # Fix amount of horizontal jitter
    dodge.width = 0.7),  # Match dodge with violins
    alpha = 0.2,         # Fix transparency
    size = 1) +          # Fix point size
  scale_y_log10() +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "#fc8d62")) +
  labs(x = "Time Period",
       y = "Number of Occurrences (log)",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Save figure
ggsave(here("figures", "occurrences_in_Y_N_cover_change_FigureS1.png"),
       width=17, height=13)


## 5.2. Violin plot with original values and log-transformed values ------------

# Violin plot with the original data
p2 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "#fc8d62")) +
  coord_cartesian(ylim = c(0, 100)) +  # Set the y-axis limit to 100
  labs(x = "Time Period",
       y = "Number of Occurrences",
       title = "Zoomed to values < 100") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Combine the two plots
plot_grid(p1, p2, labels = c('a)', 'b)'))

# Save figure
ggsave(here("figures", "occurrences_in_Y_N_cover_change_FigureS2.png"),
       width=17, height=13)

# END OF SCRIPT ----------------------------------------------------------------