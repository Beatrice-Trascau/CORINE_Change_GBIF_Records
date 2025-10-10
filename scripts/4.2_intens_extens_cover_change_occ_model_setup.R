##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.2_intens_extens_cover_change_occ_model_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (intensification/extensification) on the number of 
# occurrences in a pixel
##----------------------------------------------------------------------------##

# 1. LOAD AND PREPARE DATA -----------------------------------------------------

# Load data
load(here("data","derived_data", 
          "modeling_data_combined_corine_gbif_ssb_august2025.rda"))


# Convert data from long to wide format for modeling
modeling_data_wide <- modeling_data_filtered |>
  # select the variables we need for modeling
  select(cell_ID, SSBID, land_cover_start, land_cover_end, land_cover_start_name, 
         land_cover_end_name, cover_change, transition_type, intens_extens,
         time_period, n_occurrences, n_species, analysis_period,
         # include the taxonomic/metadata lists if you want them in the final model data
         species_list, kingdom_list, phylum_list, class_list, order_list, 
         family_list, publisher_list, datasetName_list) |>
  
  # reshape from long to wide format
  pivot_wider(id_cols = c(cell_ID, SSBID, land_cover_start, land_cover_end, 
                land_cover_start_name, land_cover_end_name, cover_change, 
                transition_type, intens_extens, analysis_period),
              names_from = time_period,
              values_from = c(n_occurrences, n_species, species_list, kingdom_list, 
                    phylum_list, class_list, order_list, family_list, 
                    publisher_list, datasetName_list),
              names_sep = "_") |>
  
  # create the before/after columns needed for your model
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
  
  # remove rows with missing before/after data (shouldn't happen but just in case)
  filter(!is.na(occurrences_before) & !is.na(occurrences_after)) |>
  
  # convert factors back for modeling
  mutate(cell_ID = as.factor(cell_ID),
         SSBID = as.factor(SSBID),
         cover_change = as.factor(cover_change),
         intens_extens = as.factor(intens_extens),
         time_period = as.factor(time_period))

# 2. MODEL 1: OCC ~ COVER CHANGE + OFFSET --------------------------------------

# Relevel cover_change to have 'No_change' as the reference
modeling_data_wide$intens_extens <- relevel(modeling_data_wide$intens_extens,
                                            ref = "No_change")

## 2.1. N binomial glmmTMB, nbinom 2, SSBID + Interaction ----------------------

# IntensExtens_model1 <- glmmTMB(occurrences_after ~ intens_extens * time_period +
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          zi = ~intens_extens + time_period,
#                          family = nbinom2,
#                          data = modeling_data_wide)


# Save model output to file to save time next time
# save(IntensExtens_model1, file = here::here("data", "models",
#                                             "IntensExtens_model1_zero_inflated_interaction.RData"))


## 2.2. Zero inflated no interaction -------------------------------------------

# Run model
# IntensExtens_model2 <- glmmTMB(occurrences_after ~ intens_extens + time_period +
#                             offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                           zi = ~intens_extens + time_period,
#                           family = nbinom2,
#                           data = modeling_data_wide)

# Save model output to file 
# save(IntensExtens_model2, file = here::here("data", "models",
#                                        "IntensExtens_model2_zero_inflated_no_interaction.RData"))

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
# Check model fit
Intens_Extens_simulationOutput3 <- simulateResiduals(fittedModel = IntensExtens_model3)

# Set up file output
png(here("figures", "FigureS8_IntensExtens_model3_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(Intens_Extens_simulationOutput3, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(Intens_Extens_simulationOutput3, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

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

# Compare AIC between Model 3 and Model 4
AICtab(IntensExtens_model3, IntensExtens_model4, base = TRUE) 
# Model3 preffered dAIC = 154
# AIC     dAIC    df
# IntensExtens_model3 5649691       0 21
# IntensExtens_model4 5649845     154 17

## 2.5. Logged occurrences after + interaction ---------------------------------

# Log transform the occurrences after
# modeling_data_wide$log_occurrences_after <- log(modeling_data_wide$occurrences_after + 1)

# Run model
# IntensExtens_model5 <- glmmTMB(log_occurrences_after ~ intens_extens * time_period + 
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          family = gaussian,
#                          data = modeling_data_wide)

# Save model output
# save(IntensExtens_model5, file = here::here("data", "models",
#                                       "IntensExtens_model5_logged_interaction.RData"))

## 2.6. Logged occurrences after no interaction --------------------------------

# Run model
# IntensExtens_model6 <- glmmTMB(log_occurrences_after ~ intens_extens + time_period + 
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          family = gaussian,
#                          data = modeling_data_wide)

# Save model output
# save(IntensExtens_model6, file = here::here("data", "models",
#                                       "IntensExtens_model6_logged_nointeraction.RData"))

# 3. SUMMARY STATISTICS --------------------------------------------------------

## 3.1. Summary statistics table -----------------------------------------------

# Calculate summary statistics for all periods combined
all_periods_stats <- modeling_data_wide |>
  group_by(intens_extens) |>
  summarise(Count = n(),
            Zeros = sum(occurrences_after == 0),
            `% Zeros` = round((sum(occurrences_after == 0) / n()) * 100, 1),
            `Total Occurrences` = sum(occurrences_before + occurrences_after),
            Median = median(occurrences_after),
            `75th Percentile` = quantile(occurrences_after, 0.75),
            `90th Percentile` = quantile(occurrences_after, 0.90),
            Maximum = max(occurrences_after),
            .groups = "drop") |>
  mutate(`Time Period` = "All Periods") |>
  select(`Time Period`, `Intens/Extens` = intens_extens, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2000-2006 period
period_2000_2006_stats <- modeling_data_wide |>
  filter(time_period == "2000_2006") |>
  group_by(intens_extens) |>
  summarise(Count = n(),
            Zeros = sum(occurrences_after == 0),
            `% Zeros` = round((sum(occurrences_after == 0) / n()) * 100, 1),
            `Total Occurrences` = sum(occurrences_before + occurrences_after),
            Median = median(occurrences_after),
            `75th Percentile` = quantile(occurrences_after, 0.75),
            `90th Percentile` = quantile(occurrences_after, 0.90),
            Maximum = max(occurrences_after),
            .groups = "drop") |>
  mutate(`Time Period` = "2000-2006") |>
  select(`Time Period`, `Intens/Extens` = intens_extens, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2006-2012 period
period_2006_2012_stats <- modeling_data_wide |>
  filter(time_period == "2006_2012") |>
  group_by(intens_extens) |>
  summarise(Count = n(),
            Zeros = sum(occurrences_after == 0),
            `% Zeros` = round((sum(occurrences_after == 0) / n()) * 100, 1),
            `Total Occurrences` = sum(occurrences_before + occurrences_after),
            Median = median(occurrences_after),
            `75th Percentile` = quantile(occurrences_after, 0.75),
            `90th Percentile` = quantile(occurrences_after, 0.90),
            Maximum = max(occurrences_after),
            .groups = "drop") |>
  mutate(`Time Period` = "2006-2012") |>
  select(`Time Period`, `Intens/Extens` = intens_extens, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2012-2018 period
period_2012_2018_stats <- modeling_data_wide |>
  filter(time_period == "2012_2018") |>
  group_by(intens_extens) |>
  summarise(Count = n(),
            Zeros = sum(occurrences_after == 0),
            `% Zeros` = round((sum(occurrences_after == 0) / n()) * 100, 1),
            `Total Occurrences` = sum(occurrences_after),
            Median = median(occurrences_after),
            `75th Percentile` = quantile(occurrences_after, 0.75),
            `90th Percentile` = quantile(occurrences_after, 0.90),
            Maximum = max(occurrences_after),
            .groups = "drop") |>
  mutate(`Time Period` = "2012-2018") |>
  select(`Time Period`, `Intens/Extens` = intens_extens, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Combine all statistics into one table
summary_table_complete_intens_extens <- bind_rows(all_periods_stats, period_2000_2006_stats,
                                                  period_2006_2012_stats, period_2012_2018_stats) |>
  arrange(factor(`Time Period`, levels = c("All Periods", "2000_2006", "2006_2012", "2012_2018")),
          `Intens/Extens`)

# Print the table
print(summary_table_complete_intens_extens)

## 3.2. Summary statistics for text --------------------------------------------

# Calculate the exact number of occurrences:
exact_totals <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after) |>
  group_by(intens_extens) |>
  summarise(total_pixels = n(),
            total_occurrences = sum(total_occurrences, na.rm = TRUE),
            .groups = "drop")

# Display results
cat("=== EXACT TOTALS ===\n")
exact_extens <- exact_totals |> filter(intens_extens == "Extensification")
exact_intens <- exact_totals |> filter(intens_extens == "Intensification")
exact_no_change <- exact_totals |> filter(intens_extens == "No_change")

cat("EXACT total occurrences for pixels with EXTENSIFICATION:", format(exact_extens$total_occurrences, big.mark = ","), "\n")
cat("EXACT total occurrences for pixels with INTENSIFICATION:", format(exact_intens$total_occurrences, big.mark = ","), "\n")
cat("EXACT total occurrences for pixels with NO CHANGE:", format(exact_no_change$total_occurrences, big.mark = ","), "\n")

# 4. VIOLIN PLOTS --------------------------------------------------------------

# First, create a combined dataset with total occurrences
modeling_data_for_violin <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after) |>
  filter(!is.na(intens_extens))

# Create violin plot for occurrences_after (log-transformed)
q1 <- ggplot(modeling_data_for_violin, 
             aes(x = time_period, y = total_occurrences + 0.1, 
                 fill = intens_extens)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  # add points with transparency and jitter
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2,
                                              dodge.width = 0.7),
              alpha = 0.1, size = 0.5) + 
  scale_y_log10() +
  scale_fill_manual(values = c("No_change" = "#66c2a5", 
                               "Intensification" = "sienna", 
                               "Extensification" = "#8da0cb"),
                    labels = c("No change", "Intensification", "Extensification")) +
  scale_x_discrete(labels = c("2000_2006" = "2000-2006", 
                              "2006_2012" = "2006-2012", 
                              "2012_2018" = "2012-2018")) +
  labs(x = "Time Period",
       y = "Total Number of Occurrences (log scale)",
       fill = "Land Use Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))


# Violin plot with original data, zoomed to see detail
q2 <- ggplot(modeling_data_for_violin, 
             aes(x = time_period, y = total_occurrences, 
                 fill = intens_extens)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("No_change" = "#66c2a5", 
                               "Intensification" = "sienna", 
                               "Extensification" = "#8da0cb"),
                    labels = c("No change", "Intensification", "Extensification")) +
  scale_x_discrete(labels = c("2000_2006" = "2000-2006", 
                              "2006_2012" = "2006-2012", 
                              "2012_2018" = "2012-2018")) +
  labs(x = "Time Period",
       y = "Total Number of Occurrences",
       fill = "Land Use Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")

# Combine plots using cowplot
combined_plot_intens_extens <- plot_grid(q2, q1, labels = c('A)', 'B)'), ncol = 2)

# Save combined figure
ggsave(here("figures", "FigureS7_occs_in_intens_extens_change.png"),
       plot = combined_plot_intens_extens, width = 16, height = 8, dpi = 300)

# 5. MODEL SUMMARY REPORTING FOR MANUSCRIPT ------------------------------------

# Extract the coefficient for cover change
cover_change_coef <- -1.25606  # Get it from the table

# Calculate the Incidence Rate Ratio (IRR)
irr <- exp(cover_change_coef)
cat("Incidence Rate Ratio (IRR):", round(irr, 3), "\n")

# Calculate percentage change
percent_change <- (irr - 1) * 100
cat("Percentage change:", round(percent_change, 1), "%\n")

# Calculate confidence intervals using standard error from the model summary
se <- 0.01004
ci_lower <- exp(cover_change_coef - 1.96 * se)
ci_upper <- exp(cover_change_coef + 1.96 * se)
cat("95% CI for IRR:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n")

# END OF SCRIPT ----------------------------------------------------------------