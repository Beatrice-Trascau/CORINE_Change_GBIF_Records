##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 4.1_Y_N_cover_change_occ_model_setup
# This script contains code which runs the simple models looking at the effect 
# of land cover change (Y/N) on the number of occurrences in a pixel
##----------------------------------------------------------------------------##

# 1. LOAD AND PREPARE DATA FOR ANALYSIS ----------------------------------------

# Load data
load(here("data","derived_data", 
          "modeling_data_combined_corine_gbif_ssb_august2025.rda"))


# Convert data from long to wide format for modeling
modeling_data_wide <- modeling_data_filtered |>
  # select the variables we need for modeling
  select(cell_ID, SSBID, x, y,  # <- ADD x and y here
         land_cover_start, land_cover_end, land_cover_start_name, 
         land_cover_end_name, cover_change, transition_type, intens_extens,
         time_period, n_occurrences, n_species, analysis_period,
         species_list, kingdom_list, phylum_list, class_list, order_list, 
         family_list, publisher_list, datasetName_list) |>
  # reshape from long to wide format
  pivot_wider(id_cols = c(cell_ID, SSBID, x, y, land_cover_start, land_cover_end,
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
         time_period = as.factor(time_period),
         x_coord = x,
         y_coord = y)

# 2. RUN ZERO-INFLATED MODELS --------------------------------------------------

## 2.1. Zero inflated interaction ----------------------------------------------

# Run model
# YN_ZINB_model <- glmmTMB(occurrences_after ~ cover_change * time_period +
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          zi = ~cover_change + time_period,
#                          family = nbinom2,
#                          data = modeling_data_wide)

# Save model output to file
# save(YN_ZINB_model, file = here::here("data", "models",
#                                       "YN_model1_zero_inflated_interaction_0.1_offset.RData"))

# Check model summary
# summary(YN_ZINB_model)

# Check model fit
# simulationOutput <- simulateResiduals(fittedModel = YN_ZINB_model)

# Plot residual diagnostics
# plot(simulationOutput)

# Use bootstrap for outlier testing
# testOutliers(simulationOutput, type = "bootstrap")

# Other useful DHARMa tests for your models:
# testDispersion(simulationOutput)
# testZeroInflation(simulationOutput)
# testTemporalAutocorrelation(simulationOutput, time = modeling_data_wide$time_period)

# Extract residuals
# residuals1 <- residuals(YN_ZINB_model, type = "pearson")

# Create both plots side by side
# par(mfrow = c(1, 2))

# QQ plot
# qqnorm(residuals1, main = "Normal Q-Q Plot")
# qqline(residuals1, col = "red")

# Residuals vs Fitted
# plot(fitted(YN_ZINB_model), residuals1, 
#      xlab = "Fitted", ylab = "Residuals",
#      main = "Residuals vs Fitted", pch = 16, cex = 0.6)
# abline(h = 0, col = "red", lty = 2)
# 
# par(mfrow = c(1, 1))  # reset layout

## 2.2. Zero inflated no interaction -------------------------------------------

# Run model
# YN_ZINB_model2 <- glmmTMB(occurrences_after ~ cover_change + time_period + 
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          zi = ~cover_change + time_period, 
#                          family = nbinom2,
#                          data = modeling_data_wide)

# Save model output to file 
# save(YN_ZINB_model2, file = here::here("data", "models",
#                                       "YN_model2_zero_inflated_interaction.RData"))

# Check model fit
# simulationOutput2 <- simulateResiduals(fittedModel = YN_ZINB_model2)

# Plot residual diagnostics
# plot(simulationOutput2)

## 2.3. Zero inflated - more complex structure ---------------------------------

# Run model
YN_ZINB_model3 <- glmmTMB(occurrences_after ~ cover_change * time_period +
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          zi = ~cover_change * time_period + (1 | SSBID),
                          family = nbinom2,
                          data = modeling_data_wide)

# Save model output to file 
save(YN_ZINB_model3, file = here::here("data", "models",
                                       "YN_model3_zero_inflated_interaction.RData"))

# Check model fit
simulationOutput3 <- simulateResiduals(fittedModel = YN_ZINB_model3)

# Set up file output
png(here("figures", "FigureS6_YN_model3_DHARMA_validation.png"),
    width = 12, height = 6, units = "in", res = 300)

# Set up side-by-side layout
par(mfrow = c(1, 2))

# Create the plots
plotQQunif(simulationOutput3, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
plotResiduals(simulationOutput3, quantreg = FALSE)

# Close the file
dev.off()

# Reset layout
par(mfrow = c(1, 1))

## 2.4. Zero inflated - complex structure + interaction ------------------------

# Run model
YN_ZINB_model4 <- glmmTMB(occurrences_after ~ cover_change + time_period +
                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
                          zi = ~cover_change * time_period + (1 | SSBID),
                          family = nbinom2,
                          data = modeling_data_wide)

# Save model output to file 
save(YN_ZINB_model4, file = here::here("data", "models",
                                       "YN_model4_zero_inflated_nointeraction.RData"))

# Compare AIC between Model 3 and Model 4
AICtab(YN_ZINB_model3, YN_ZINB_model4, base = TRUE)
# Model3 preffered dAIC = 154
#                 AIC       dAIC      df
# YN_ZINB_model3 5649829.7       0.0 15
# YN_ZINB_model4 5649879.8      50.1 13


# Check model fit
# simulationOutput4 <- simulateResiduals(fittedModel = YN_ZINB_model4)

# Plot residual diagnostics
# plot(simulationOutput4)

## 2.5. Logged occurrences after + interaction ---------------------------------

# Log transform the occurrences after
# modeling_data_wide$log_occurrences_after <- log(modeling_data_wide$occurrences_after + 1)

# Run model
# YN_log_model5 <- glmmTMB(log_occurrences_after ~ cover_change * time_period + 
#                           offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                         family = gaussian,
#                         data = modeling_data_wide)

# Save model output
# save(YN_log_model5, file = here::here("data", "models",
#                                        "YN_model5_logged_interaction.RData"))

# Check model fit
# simulationOutput5 <- simulateResiduals(fittedModel = YN_log_model5)

# Plot residual diagnostics
# plot(simulationOutput5)

# Extract residuals
# residuals <- residuals(YN_log_model5, type = "pearson")

# Create both plots side by side
# par(mfrow = c(1, 2))

# QQ plot
# qqnorm(residuals, main = "Normal Q-Q Plot")
# qqline(residuals, col = "red")

# Residuals vs Fitted
# plot(fitted(YN_log_model5), residuals, 
#      xlab = "Fitted", ylab = "Residuals",
#      main = "Residuals vs Fitted", pch = 16, cex = 0.6)
# abline(h = 0, col = "red", lty = 2)
# 
# par(mfrow = c(1, 1))  # reset layout

## 2.6. Logges occurrences after no interaction --------------------------------

# Run model
# YN_log_model6 <- glmmTMB(log_occurrences_after ~ cover_change + time_period + 
#                            offset(log(occurrences_before + 0.1)) + (1 | SSBID),
#                          family = gaussian,
#                          data = modeling_data_wide)

# Save model output
# save(YN_log_model6, file = here::here("data", "models",
#                                       "YN_model6_logged_nointeraction.RData"))

# Check model fit
# simulationOutput6 <- simulateResiduals(fittedModel = YN_log_model6)

# Plot residual diagnostics
# plot(simulationOutput6)

# 3. GAM -----------------------------------------------------------------------

## 3.1. Simple spatial smoother ------------------------------------------------

# Run model
# YN_gam_model7 <- gam(occurrences_after ~ cover_change * time_period +
#                        offset(log(occurrences_before + 0.1)) +
#                        s(x_coord, y_coord, bs = "tp", k = 200),
#                      family = nb(), method = "REML", 
#                      data = modeling_data_wide)

# Save model output
# save(YN_gam_model7, file = here::here("data", "models",
#                                       "YN_model7_gam_interaction.RData"))

# Check residuals with DHARMa
# sim_gam <- simulateResiduals(YN_gam_model7, n = 1000)
# plot(sim_gam)
# gam.check(YN_gam_model7)

## 3.2. Spatial smoother includes time -----------------------------------------

# Run model
# YN_gam_model8 <- gam(occurrences_after ~ cover_change * time_period +
#                        offset(log(occurrences_before + 0.1)) + 
#                        s(x_coord, y_coord, by = time_period, bs = "tp", k = 150),
#                      family = nb(), method = "REML", data = modeling_data_wide)

# Save model outpu
# save(YN_gam_model8, file = here::here("data", "models",
#                                       "YN_model8_gam_interaction.RData"))

# Check residuals with DHARMa
# sim_gam2 <- simulateResiduals(YN_gam_model8, n = 1000)
# plot(sim_gam2)
# gam.check(YN_gam_model8)

# 4. SUMMARY STATISTICS --------------------------------------------------------

## 4.1. Summary statistics table -----------------------------------------------

# Calculate summary statistics for all periods combined
all_periods_stats <- modeling_data_wide |>
  group_by(cover_change) |>
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
  select(`Time Period`, `Cover Change` = cover_change, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2000-2006 period
period_2000_2006_stats <- modeling_data_wide |>
  filter(time_period == "2000_2006") |>
  group_by(cover_change) |>
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
  select(`Time Period`, `Cover Change` = cover_change, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2006-2012 period
period_2006_2012_stats <- modeling_data_wide |>
  filter(time_period == "2006_2012") |>
  group_by(cover_change) |>
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
  select(`Time Period`, `Cover Change` = cover_change, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Calculate summary statistics for 2012-2018 period
period_2012_2018_stats <- modeling_data_wide |>
  filter(time_period == "2012_2018") |>
  group_by(cover_change) |>
  summarise(Count = n(),
            Zeros = sum(occurrences_after == 0),
            `% Zeros` = round((sum(occurrences_after == 0) / n()) * 100, 1),
            `Total Occurrences` = sum(occurrences_before + occurrences_after),
            Median = median(occurrences_after),
            `75th Percentile` = quantile(occurrences_after, 0.75),
            `90th Percentile` = quantile(occurrences_after, 0.90),
            Maximum = max(occurrences_after),
            .groups = "drop") |>
  mutate(`Time Period` = "2012-2018") |>
  select(`Time Period`, `Cover Change` = cover_change, Count, Zeros, `% Zeros`, 
         `Total Occurrences`, Median, `75th Percentile`, `90th Percentile`, Maximum)

# Combine all statistics into one table
summary_table_complete <- bind_rows(all_periods_stats, period_2000_2006_stats,
                                    period_2006_2012_stats, period_2012_2018_stats) |>
  arrange(factor(`Time Period`, levels = c("All Periods", "2000_2006", "2006_2012", "2012_2018")),
          `Cover Change`)

# Print the table
print(summary_table_complete)

## 4.2. Summary statistics for text --------------------------------------------

# Calculate the exact number of occurrences:
exact_totals <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after) |>
  group_by(cover_change) |>
  summarise(total_pixels = n(),
            total_occurrences = sum(total_occurrences, na.rm = TRUE),
            .groups = "drop")

# Display results
cat("=== EXACT TOTALS ===\n")
exact_change <- exact_totals |> filter(cover_change == "Y")
exact_no_change <- exact_totals |> filter(cover_change == "N")

cat("EXACT total occurrences for pixels WITH cover change:", format(exact_change$total_occurrences, big.mark = ","), "\n")
cat("EXACT total occurrences for pixels WITHOUT cover change:", format(exact_no_change$total_occurrences, big.mark = ","), "\n")


# 5. VIOLIN PLOT FIGURES --------------------------------------------------------

# First, create a combined dataset with total occurrences
modeling_data_for_violin <- modeling_data_wide |>
  mutate(total_occurrences = occurrences_before + occurrences_after)

# Create violin plot for occurrences_after (log-transformed)
p1 <- ggplot(modeling_data_for_violin, 
             aes(x = time_period, y = total_occurrences + 0.1, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  # add points with transparency and jitter
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2,
                                              dodge.width = 0.7),
              alpha = 0.1, size = 0.5) + 
  scale_y_log10() +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  scale_x_discrete(labels = c("2000_2006" = "2000-2006", 
                              "2006_2012" = "2006-2012", 
                              "2012_2018" = "2012-2018")) +
  labs(x = "Time Period",
       y = "Total Number of Occurrences (log scale)",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

# Violin plot with original data, zoomed to see detail
p2 <- ggplot(modeling_data_for_violin, 
             aes(x = time_period, y = total_occurrences, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  scale_x_discrete(labels = c("2000_2006" = "2000-2006", 
                              "2006_2012" = "2006-2012", 
                              "2012_2018" = "2012-2018")) +
  labs(x = "Time Period",
       y = "Total Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

# Combine plots using cowplot
combined_plot <- plot_grid(p2, p1, labels = c('A)', 'B)'), ncol = 2)

# Save combined figure
ggsave(here("figures", "FigureS5_occs_in_YN_cover_change.png"),
       plot = combined_plot, width = 16, height = 8, dpi = 300)

# 6. MODEL SUMMARY REPORTING FOR MANUSCRIPT ------------------------------------

# Extract the coefficient for cover change
cover_change_coef <- -20.07639  # Get it from the table

# Calculate the Incidence Rate Ratio (IRR)
irr <- exp(cover_change_coef)
cat("Incidence Rate Ratio (IRR):", round(irr, 3), "\n")

# Calculate percentage change
percent_change <- (irr - 1) * 100
cat("Percentage change:", round(percent_change, 1), "%\n")

# Calculate confidence intervals using standard error from the model summary
se <- 56.71044
ci_lower <- exp(cover_change_coef - 1.96 * se)
ci_upper <- exp(cover_change_coef + 1.96 * se)
cat("95% CI for IRR:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n")

# END OF SCRIPT ----------------------------------------------------------------