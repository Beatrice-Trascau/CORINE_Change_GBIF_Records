##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 5.1_cover_change_combine_model_outputs_in_figure4
# This script contains code which combines the outputs of the individual land
# cover models into a single figure for the paper (Figure 4)
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------
load(here("data", "models", "urban_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "complex_agri_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "agri_veg_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "forests_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "moors_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "woodland_model3_SSB_interaction_0.1_offset.RData"))
load(here("data", "models", "sparse_model3_SSB_interaction_0.1_offset.RData"))

# 2. EXTRACT MODEL OUTPUTS -----------------------------------------------------

# Create list of models
model_list <- list(urban_model3_SSB_interaction_0.1_offset, 
                   complex_agri_model3_SSB_interaction_0.1_offset, 
                   agri_veg_model3_SSB_interaction_0.1_offset,
                   forests_model3_SSB_interaction_0.1_offset,
                   moors_model3_SSB_interaction_0.1_offset, 
                   woodland_model3_SSB_interaction_0.1_offset,
                   sparse_model3_SSB_interaction_0.1_offset)

# Apply function to extract model summary to df
models_results <-lapply(model_list, extract_summary_as_df)

# Combine all extracted results in a single df
combined_results <- bind_rows(models_results, .id = "model_id")

# 3. SHAPE DF FOR PLOTTING -----------------------------------------------------

# Add new column with the initial land cover (from model id)
combined_results1 <- combined_results |>
  mutate(intial_cover = case_when(model_id == 1 ~ "Urban Fabric",
                                  model_id == 2 ~ "Complex Agriculture",
                                  model_id == 3 ~ "Agriculture & Vegetation",
                                  model_id == 4 ~ "Forests",
                                  model_id == 5 ~ "Moors, Heathland & Grassland",
                                  model_id == 6 ~ "Transitional Woodland Shrub",
                                  model_id == 7 ~ "Sparse Vegetation"))

# Add new column with the cover change
combined_results2 <- combined_results1 |>
  mutate(term = str_replace_all(term, "complex_agri", "complex.agri"),
         term = str_replace_all(term, "agri_sig_veg", "agri.sig.veg"),
         term = str_replace_all(term, "moors_heath_grass", "moors.heath.grass"),
         term = str_replace_all(term, "woodland_shrub", "woodland.shrub"),
         term = str_replace_all(term, "sparse_veg", "sparse.veg"),
         term = str_replace_all(term, "cover_change", "cover;change"),
         term = str_replace_all(term, "time_period", "time.period"))

# Separate the term column into initial land_cover and cover_change
combined_results3 <- combined_results2 |>
  separate(term, into = c("term", "cover_change"), sep = "_", remove = FALSE)

# Separate cover_change column into cover_change and time_period
combined_results4 <- combined_results3 |>
  separate(cover_change, into = c("cover_change", "time_period"), sep = ":", remove = FALSE)

# Fix names in cover_change column
combined_results5 <- combined_results4 |>
  mutate(cover_change = case_when(cover_change == "complex.agri" ~ "Complex Agriculture",
                                  cover_change == "forests" ~ "Forests",
                                  cover_change == "agri.sig.veg" ~ "Agriculture & Vegetation",
                                  cover_change == "urban" ~ "Urban Fabric",
                                  cover_change == "woodland.shrub" ~ "Transitional Woodland Shrub",
                                  cover_change == "moors.heath.grass" ~ "Moors, Heathland & Grassland"))

# 4. PLOT EFFECTS WITHOUT INTERACTIONS WITH YEARS ------------------------------

# Subset df to not contain interaction rows
effect_no_year_interaction <- combined_results5 |>
  filter(is.na(time_period)) |>
  mutate(cover_change = ifelse(is.na(cover_change) & term == "time.period2006-2012",
                               "2006-2012", cover_change),
         cover_change = ifelse(is.na(cover_change) & term == "time.period2012-2018", 
                               "2012-2018", cover_change))

# Add significance
p_value_threshold <- 0.05

# Add new column to indicate significance
effect_no_year_interaction <- effect_no_year_interaction |>
  mutate(Significant = ifelse(`Pr(>|z|)` < p_value_threshold, Estimate, NA)) |>
  filter(!is.na(cover_change))

# Split df into time-related and cover-related effects
time_effect_no_year_interaction <- effect_no_year_interaction |>
  filter(cover_change %in% c("2006-2012", "2012-2018"))

# Cover-related effect
cover_effect_no_year_interaction <- effect_no_year_interaction |>
  filter(!cover_change %in% c("2006-2012", "2012-2018"))

# Manually add rows where intial_cover equals cover_change
missing_rows <- data.frame(model_id = NA, term = NA,
                           cover_change = c("Urban Fabric", 
                                            "Forests", "Complex Agriculture",
                                            "Transitional Woodland Shrub", 
                                            "Moors, Heathland & Grassland",
                                            "Sparse Vegetation", 
                                            "Agriculture & Vegetation"),
                           intial_cover = c("Urban Fabric", "Forests", 
                                            "Complex Agriculture",
                                            "Transitional Woodland Shrub", 
                                            "Moors, Heathland & Grassland",
                                            "Sparse Vegetation", 
                                            "Agriculture & Vegetation"),
                           Estimate = NA,
                           `Std. Error` = NA,
                           `z value` = NA,
                           `Pr(>|z|)` = NA,
                           Significant = NA,
                           time_period = NA)

# Add missing rows to cover df
cover_effect_no_year_interaction <- cover_effect_no_year_interaction |>
  bind_rows(missing_rows)

# Create new column to indicate when inital land cover = cover change
cover_effect_no_year_interaction <- cover_effect_no_year_interaction |>
  mutate(is_same = ifelse(intial_cover == cover_change, TRUE, FALSE))

# Reorder the levels of initial land cover column
cover_effect_no_year_interaction <- cover_effect_no_year_interaction |>
  mutate(intial_cover = factor(intial_cover, 
                               levels = c("Urban Fabric",
                                          "Transitional Woodland Shrub",
                                          "Sparse Vegetation",
                                          "Moors, Heathland & Grassland",
                                          "Forests",
                                          "Complex Agriculture",
                                          "Agriculture & Vegetation")),
         cover_change = factor(cover_change, 
                               levels = c("Agriculture & Vegetation", 
                                          "Complex Agriculture", 
                                          "Forests", 
                                          "Moors, Heathland & Grassland", 
                                          "Sparse Vegetation", 
                                          "Transitional Woodland Shrub", 
                                          "Urban Fabric")))

# Define breaks for the time legend
legend_breaks_cover <- c(min(cover_effect_no_year_interaction$Significant, na.rm = TRUE),
                         5, 2.5, 0, -2.5,
                         max(cover_effect_no_year_interaction$Significant, na.rm = TRUE))

# Plot heatmap for cover change effects
a <- ggplot(cover_effect_no_year_interaction, 
            aes(x = cover_change, y = intial_cover)) +
  geom_tile(aes(fill = ifelse(is_same, NA, Significant)), color = "black") +
  geom_tile(data = cover_effect_no_year_interaction %>% filter(is_same), 
            fill = "black", color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90", name = "Estimate",
                       breaks = legend_breaks_cover, 
                       labels = scales::label_number()) +
  scale_x_discrete(position = "top") +  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 0, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = "Cover Change", y = "Initial Cover")

# Create a label mapping
label_mapping <- c("Agriculture & Vegetation" = "ASNV",
                   "Complex Agriculture" = "CA",
                   "Forests" = "Forests",
                   "Moors, Heathland & Grassland" = "MHG",
                   "Sparse Vegetation" = "SV",
                   "Transitional Woodland Shrub" = "TWS",
                   "Urban Fabric" = "UF")

# Reorder the levels of initial land cover column
time_effect_no_year_interaction <- time_effect_no_year_interaction |>
  mutate(intial_cover = factor(label_mapping[as.character(intial_cover)]))

time_effect_no_year_interaction <- time_effect_no_year_interaction |>
  mutate(intial_cover = factor(intial_cover,
                               levels = c("UF", "TWS", "SV", "MHG",
                                          "Forests", "CA", "ASNV")))

# Define breaks for the time legend
legend_breaks_time <- c(min(time_effect_no_year_interaction$Significant, na.rm = TRUE), 
                        0, -0.4, -0.8,
                        max(time_effect_no_year_interaction$Significant, na.rm = TRUE))


# Add significance indicator to time effects
time_effect_no_year_interaction <- time_effect_no_year_interaction |>
  mutate(plot_type = ifelse(`Pr(>|z|)` < p_value_threshold, "significant", "non_significant"))

# Filter to only include significant time effects
time_sig <- time_effect_no_year_interaction |>
  filter(plot_type == "significant")

# Reverse mapping to get full names back
reverse_mapping <- setNames(names(label_mapping), label_mapping)

# Convert abbreviated labels back to full names for plotting
time_sig <- time_sig |>
  mutate(intial_cover_full = factor(reverse_mapping[as.character(intial_cover)],
                                    levels = c("Urban Fabric",
                                               "Transitional Woodland Shrub",
                                               "Sparse Vegetation",
                                               "Moors, Heathland & Grassland",
                                               "Forests",
                                               "Complex Agriculture",
                                               "Agriculture & Vegetation")))

# Plot time effects with only significant effects shown
time_updated <- ggplot(time_sig, 
                       aes(x = cover_change, y = intial_cover_full)) +
  geom_tile(aes(fill = Significant), color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#0072B2", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90", name = "Estimate", 
                       breaks = legend_breaks_time, 
                       labels = scales::label_number()) +
  scale_x_discrete(position = "top") +
  theme_classic() +
  theme(
    axis.text.x = element_text(hjust = 0, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = "Time Period", y = "Initial Cover")

# Combine the two plots
cover_time_combined_plot <- plot_grid(a, time,
                                      ncol = 1, labels = c("a)", "b)"))

# Save to file as .png
ggsave(here("figures", "model_outputs_Figure4.5.png"),
       width=20, height=13)

# 5. PLOT EFFECTS AS POINTS WITH ERROR BARS ------------------------------------

# Create a new order for the factors
new_order <- c("UF", "TWS", "SV", "MHG", "Forests", "CA", "ASNV")

# Use full names rather than abbreviations in the cover_points plot
cover_points_full <- cover_effect_no_year_interaction |>
  # Keep the original names
  mutate(plot_type = case_when(is_same ~ "same",
                               is.na(Significant) ~ "non_significant",
                               !is.na(Significant) ~ "significant")) |>
  rename(std_error = 'Std. Error') |>
  # Set reversed order (Agriculture & Vegetation at top) for facets
  mutate(intial_cover = factor(intial_cover, 
                               levels = c("Agriculture & Vegetation",
                                          "Complex Agriculture",
                                          "Forests",
                                          "Moors, Heathland & Grassland", 
                                          "Sparse Vegetation",
                                          "Transitional Woodland Shrub",
                                          "Urban Fabric")))

# Calculate y-axis values with only the significant results
significant_results_full <- cover_points_full |>
  filter(!is.na(Significant))

# Calculate ymin and ymax
y_min <- min(significant_results_full$Estimate - significant_results_full$std_error, 
             na.rm = TRUE)
y_max <- max(significant_results_full$Estimate + significant_results_full$std_error, 
             na.rm = TRUE)

# Add wrap width
wrap_width <- 15

# Define correct order for labels
correct_order <- c("Agriculture & Vegetation", 
                   "Complex Agriculture", 
                   "Forests", 
                   "Moors, Heathland & Grassland", 
                   "Sparse Vegetation", 
                   "Transitional Woodland Shrub", 
                   "Urban Fabric")

# Apply str_wrap to create wrapped labels with correct ordering
cover_points_full_wrapped <- cover_points_full |>
  # Ensure cover_change is in the correct order first
  mutate(cover_change = factor(as.character(cover_change), levels = correct_order),
    # Create wrapped versions of the category names using str_wrap
    intial_cover_wrapped = factor(
      stringr::str_wrap(as.character(intial_cover), width = wrap_width),
      levels = stringr::str_wrap(levels(intial_cover), width = wrap_width)),
    # Create wrapped versions with preserved ordering
    cover_change_wrapped = factor(
      stringr::str_wrap(as.character(cover_change), width = wrap_width),
      levels = stringr::str_wrap(correct_order, width = wrap_width)))

# Create a df with break points for y axis
y_breaks_df_wrapped <- cover_points_full_wrapped |>
  filter(plot_type == "significant") |>
  distinct(intial_cover_wrapped, cover_change_wrapped, .keep_all = TRUE) |>
  select(intial_cover_wrapped, cover_change_wrapped, intial_cover, cover_change) |>
  crossing(y_breaks = c(-4, 0, 2))

# Plot "heatmap" with points using full names
cover_points_plot_fixed <- ggplot() +
  # Create facets with wrapped labels
  facet_grid(intial_cover_wrapped ~ cover_change_wrapped, switch = "y") +
  # Add colored background for significant plots
  geom_rect(data = cover_points_full_wrapped %>% filter(plot_type == "significant"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = Significant)) +
  # Add black background for same initial/cover change
  geom_rect(data = cover_points_full_wrapped %>% filter(plot_type == "same"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "black") +
  # Add horizontal reference line at y=0
  geom_hline(data = cover_points_full_wrapped %>% filter(plot_type == "significant"), 
             aes(yintercept = 0), 
             linetype = "dotted") +
  # Set x-axis scale
  scale_x_continuous(limits = c(0, 1), position = "top", breaks = NULL) +
  # Add error bars
  geom_errorbar(data = cover_points_full_wrapped %>% filter(plot_type == "significant"),
                aes(x = 0.5, ymin = Estimate - std_error, ymax = Estimate + std_error),
                width = 0.1,
                color = "black") +
  # Add points
  geom_point(data = cover_points_full_wrapped %>% filter(plot_type == "significant"),
             aes(x = 0.5, y = Estimate),
             size = 2,
             color = "black") +
  # Add y-axis labels
  geom_text(data = y_breaks_df_wrapped,
            aes(x = 0.001, y = y_breaks, label = y_breaks),
            hjust = 0,
            size = 3) +
  # Set color scale
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90", 
                       name = "Estimate", 
                       breaks = legend_breaks_cover,
                       labels = scales::label_number()) +
  # Set y-axis limits
  coord_cartesian(ylim = c(floor(y_min), ceiling(y_max))) +
  # Theme customization
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),
    # Adjust strip text to accommodate wrapped labels
    strip.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    strip.text.y.left = element_text(size = 11, angle = 0, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank(),
    # Add some extra spacing for wrapped text
    strip.text = element_text(margin = margin(3, 3, 3, 3))) +
  labs(y = "Initial Cover",
       x = "Cover change")

# Combine the two plots
cover_time_point_plot <- plot_grid(cover_points_plot_fixed, time_updated,
                                   ncol = 1, labels = c("a)", "b)"))

# Save to file as .png
ggsave(here("figures", "model_outputs_Figure4.5.png"),
       width=20, height=13)

# Save to file as .svg
ggsave(here("figures", "model_outputs_Figure4.5.svg"),
       width=20, height=13)

# END OF SCRIPT ----------------------------------------------------------------