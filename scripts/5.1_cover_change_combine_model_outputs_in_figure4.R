##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 5.1_cover_change_combine_model_outputs_in_figure4
# This script contains code which combines the outputs of the individual land
# cover models into a single figure for the paper (Figure 4)
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------
load(here("data", "models", "model4.1_urban.RData"))
load(here("data", "models", "model4.2_complex_agri.RData"))
load(here("data", "models", "model4.3_agri_veg.RData"))
load(here("data", "models", "model4.4_forests.RData"))
load(here("data", "models", "model4.5_moors.RData"))
load(here("data", "models", "model4.6_woodland.RData"))
load(here("data", "models", "model4.7_sparse.RData"))

# 2. EXTRACT MODEL OUTPUTS -----------------------------------------------------

# Create list of models
model_list <- list(model4.1_urban, model4.2_complex_agri, model4.3_agri_veg,
                   model4.4_forests, model4.5_moors, model4.6_woodland,
                   model4.7_sparse)

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
missing_rows <- data.frame(
  model_id = NA, 
  term = NA,
  cover_change = c("Urban Fabric", "Forests", "Complex Agriculture", 
                   "Transitional Woodland Shrub", "Moors, Heathland & Grassland",
                   "Sparse Vegetation", "Agriculture & Vegetation"),
  intial_cover = c("Urban Fabric", "Forests", "Complex Agriculture", 
                    "Transitional Woodland Shrub", "Moors, Heathland & Grassland", 
                    "Sparse Vegetation", "Agriculture & Vegetation"),
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

time_effect_no_year_interaction <- time_effect_no_year_interaction1 |>
  mutate(intial_cover = factor(intial_cover,
                               levels = c("UF", "TWS", "SV", "MHG",
                                          "Forests", "CA", "ASNV")))

# Define breaks for the time legend
legend_breaks_time <- c(min(time_effect_no_year_interaction$Significant, na.rm = TRUE), 
                   0, -0.4, -0.8,
                   max(time_effect_no_year_interaction$Significant, na.rm = TRUE))


# Plot heatmap for time period effects
time <- ggplot(time_effect_no_year_interaction, 
               aes(x = cover_change, y = intial_cover)) +
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

# Change df to fit the plot
cover_points <- cover_effect_no_year_interaction |>
  mutate(intial_cover = factor(label_mapping[as.character(intial_cover)], 
                               levels = rev(new_order)),
    cover_change = factor(label_mapping[as.character(cover_change)]),
    plot_type = case_when(
      is_same ~ "same",
      is.na(Significant) ~ "non_significant",
      !is.na(Significant) ~ "significant")) |>
  rename(std_error = 'Std. Error')

# Calculate y-axis with only the significant results
significant_results <- cover_points |>
  filter(!is.na(Significant))

# Calculate ymin and ymax
y_min <- min(significant_data$Estimate - significant_data$std_error, 
             na.rm = TRUE)
y_max <- max(significant_data$Estimate + significant_data$std_error, 
             na.rm = TRUE)

# Create a df with break points for y axis
y_breaks_df <- cover_points |>
  filter(plot_type == "significant") |>
  distinct(intial_cover, cover_change) |>
  crossing(y_breaks = pretty(c(y_min, y_max), n = 4))

# Plot "heatmap" with points
cover_points_plot <- ggplot() +
  # Create facets for each combination of initial land cover and cover change
  facet_grid(intial_cover ~ cover_change, switch = "y") +
  # Add black background where initial_cover = cover_change
  geom_rect(data = cover_effect_no_year_interaction |> 
              filter(plot_type == "same"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "black") +
  # Add y-axis text for significant results
  geom_text(data = y_breaks_df,
            aes(x = 0.4, y = y_breaks, label = format(y_breaks, digits = 2)),
            hjust = 1,
            size = 3) +
  # Add elements only for significant plots
  geom_hline(data = cover_effect_no_year_interaction |>
               filter(plot_type == "significant"), 
             aes(yintercept = 0), 
             linetype = "dotted") +
  # Add error bars and points
  geom_errorbar(data = cover_effect_no_year_interaction |>
                  filter(plot_type == "significant"),
                aes(x = 0.5, ymin = Estimate - std_error, 
                    ymax = Estimate + std_error, color = Significant),
                width = 0.2) +
  geom_point(data = cover_effect_no_year_interaction |>
               filter(plot_type == "significant"),
             aes(x = 0.5, y = Estimate, color = Significant),
             size = 2) +
  # Custom scale for point colors
  scale_color_gradient2(low = "#0072B2", mid = "white", high = "red", 
                        midpoint = 0, na.value = "grey90", 
                        name = "Estimate", 
                        breaks = legend_breaks_cover,
                        labels = scales::label_number()) +
  # Set consistent y-axis limits
  coord_cartesian(ylim = c(y_min, y_max)) +
  # Theme customization
  theme_classic() +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    strip.text.x = element_text(hjust = 0, size = 12),
    strip.text.y.left = element_text(size = 14, hjust = 1),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank()) +
  labs(y = "Initial Cover",
       x = "Cover Change") +
  scale_x_continuous(position = "top")

# Combine the two plots
cover_time_point_plot <- plot_grid(cover_points_plot, time,
                                      ncol = 1, labels = c("a)", "b)"))

# Save to file as .png
ggsave(here("figures", "model_outputs_Figure4.5.png"),
       width=20, height=13)

# END OF SCRIPT ----------------------------------------------------------------