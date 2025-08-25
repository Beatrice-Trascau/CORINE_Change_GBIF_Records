##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 5.1_cover_change_combine_model_outputs_in_figure4
# This script contains code which combines the outputs of the individual land
# cover models into a single figure for the paper (Figure 4)
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Load your new individual land cover models (adjust file names as needed)
load(here("data", "models","urban_model1_time.RData"))
load(here("data", "models","complex_agri_model1_interaction.RData"))
load(here("data", "models", "agri_sig_veg_model1_interaction.RData"))
load(here("data", "models", "forests_model1_interaction.RData"))
load(here("data", "models","moors_model2_no_interaction.RData"))
load(here("data", "models","woodland_model2_no_interaction.RData"))
load(here("data", "models","sparse_model2_no_interaction.RData"))

# 2. EXTRACT MODEL OUTPUTS -----------------------------------------------------

# Create list of models with type indicators
model_list <- list(extract_summary_as_df(urban_temporal_model, "temporal"),
                   extract_summary_as_df(complex_agri_model1_interaction, "transition"),
                   extract_summary_as_df(agri_sig_veg_model1_interaction, "transition"), 
                   extract_summary_as_df(forests_model1_interaction, "transition"),
                   extract_summary_as_df(moors_model2_no_interaction, "transition"),
                   extract_summary_as_df(woodland_model2_no_interaction, "transition"),
                   extract_summary_as_df(sparse_model2_no_interaction, "transition"))

# Combine all extracted results in a single df
combined_results <- bind_rows(model_list, .id = "model_id")

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

# More robust separation handling different term structures
combined_results3 <- combined_results2 |>
  # extract cover change part (everything after the first underscore, before any colon)
  mutate(cover_change = case_when(
    # time period terms
    str_detect(term, "^time\\.period") ~ str_extract(term, "(?<=time\\.period).*"),
    # transition terms (extract everything after "transition_type")
    str_detect(term, "^transition") ~ str_extract(term, "(?<=transition_type).*"),
    TRUE ~ NA_character_),
    # clean up the base term
    term_clean = case_when(str_detect(term, "^time\\.period") ~ "time.period",
                           str_detect(term, "^transition") ~ "transition_type", 
                           TRUE ~ term))

# Separate cover_change column into cover_change and time_period
combined_results4 <- combined_results3 |>
  mutate(time_period = case_when(str_detect(cover_change, ":") ~ str_extract(cover_change, "(?<=:).*"),
                                 TRUE ~ NA_character_),
         cover_change = case_when(str_detect(cover_change, ":") ~ str_extract(cover_change, ".*(?=:)"),
                                  TRUE ~ cover_change)) |>
  # use the cleaned term column
  select(-term) |>
  rename(term = term_clean)

# Fix names in cover_change column - handle full transition names
combined_results5 <- combined_results4 |>
  mutate(cover_change = case_when(cover_change == "2006_2012" ~ "2006-2012",
                                  cover_change == "2012_2018" ~ "2012-2018",
                                  # handle transition destinations by extracting the part after "_to_"
                                  str_detect(cover_change, "_to_") ~ case_when(str_detect(cover_change, "to_complex\\.agri$") ~ "Complex Agriculture",
                                                                               str_detect(cover_change, "to_forests$") ~ "Forests", 
                                                                               str_detect(cover_change, "to_agri\\.sig\\.veg$") ~ "Agriculture & Vegetation",
                                                                               str_detect(cover_change, "to_urban$") ~ "Urban Fabric",
                                                                               str_detect(cover_change, "to_woodland\\.shrub$") ~ "Transitional Woodland Shrub",
                                                                               str_detect(cover_change, "to_moors\\.heath\\.grass$") ~ "Moors, Heathland & Grassland",
                                                                               str_detect(cover_change, "to_sparse\\.veg$") ~ "Sparse Vegetation",
                                                                               TRUE ~ cover_change),
                                  TRUE ~ cover_change))

# 4. PLOT EFFECTS WITHOUT INTERACTIONS WITH YEARS ------------------------------

# Subset df to not contain interaction rows
effect_no_year_interaction <- combined_results5 |>
  filter(is.na(time_period)) |>
  mutate(cover_change = ifelse(is.na(cover_change) & term == "time.period2006-2012",
                               "2006-2012", cover_change),
         cover_change = ifelse(is.na(cover_change) & term == "time.period2012-2018", 
                               "2012-2018", cover_change))

# Add significance threshold
p_value_threshold <- 0.05

# Add significance, percentage change, and significance stars
effect_no_year_interaction <- effect_no_year_interaction |>
  mutate(Significant = ifelse(`Pr(>|z|)` < p_value_threshold, Estimate, NA),
         Percent_Change = (exp(Estimate) - 1) * 100,
         Significance_Star = case_when(`Pr(>|z|)` < 0.001 ~ "***",
                                       `Pr(>|z|)` < 0.01 ~ "**",
                                       `Pr(>|z|)` < 0.05 ~ "*",
                                       `Pr(>|z|)` < 0.1 ~ "†",
                                       TRUE ~ "")) |>
  filter(!is.na(cover_change))

# Split df into time-related and cover-related effects
time_effect_no_year_interaction <- effect_no_year_interaction |>
  filter(cover_change %in% c("2006-2012", "2012-2018") | 
           (intial_cover == "Urban Fabric" & model_type == "temporal" & is.na(cover_change)))

# Cover-related effect - exclude urban temporal effects
cover_effect_no_year_interaction <- effect_no_year_interaction |>
  filter(!cover_change %in% c("2006-2012", "2012-2018")) |>
  filter(!(intial_cover == "Urban Fabric" & model_type == "temporal" & is.na(cover_change)))

# Manually add rows where initial_cover equals cover_change
missing_rows <- data.frame(model_id = NA, 
                           term = NA,
                           cover_change = c("Urban Fabric", "Forests", "Complex Agriculture", 
                                            "Transitional Woodland Shrub", "Moors, Heathland & Grassland",
                                            "Sparse Vegetation", "Agriculture & Vegetation"),
                           intial_cover = c("Urban Fabric", "Forests", "Complex Agriculture", 
                                            "Transitional Woodland Shrub", "Moors, Heathland & Grassland", 
                                            "Sparse Vegetation", "Agriculture & Vegetation"),
                           Estimate = NA, `Std. Error` = NA, `z value` = NA,
                           `Pr(>|z|)` = NA, Significant = NA, Percent_Change = NA,
                           Significance_Star = "", time_period = NA, model_type = NA,
                           stringsAsFactors = FALSE)

# Add missing rows to cover df
cover_effect_no_year_interaction <- cover_effect_no_year_interaction |>
  bind_rows(missing_rows)

# Create new columns for display
cover_effect_no_year_interaction <- cover_effect_no_year_interaction |>
  mutate(is_same = ifelse(intial_cover == cover_change, TRUE, FALSE),
         # create display text - exclude all Urban Fabric transitions (including temporal effects)
         Display_Text = case_when(is_same ~ "",  # no text for diagonal
                                  intial_cover == "Urban Fabric" ~ "",  # no urban transitions at all
                                  !is.na(Percent_Change) ~ paste0(sprintf("%.0f%%", Percent_Change), Significance_Star),
                                  TRUE ~ ""),
    # use all estimates for coloring - exclude all Urban Fabric transitions
    Fill_Value = case_when(is_same ~ NA_real_,
                           intial_cover == "Urban Fabric" ~ NA_real_,  # all urban transitions blank
                           TRUE ~ Estimate))

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

# Define breaks for the cover legend
legend_breaks_cover <- c(min(cover_effect_no_year_interaction$Fill_Value, na.rm = TRUE),
                         -2, 0, 2,
                         max(cover_effect_no_year_interaction$Fill_Value, na.rm = TRUE))

# 5. PLOT EFFECTS AS POINTS WITH ERROR BARS ------------------------------------

# Use the points plot instead of simple heatmap
cover_points_full <- cover_effect_no_year_interaction |>
  mutate(plot_type = case_when(is_same ~ "same",
                               is.na(Significant) ~ "non_significant", 
                               !is.na(Significant) ~ "significant")) |>
  rename(std_error = `Std. Error`) |>
  # reorder the facet levels
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
  # ensure cover_change is in the correct order first
  mutate(cover_change = factor(as.character(cover_change), levels = correct_order),
         # create wrapped versions of the category names using str_wrap
         intial_cover_wrapped = factor(stringr::str_wrap(as.character(intial_cover), 
                                                         width = wrap_width),
                                       levels = stringr::str_wrap(levels(intial_cover), 
                                                                  width = wrap_width)),
         # create wrapped versions with preserved ordering
         cover_change_wrapped = factor(stringr::str_wrap(as.character(cover_change), 
                                                         width = wrap_width),
                                       levels = stringr::str_wrap(correct_order, 
                                                                  width = wrap_width)))

# Update the y_breaks_df to use the wrapped labels
y_breaks_df_wrapped <- cover_points_full_wrapped |>
  filter(plot_type == "significant") |>
  distinct(intial_cover_wrapped, cover_change_wrapped, .keep_all = TRUE) |>
  select(intial_cover_wrapped, cover_change_wrapped, intial_cover, cover_change) |>
  crossing(y_breaks = c(-4, 0, 2))

# Plot "heatmap" with points using full names - show ALL effects with significance indicators
cover_points_plot_fixed <- ggplot() +
  # create facets with wrapped labels
  facet_grid(intial_cover_wrapped ~ cover_change_wrapped, switch = "y") +
  # add colored background for ALL plots (not just significant)
  geom_rect(data = cover_points_full_wrapped |> filter(plot_type != "same"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = Estimate)) +
  # add black background for same initial/cover change
  geom_rect(data = cover_points_full_wrapped |> filter(plot_type == "same"), 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "black") +
  # add horizontal reference line at y=0 for ALL plots
  geom_hline(data = cover_points_full_wrapped |> filter(plot_type != "same"), 
             aes(yintercept = 0), 
             linetype = "dotted") +
  # set x-axis scale
  scale_x_continuous(limits = c(0, 1), position = "top", breaks = NULL) +
  # add error bars for ALL effects
  geom_errorbar(data = cover_points_full_wrapped |> filter(plot_type != "same"),
                aes(x = 0.5, ymin = Estimate - std_error, ymax = Estimate + std_error),
                width = 0.1,
                color = "black") +
  # add points for ALL effects
  geom_point(data = cover_points_full_wrapped |> filter(plot_type != "same"),
             aes(x = 0.5, y = Estimate),
             size = 2,
             color = "black") +
  # add significance stars
  geom_text(data = cover_points_full_wrapped |> filter(plot_type != "same"),
            aes(x = 0.5, y = Estimate + std_error + 0.3, label = Significance_Star),
            size = 4, fontface = "bold", color = "black") +
  # add y-axis labels for ALL plots (update to use all data)
  geom_text(data = cover_points_full_wrapped |> 
              filter(plot_type != "same") |>
              distinct(intial_cover_wrapped, cover_change_wrapped) |>
              crossing(y_breaks = c(floor(y_min), 0, ceiling(y_max))),
            aes(x = 0.05, y = y_breaks, label = y_breaks),
            hjust = 0,
            size = 2.5,
            color = "black",
            fontface = "plain") +
  # set color scale
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90", 
                       name = "Estimate", 
                       breaks = legend_breaks_cover,
                       labels = scales::label_number()) +
  # Set y-axis limits
  coord_cartesian(ylim = c(floor(y_min), ceiling(y_max))) +
  # Theme customization
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        panel.grid = element_blank(),
        # adjust strip text to accommodate wrapped labels
        strip.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
        strip.text.y.left = element_text(size = 11, angle = 0, hjust = 1),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.line = element_blank(),
        # add some extra spacing for wrapped text
        strip.text = element_text(margin = margin(3, 3, 3, 3))) +
  labs(y = "Initial Cover",
       x = "Cover change",
       caption = "*** p<0.001, ** p<0.01, * p<0.05, † p<0.1")

# Replace the simple heatmap with the points plot
a <- cover_points_plot_fixed

# Create a label mapping for time effects
label_mapping <- c("Agriculture & Vegetation" = "ASNV",
                   "Complex Agriculture" = "CA",
                   "Forests" = "Forests",
                   "Moors, Heathland & Grassland" = "MHG",
                   "Sparse Vegetation" = "SV",
                   "Transitional Woodland Shrub" = "TWS",
                   "Urban Fabric" = "UF")

# Process time effects - fix the data filtering
time_effect_no_year_interaction <- time_effect_no_year_interaction |>
  # clean up the cover_change values for time effects
  mutate(cover_change = case_when(cover_change == "2006_2012" ~ "2006-2012",
                                  cover_change == "2012_2018" ~ "2012-2018", 
                                  is.na(cover_change) & intial_cover == "Urban Fabric" ~ case_when(str_detect(term, "2006_2012") ~ "2006-2012",
                                                                                                   str_detect(term, "2012_2018") ~ "2012-2018",
                                                                                                   TRUE ~ cover_change),
                                  TRUE ~ cover_change)) |>
  # filter out any remaining NAs
  filter(!is.na(cover_change)) |>
  mutate(intial_cover = factor(label_mapping[as.character(intial_cover)]),
         intial_cover = factor(intial_cover,
                               levels = c("UF", "TWS", "SV", "MHG",
                                          "Forests", "CA", "ASNV")))

# Define breaks for the time legend
legend_breaks_time <- c(min(time_effect_no_year_interaction$Significant, na.rm = TRUE), 
                        0, 0.5, 1.0,
                        max(time_effect_no_year_interaction$Significant, na.rm = TRUE))

# Add significance indicator to time effects
time_effect_no_year_interaction <- time_effect_no_year_interaction |>
  mutate(plot_type = ifelse(`Pr(>|z|)` < p_value_threshold, "significant", "non_significant"))

# Filter to only include significant time effects
time_sig <- time_effect_no_year_interaction |>
  filter(plot_type == "significant")

# Convert abbreviated labels back to full names for plotting
reverse_mapping <- setNames(names(label_mapping), label_mapping)

time_sig <- time_sig |>
  mutate(intial_cover_full = factor(reverse_mapping[as.character(intial_cover)],
                                    levels = c("Urban Fabric",
                                               "Transitional Woodland Shrub",
                                               "Sparse Vegetation",
                                               "Moors, Heathland & Grassland",
                                               "Forests",
                                               "Complex Agriculture",
                                               "Agriculture & Vegetation")))

# Define wrap width and apply to time_sig
wrap_width <- 15

time_sig_wrapped <- time_sig |>
  mutate(intial_cover_full_wrapped = factor(stringr::str_wrap(as.character(intial_cover_full), 
                                                              width = wrap_width),
                                            levels = stringr::str_wrap(c("Urban Fabric","Transitional Woodland Shrub",
                                                                         "Sparse Vegetation",
                                                                         "Moors, Heathland & Grassland",
                                                                         "Forests",
                                                                         "Complex Agriculture",
                                                                         "Agriculture & Vegetation"), 
                                                                       width = wrap_width)),
         cover_change_wrapped = factor(stringr::str_wrap(as.character(cover_change), 
                                                         width = wrap_width)))

# Create time effects plot
time_updated <- ggplot(time_sig_wrapped, 
                       aes(x = cover_change_wrapped, y = intial_cover_full_wrapped)) +
  geom_tile(aes(fill = Significant), color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#0072B2", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90", name = "Estimate", 
                       breaks = legend_breaks_time, 
                       labels = scales::label_number()) +
  scale_x_discrete(position = "top") +
  theme_classic() +
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 11),  
    axis.text.y = element_text(size = 11),              
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(margin = margin(3, 3, 3, 3))) +
  labs(x = "Time Period", y = "Initial Cover")

# Combine the two plots
cover_time_combined_plot <- plot_grid(a, time_updated,
                                      ncol = 1, labels = c("a)", "b)"))

# Save to file
ggsave(here("figures", "model_outputs_Figure4_updated.png"),
       plot = cover_time_combined_plot,
       width = 20, height = 13, dpi = 300)

ggsave(here("figures", "model_outputs_Figure4_updated.svg"),
       plot = cover_time_combined_plot,
       width = 20, height = 13)

# END OF SCRIPT ----------------------------------------------------------------