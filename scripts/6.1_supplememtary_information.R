##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 6.1_supplementary_information
# This script contains code which plots and extracts information used in the
# supplementary information of the manuscript
##----------------------------------------------------------------------------##

# 1. LAND COVER CATEGORIES AS % OF TOTAL AREA ----------------------------------

# Load data
norway_corine_status_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_stack.tif"))
# Extract 2018 layer
norway_2018 <- norway_corine_status_stack[[4]]

# Get frequency table of all values
freq_table <- freq(norway_2018)

# Calculate percentages
freq_table$percentage <- round((freq_table$count / sum(freq_table$count)) * 100, 4)

# View results
print(freq_table)

# Create better table
freq_table |>
  kable(col.names = c("layer", "clc value", "pixel count", "percentage")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

# Save frequency table
write.csv(freq_table, here("data", "derived_data",
                           "norway_2018_pixel_percentages.csv"), 
          row.names = FALSE)

# 2. Y/N LC CHANGE SUMMARY -----------------------------------------------------

# Load data
load(here("data", "derived_data",
          "occ_y_n_cover_change_before_after_for_modell.rda"))

## 2.1. Get summary statistics -------------------------------------------------

# Summarise data
summary_stats_y_n <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)  
  ) |>
  ungroup()

# Create a table from the data
summary_stats_y_n |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 2.2. Summary figures --------------------------------------------------------

# First panel with original data
p1 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Log-transform values
occ_y_n_cover_change_before_after_for_modell <- occ_y_n_cover_change_before_after_for_modell |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Second panel with logged data
p2 <- ggplot(occ_y_n_cover_change_before_after_for_modell, 
             aes(x = time_period, y = occ_log, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Combine plots
combined_violins_y_n <- plot_grid(p1, p2,
                                  labels = c("a)", "b)"),
                                  ncol = 2,
                                  align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_y_n_FigureS4.png"),
       width=20, height=13)

# 3. INTENSIFICATION/EXTENSIFICATION SUMMARY -----------------------------------

# Load data
load(here("data", "derived_data",
          "occ_intens_extens_before_after_for_model.rda"))

## 3.1. Summary statistics -----------------------------------------------------

# Summarise data
summary_stats_intens_extens <-  occ_intens_extens_before_after_for_model |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)) |>
  ungroup()

# Create table
summary_stats_intens_extens |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 3.2. Summary figures --------------------------------------------------------

# First panel with original data
q1 <- ggplot(occ_intens_extens_before_after_for_model, 
             aes(x = time_period, y = ocurrences_after, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("N" = "#66c2a5", "Y" = "sienna")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Log-transform values
occ_intens_extens_before_after_for_model <- occ_intens_extens_before_after_for_model |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Second panel with logged data
q2 <- ggplot(occ_intens_extens_before_after_for_model, 
             aes(x = time_period, y = occ_log, 
                 fill = cover_change)) +
  geom_violin(position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7),
             alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("Extensification" = "#66c2a5", 
                               "Intensification" = "sienna",
                               "No_change" = "#0072B2")) +
  labs(x = "Time Period",
       y = "Number of Occurrences",
       fill = "Cover Change") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))

# Combine plots
combined_violins_intens_extens <- plot_grid(q1, q2,
                                            labels = c("a)", "b)"),
                                            ncol = 2,
                                            align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_intens_extens_FigureS5.png"),
       width=20, height=13)

# 4. COVER CHANGE TYPES SUMMARY -------------------------------------------------

# Load data
load(here("data", "derived_data", 
          "occ_cover_change_types_before_after_for_model.rda"))

## 4.1. Summary statistics -----------------------------------------------------

# Summarise data
summary_stats_change_types <-  occ_cover_change_types_before_after_for_model |>
  group_by(time_period, cover_change) |>
  summarise(
    n = n(),  
    median = median(ocurrences_after),
    mode = list(names(sort(table(ocurrences_after), decreasing = TRUE)[1])),
    max = max(ocurrences_after),
    min = min(ocurrences_after),
    zeros = sum(ocurrences_after == 0)) |>
  ungroup()

# Create table
summary_stats_change_types |>
  kable(col.names = c("Time Period", "Cover Change", "N", "Median", 
                      "Mode", "Max", "Min", "Zeros")) |>
  kable_styling(bootstrap_options = c("striped", "hover"))

## 4.2. Summary figures --------------------------------------------------------

# Check what categories there are in the df
unique(occ_cover_change_types_before_after_for_model$cover_change)

# Log-transform values
occ_cover_change_types_before_after_for_model <- occ_cover_change_types_before_after_for_model |>
  mutate(occ_log = log(ocurrences_after + 0.0001))

# Create mapping for initial land cover mappings
cover_mapping <- c(
  "urban" = "Urban",
  "complex" = "Complex Agricultural Cover",
  "agri" = "Agriculture with Significant Natural Vegetation",
  "forests" = "Forest",
  "moors" = "Moors, Heathland & Grassland",
  "woodland" = "Transitional Woodland Shrub",
  "sparse" = "Sparse Vegetation")

# Get initial land cover by splitting the cover_change column
transformed_df <- occ_cover_change_types_before_after_for_model |>
  # Split cover_change and take first element
  mutate(initial_cover = str_split_fixed(cover_change, "_", n = 2)[,1]) |>
  # Recode values using the mapping
  mutate(initial_cover = recode(initial_cover, !!!cover_mapping)) |>
  # Remove rows with "other"
  filter(!str_detect(cover_change, "other"))

# Define list of cover categories
unique_covers <- unique(transformed_df$initial_cover)

# Define list to store plots
plot_list <- list()

# Loop to create violin plots for each cover category
for(cover in unique_covers){
  plot_list[[cover]] <- transformed_df |>
    filter(initial_cover == cover) |>
    ggplot(aes(x = time_period, y = occ_log)) +
    geom_violin(fill = "#66c2a5") +
    geom_point(position = position_jitter(width = 0.1),
               alpha = 0.3, size = 1) +
    labs(x = "Time Period",
         y = "Number of Occurrences") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 10))
}

# Calculate number of rows needed for grid
n_plots <- length(plot_list)
n_cols <- 3
n_rows <- ceiling(n_plots / n_cols)

# Create labels for plots
plot_labels <- paste0(letters[1:n_plots], ")")

# Combine plots
combined_violins_change_types <- plot_grid(plot_list[[1]], plot_list[[2]],
                                           plot_list[[3]], plot_list[[4]],
                                           plot_list[[5]], plot_list[[6]],
                                           plot_list[[7]],
                                           labels = c("a)", "b)", "c)", "d)",
                                                      "e)", "f)", "g)"),
                                           ncol = 3,
                                           align = "h")

# Save to file as .png
ggsave(here("figures", "occurrences_violins_change_types_FigureS6.png"),
       width=20, height=13)

# 5. DATASET NAME EXPLORATIONS -------------------------------------------------
# Load data
load(here("data", "derived_data",
          "occ_y_n_cover_change_before_after_extra_info_Jan2025.rda"))

# Check number of publishers
length(unique(occ_y_n_cover_change_before_after_for_modell$publisher.x))

# Rename data for easier work
df <- occ_y_n_cover_change_before_after_for_modell


## 5.1. Plot 1 -----------------------------------------------------------------

# For cover_change = "Y"
plot_Y <- df %>%
  filter(cover_change == "Y") %>%
  group_by(publisher.x) %>%
  summarize(total_occurrences = sum(ocurrences_after)) %>%
  arrange(desc(total_occurrences)) %>%
  slice_head(n = 15) %>%  # Top 15 publishers
  ggplot(aes(x = reorder(publisher.x, total_occurrences), 
             y = total_occurrences)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Publisher",
       y = "Total Occurrences (Cover Change)") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5))

# For cover_change = "N"
plot_N <- df |>
  filter(cover_change == "N") |>
  group_by(publisher.x) |>
  summarize(total_occurrences = sum(ocurrences_after)) %>%
  arrange(desc(total_occurrences)) %>%
  slice_head(n = 15) %>%  # Top 15 publishers
  ggplot(aes(x = reorder(publisher.x, total_occurrences), 
             y = total_occurrences)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # This line removes scientific notation
  labs(x = "Publisher",
       y = "Total Occurrences (No Cover Change)") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5))

# Display plots
plot_Y
plot_N

## 5.2. Plot 2 --------------------------------------------------------------------

#Prepare the data
plot_data <- df %>%
  # Group by cover_change and publisher, get counts
  group_by(cover_change, publisher.x) %>%
  summarize(counts = sum(ocurrences_after)) %>%
  # Calculate percentage within each cover_change group
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  # Keep top publishers for each category (adjust n as needed)
  arrange(desc(percentage)) %>%
  slice_head(n = 15) %>%
  # Add a category for remaining publishers if desired
  mutate(publisher.x = factor(publisher.x, levels = rev(unique(publisher.x))))

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = publisher.x)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Paired") +  # You can change the color palette
  labs(x = "Cover Change",
       y = "Percentage",
       fill = "Publisher"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

## 5.3. Plot 3 - by dataset name -----------------------------------------------

# Prepare the data
plot_data <- df %>%
  # Group by cover_change and datasetName.x, get counts
  group_by(cover_change, datasetName.x) %>%
  summarize(counts = sum(ocurrences_after)) %>%
  # Calculate percentage within each cover_change group
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  # Keep top datasets for each category (adjust n as needed)
  arrange(desc(percentage)) %>%
  slice_head(n = 10) %>%
  # Add a category for remaining datasets if desired
  mutate(datasetName.x = factor(datasetName.x, levels = rev(unique(datasetName.x))))

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = datasetName.x)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Dataset Distribution in Change vs No-Change Records",
    x = "Cover Change",
    y = "Percentage",
    fill = "Dataset Name"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5))

## 5.4. Plot 4 - by publisher and dataset --------------------------------------

# Create a named vector for publisher name mapping
publisher_map <- c(
  "The Norwegian Biodiversity Information Centre (NBIC)" = "NBIC",
  "Cornell Lab of Ornithology" = "Cornell",
  "Norwegian University of Life Sciences (NMBU)" = "NMBU",
  "University of Oslo" = "UiO",
  "Norwegian Institute for Nature Research" = "NINA",
  "The Inernational Barcode of Life Consortium" = "Barcode of Life",
  "Norwegian University of Science and Technology" = "NTNU")

# Prepare the data with shortened publisher names
plot_data <- df %>%
  # First, replace the long publisher names
  mutate(publisher.x = case_when(
    publisher.x %in% names(publisher_map) ~ publisher_map[publisher.x],
    TRUE ~ publisher.x
  )) %>%
  # Continue with the rest of the data preparation
  group_by(cover_change, datasetName.x, publisher.x) %>%
  summarize(counts = sum(ocurrences_after), .groups = 'drop') %>%
  group_by(cover_change) %>%
  mutate(percentage = counts/sum(counts) * 100) %>%
  arrange(desc(percentage)) %>%
  slice_head(n = 10) %>%
  mutate(combined_label = paste0(datasetName.x, " (", publisher.x, ")")) %>%
  mutate(combined_label = factor(combined_label, levels = rev(unique(combined_label))))

# Create custom color palette
custom_colors <- c(
  brewer.pal(8, "Set2"),
  brewer.pal(8, "Set1"),
  brewer.pal(4, "Dark2")
)

# Create the plot
ggplot(plot_data, aes(x = cover_change, y = percentage, fill = combined_label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Dataset Distribution in Change vs No-Change Records",
    x = "Cover Change",
    y = "Percentage",
    fill = "Dataset (Publisher)"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.8, "cm"),
    legend.nrow = 5
  )

# 6. SUMMARY STATISTICS --------------------------------------------------------

## 6.1. Y/N Cover Change -------------------------------------------------------

# Load data
load(here::here("data", "derived_data",
                "occ_y_n_cover_change_before_after_for_modell.rda"))

# Create a summary statistics table for occurrences_after 
occurrence_stats <- occ_y_n_cover_change_before_after_for_modell |>
  group_by(time_period, cover_change) |>
  summarise( count = n(),
             zeros = sum(ocurrences_after == 0, na.rm = TRUE),
             pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
             median = median(ocurrences_after, na.rm = TRUE),
             q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
             q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
             max = max(ocurrences_after, na.rm = TRUE),
             .groups = "drop") |>
  arrange(time_period, cover_change)

# Format with kableExtra
occurrence_stats |>
  kable(format = "html", 
        col.names = c("Time Period", "Cover Change", "Count", "Zeros", "% Zeros", 
                      "Median", "75th Percentile", "90th Percentile", "Maximum"),
        digits = c(0, 0, 0, 0, 1, 2, 2, 2, 0)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) |>
  add_header_above(c(" " = 2, "Sample Size" = 1, "Zero Values" = 2, "Distribution Statistics" = 4)) |>
  row_spec(0, bold = TRUE, background = "#F2F2F2") |>
  column_spec(1:2, bold = TRUE)

## 6.2. Intens/Extens Cover Change ---------------------------------------------

# Load data
load(here::here("data", "derived_data",
                "occ_intens_extens_before_after_for_model.rda"))

# Create a summary statistics table for occurrences_after 
occurrence_stats <- occ_intens_extens_before_after_for_model |>
  group_by(time_period, cover_change) |>
  summarise( count = n(),
             zeros = sum(ocurrences_after == 0, na.rm = TRUE),
             pct_zeros = round(sum(ocurrences_after == 0, na.rm = TRUE) / n() * 100, 1),
             median = median(ocurrences_after, na.rm = TRUE),
             q75 = quantile(ocurrences_after, 0.75, na.rm = TRUE),
             q90 = quantile(ocurrences_after, 0.90, na.rm = TRUE),
             max = max(ocurrences_after, na.rm = TRUE),
             .groups = "drop") |>
  arrange(time_period, cover_change)

# Format with kableExtra
occurrence_stats |>
  kable(format = "html", 
        col.names = c("Time Period", "Cover Change", "Count", "Zeros", "% Zeros", 
                      "Median", "75th Percentile", "90th Percentile", "Maximum"),
        digits = c(0, 0, 0, 0, 1, 2, 2, 2, 0)) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE) |>
  add_header_above(c(" " = 2, "Sample Size" = 1, "Zero Values" = 2, "Distribution Statistics" = 4)) |>
  row_spec(0, bold = TRUE, background = "#F2F2F2") |>
  column_spec(1:2, bold = TRUE)

# END OF SCRIPT ----------------------------------------------------------------