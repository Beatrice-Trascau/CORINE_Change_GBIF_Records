##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.3_GBIF_data_exploration
# This script contains code which cleans the downloaded GBIF occurrence records
##----------------------------------------------------------------------------##

# 1. LOAD & PREPARE DATA -------------------------------------------------------

# Cleaned occurrence records
load(here("data", "derived_data","clened_occurrences_redownloaded_August2025.rda"))
occurrences_norway <- clean_occurrences

# All the before periods (avoids overlap)
occurrences_before <- occurrences_norway |>
  filter(year %in% c(1997:2000, 2003:2006, 2009:2012))|>
  mutate(time_period = case_when(year %in% 1997:2000 ~ "before_2000_2006",
                                 year %in% 2003:2006 ~ "before_2006_2012",
                                 year %in% 2009:2012 ~ "before_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# All the after periods (avoids overlap)
occurrences_after <- occurrences_norway |>
  filter(year %in% c(2006:2009, 2012:2015, 2018:2021))|>
  mutate(time_period = case_when(year %in% 2006:2009 ~ "after_2000_2006",
                                 year %in% 2012:2015 ~ "after_2006_2012",
                                 year %in% 2018:2021 ~ "after_2012_2018",
                                 TRUE ~ NA_character_)) |>
  filter(!is.na(time_period))

# Combine before and after
occurrences_df_periods <- bind_rows(occurrences_before, occurrences_after)


# 2. PLOT TAXONOMIC DISTRIBUTION -----------------------------------------------

# Classify records
taxonomic_composition <- occurrences_df_periods |>
  mutate(taxonomic_group = case_when(kingdom == "Plantae" ~ "Plants",
                                     class == "Aves" ~ "Birds",
                                     phylum == "Arthropoda" ~ "Arthropods",
                                     class == "Mammalia" ~ "Mammals",
                                     TRUE ~ "Other"))

# Calculate counts and proportions by period and taxonomic group
taxonomic_summary <- taxonomic_composition |>
  filter(!is.na(time_period)) |>
  group_by(time_period, taxonomic_group) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(time_period) |>
  mutate(total = sum(count), proportion = count / total) |>
  ungroup()

# Create a colour palette
taxonomic_colours <- c("Plants" = "#66C2A5", "Birds" = "#FC8D62", 
                       "Arthropods" = "#8DA0CB", "Mammals" = "#E78AC3",
                       "Other" = "#A6D854")

# Define order of the periods
period_order <- c("before_2000_2006", "after_2000_2006",
                  "before_2006_2012", "after_2006_2012",
                  "before_2012_2018", "after_2012_2018")

# Define the new labels for the periods
period_labels <- c("before_2000_2006" = "1997-2000",
                   "after_2000_2006" = "2006-2009", 
                   "before_2006_2012" = "2003-2006",
                   "after_2006_2012" = "2012-2015",
                   "before_2012_2018" = "2009-2012",
                   "after_2012_2018" = "2018-2021")

# Convert period column to a factor and follow the order
taxonomic_summary <- taxonomic_summary |>
  mutate(time_period = factor(time_period, levels = period_order))

# Create stacked barplot
taxonomic_plot <- ggplot(taxonomic_summary,
                         aes(x = time_period, y = proportion, fill = taxonomic_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = taxonomic_colours) +
  scale_x_discrete(labels = period_labels) +
  labs(x = "Time Period", y = "Proportion", fill = "Group") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))

# Save figure as .png
ggsave(filename = here("figures", "SupplementaryFigure1_taxonomic_composition_across_periods.png"),
       width = 10, height = 8, dpi = 300)

# 3. PLOT PUBLISHER NAMES ------------------------------------------------------

# Identify the top 10 publishers by total records
top_publishers <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  count(publisher, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(publisher)

# Map original publisher names to something that makes more sense
publisher_name_mapping <- c("The Norwegian Biodiversity Information Centre (NBIC)" = "NBIC",
                            "Miljølære.no" = "Miljølære.no",
                            "Cornell Lab of Ornithology"= "Cornell Ornithology",
                            "Norwegian Institute for Nature Research" = "NINA",
                            "Biofokus" = "Biofokus",
                            "University of Oslo" = "UiO",
                            "Norwegian University of Science and Technology" = "NTNU",
                            "Biolog J.B. Jordal AS" = "Biolog J.B. Jordal",
                            "Norwegian University of Life Sciences (NMBU)" = "NMBU",
                            "Norwegian Environment Agency" = "Norwegian Environment Agency")

# Calculate counts and proportions by period and dataset name
publisher_summary <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  mutate(publisher_group = case_when(publisher == "" ~ "No name provided",
                                     publisher %in% names(publisher_name_mapping) ~ publisher_name_mapping[publisher],
                                     publisher %in% top_publishers ~ publisher,
                                     TRUE ~ "Other")) |>
  group_by(time_period, publisher_group) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(time_period) |>
  mutate(total = sum(count), proportion = count / total) |>
  ungroup()

# Convert period column to a factor and follow the order
publisher_summary <- publisher_summary |>
  mutate(time_period = factor(time_period, levels = period_order))

# Create the mapped names for the top datasets
top_publisher_mapped <- case_when(top_publishers == "" ~ "No name provided",
                                  top_publishers %in% names(publisher_name_mapping) ~ publisher_name_mapping[top_publishers],
                                 TRUE ~ top_publishers)

# Create colour pallete
publisher_colours <- c(brewer.pal(length(top_publisher_mapped), "Spectral"), "Other" = "#CCCCCC")
names(publisher_colours)[1:length(top_publisher_mapped)] <- top_publisher_mapped

# Create stacked barplot
publisher_plot <- ggplot(publisher_summary,
                         aes(x = time_period, y = proportion, fill = publisher_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = publisher_colours) +
  scale_x_discrete(labels = period_labels) +
  labs(x = "Time Period", y = "Proportion", fill = "Publisher") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))

# Save figure as .png
ggsave(filename = here("figures", "SupplementaryFigure2_publihser_composition_across_periods.png"),
       width = 12, height = 10, dpi = 300)

# Create table of all publishers broken down by period
publisher_table <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  group_by(publisher, time_period) |>
  summarise(count = n(), .groups = "drop") |>
  # calculate total records per publisher across all periods
  group_by(publisher) |>
  mutate(total_records = sum(count)) |>
  ungroup() |>
  # pivot to wide format with periods as columns
  pivot_wider(names_from = time_period, values_from = count, values_fill = 0) |>
  # arrange by total records in descending order
  arrange(desc(total_records)) |>
  # reorder columns to put total_records first, then the periods in logical order
  select(publisher, total_records, 
         before_2000_2006, after_2000_2006,
         before_2006_2012, after_2006_2012, 
         before_2012_2018, after_2012_2018)

# View the table
print(publisher_table)

# Save table as a csv
write_csv(publisher_table, here("data", "derived_data", "publisher_records_by_period.csv"))

# 4. PLOT BY DATASET NAME ------------------------------------------------------

# Check how many datasets there are
length(unique(occurrences_df_periods$datasetName)) # 1640

# Identify the top 10 datasets by total records
top_datasets <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  count(datasetName, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(datasetName)

# Map original dataset names to something that makes more sense
dataset_name_mapping <- c("Kartleggingsmidler Sabima" = "Mapping tools Sabima",
                          "BirdLife OA" = "BirdLife OA",
                          "Prosjekt Saltens flora" = "Proj Saltens flora",
                          "Florakartlegging Østfold" = "Flora mapping Østfold",
                          "iNaturalist research-grade observations" = "iNaturalist research-grade obs",
                          "Effects of vegetation clearing and dead wood on beetles in power line clearings southeast Norway" = "Powerline clearings South",
                          "Miljøfaglig Utredning AS" = "Environmental Investigation",
                          "Statens vegvesen" = "Norwegian Public Roads Administration",
                          "Miljodir_naturtypekartlegging_2021" = "Environmental Agency Habitat Mapping 2021")

# Calculate counts and proportions by period and dataset name
dataset_summary <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  mutate(dataset_group = case_when(datasetName == "" ~ "No name provided",
                                   datasetName %in% names(dataset_name_mapping) ~ dataset_name_mapping[datasetName],
                                   datasetName %in% top_datasets ~ datasetName,
                                   TRUE ~ "Other")) |>
  group_by(time_period, dataset_group) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(time_period) |>
  mutate(total = sum(count), proportion = count / total) |>
  ungroup()

# Convert period column to a factor and follow the order
dataset_summary <- dataset_summary |>
  mutate(time_period = factor(time_period, levels = period_order))

# Create the mapped names for the top datasets
top_datasets_mapped <- case_when(top_datasets == "" ~ "No name provided",
                                 top_datasets %in% names(dataset_name_mapping) ~ dataset_name_mapping[top_datasets],
                                 TRUE ~ top_datasets)

# Create colour pallete
dataset_colours <- c(brewer.pal(length(top_datasets_mapped), "Spectral"), "Other" = "#CCCCCC")
names(dataset_colours)[1:length(top_datasets_mapped)] <- top_datasets_mapped

# Create stacked barplot
dataset_plot <- ggplot(dataset_summary,
                       aes(x = time_period, y = proportion, fill = dataset_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = dataset_colours) +
  scale_x_discrete(labels = period_labels) +
  labs(x = "Time Period", y = "Proportion", fill = "Dataset Name") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14))

# Save figure as .png
ggsave(filename = here("figures", "SupplementaryFigure3_publihser_composition_across_periods.png"),
       width = 10, height = 8, dpi = 300)

# Create table of all publishers broken down by period
dataset_table <- occurrences_df_periods |>
  filter(!is.na(time_period)) |>
  group_by(datasetName, time_period) |>
  summarise(count = n(), .groups = "drop") |>
  # calculate total records per dataset across all periods
  group_by(datasetName) |>
  mutate(total_records = sum(count)) |>
  ungroup() |>
  # pivot to wide format with datasets as columns
  pivot_wider(names_from = time_period, values_from = count, values_fill = 0) |>
  # arrange by total records in descending order
  arrange(desc(total_records)) |>
  # reorder columns to put total_records first, then the periods in logical order
  select(datasetName, total_records, 
         before_2000_2006, after_2000_2006,
         before_2006_2012, after_2006_2012, 
         before_2012_2018, after_2012_2018)

# Save table as a csv
write_csv(dataset_table, here("data", "derived_data", "dataset_name_records_by_period.csv"))
