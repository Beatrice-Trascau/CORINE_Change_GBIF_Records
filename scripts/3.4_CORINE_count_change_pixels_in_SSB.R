##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.4_CORINE_count_change_pixels_in_SSB
# This script contains code which counts the number of CORINE status pixels
# which are (not)changing in each SSB ID
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# CORINE stack
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# SSB grid
ssb_grid <- vect(here("data", "raw_data",
                       "SSB050KM", "ssb50km.shp"))

# 2. PLOT  NUMBER OF PIXELS CHANGING/NOT CHANGING IN EACH SSB ID: 2000-2006 ----

# CORINE Layers for 2000 and 2006
corine_2000 <- norway_corine_status_modified_stack[[1]]
corine_2006 <- norway_corine_status_modified_stack[[2]]
corine_2012 <- norway_corine_status_modified_stack[[3]]
corine_2018 <- norway_corine_status_modified_stack[[4]]

# Check if corine and SSBs have the same CRS
crs(corine_2000, proj = TRUE) #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(ssb_grid, proj = TRUE) #"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"

# List of variable names as strings
corine_vars <- c("corine_2000", "corine_2006", "corine_2012", "corine_2018")

# Loop to re-project layers to match the SSB grid
for (var in corine_vars) {
  # Retrieve the variable by name
  corine_layer <- get(var)
  
  # Reproject
  projected_layer <- terra::project(corine_layer, 
                                    "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
  
  # Assign the projected layer back to the original variable name
  assign(var, projected_layer)
}

# Calculate difference in land cover change between 2000 and 2006
change_raster <- corine_2000 != corine_2006

# Extract raster values by SSB grid
ssb_values <- terra::extract(change_raster, ssb_grid)

# Summarize the values to count changing and not changing pixels
ssb_summary <- ssb_values %>%
  group_by(ID) %>%
  summarise(changing = sum(U2006_CLC2000_V2020_20u1, na.rm = TRUE), 
            not_changing = sum(!U2006_CLC2000_V2020_20u1, na.rm = TRUE))

# Add SSB ID to ssb_summary
ssb_summary$SSBid <- ssb_grid$SSBid

# Merge the summary values with the SSB grid
ssb_grid$changing <- ssb_summary$changing
ssb_grid$not_changing <- ssb_summary$not_changing


# Classify SSB grid based on changes
ssb_grid$classification <- ifelse(ssb_grid$changing > ssb_grid$not_changing, 
                                  "Changing", "Not Changing")

# Change to ggplot suitable format
ssb_grid_centroids <- centroids(ssb_grid)  # Calculate centroids

# Convert to dataframe
ssb_grid_df <- as.data.frame(ssb_grid_centroids, xy = TRUE)

# Select only needed columns
ssb_grid_df <- ssb_grid_df %>%
  select(SSBid, CENTROID_X, CENTROID_Y, changing, not_changing, classification)

# Convert to dataframe
ssb_grid_df <- as.data.frame(ssb_grid, xy=TRUE)

# Select only needed columns
ssb_grid_df <- ssb_grid_df %>%
  select(SSBid, CENTROID_X, CENTROID_Y, classification)

# Plot map
ssb_grid_2000.2006 <- ggplot(ssb_grid_df) +
  geom_tile(aes(x = CENTROID_X, y = CENTROID_Y, fill = classification), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Changing" = "#800080", "Not Changing" = "#FFD700")) +
  theme_classic() +
  labs(fill = "Change Classification")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.text = element_text(size = 14))

# Convert ggplot to interactive plot with plotly
interactive_ssb_grid_2000.2006 <- ggplotly(ssb_grid_2000.2006)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_ssb_grid_2000.2006, 
                        here("figures", "additional_figures",
                             "number_of_changing_pixels_ssb_grid_ map_2000.2006.html"))

# 3. PLOT THE NUMBER OF PIXELS CHANGING/NOT CHANGING IN EACH SSB ID ------------

# Calculate difference in land cover change between 2000 and 2006
change_raster_2012.2018 <- corine_2012 != corine_2018

# Extract raster values by SSB grid
ssb_values_2012.2018 <- terra::extract(change_raster_2012.2018, ssb_grid)

# Summarize the values to count changing and not changing pixels
ssb_summary_2012.2018 <- ssb_values_2012.2018 %>%
  group_by(ID) %>%
  summarise(changing = sum(U2018_CLC2012_V2020_20u1, na.rm = TRUE), 
            not_changing = sum(!U2018_CLC2012_V2020_20u1, na.rm = TRUE))

# Add SSB ID to ssb_summary
ssb_summary_2012.2018$SSBid <- ssb_grid$SSBid

# Merge the summary values with the SSB grid
ssb_grid$changing <- ssb_summary_2012.2018$changing
ssb_grid$not_changing <- ssb_summary_2012.2018$not_changing


# Classify SSB grid based on changes
ssb_grid$classification <- ifelse(ssb_grid$changing > ssb_grid$not_changing, 
                                  "Changing", "Not Changing")

# Change to ggplot suitable format
ssb_grid_centroids <- centroids(ssb_grid)  # Calculate centroids
ssb_grid_df <- as.data.frame(ssb_grid_centroids, xy = TRUE)
ssb_grid_df <- ssb_grid_df %>%
  select(SSBid, CENTROID_X, CENTROID_Y, changing, not_changing, classification)

# Plot map
ssb_grid_2012.2018 <- ggplot(ssb_grid_df) +
  geom_tile(aes(x = CENTROID_X, y = CENTROID_Y, fill = classification), 
            color = "white", size = 0.1) +
  scale_fill_manual(values = c("Changing" = "#800080", 
                               "Not Changing" = "#FFD700")) +
  theme_classic() +
  labs(fill = "Change Classification")+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom", 
    legend.title = element_blank(),
    legend.text = element_text(size = 14))

# Convert ggplot to interactive plot with plotly
interactive_ssb_grid_2012.2006 <- ggplotly(ssb_grid_2000.2006)

# Save interactive plot as html file
htmlwidgets::saveWidget(interactive_ssb_grid_2000.2006, 
                        here("figures","number_of_changing_pixels_ssb_grid_ map_2000.2006.html"))

# END OF SCRIPT ----------------------------------------------------------------