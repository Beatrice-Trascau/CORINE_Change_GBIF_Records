# 1. DEBUGGING EXTRACTION OF LAND COVER VALUES AT OCCURRENCE RECORDS ----

## 1.1. Make a copy of just the first layer of CORINE ----
corine_2000 <- norway_corine_change_modified_stack[[1]]

# check values
corine_2000_df <- as.data.frame(corine_2000)
levels(as.factor(corine_2000_df$U2006_CHA0006_00_V2020_20u1))

## 1.2. Re-project corine to WGS 84 (the crs used by GBIF records) ----
corine_2000_wgs84 <- project(corine_2000, "+proj=longlat +datum=WGS84 +no_defs",
                             method = "near")

# check values
corine_2000_wgs84_df <- as.data.frame(corine_2000_wgs84)
levels(as.factor(corine_2000_wgs84_df$U2006_CHA0006_00_V2020_20u1))

## 1.3. Aggregate corine layer to 250km by 250 km ----
factor <- 25000 / 100
coarse_corine <-  terra::aggregate(corine_2000_wgs84, fact=factor, 
                            fun=max, na.rm=TRUE)

# check values
coarse_corine_df <- as.data.frame(coarse_corine)
levels(as.factor(coarse_corine_df$U2006_CHA0006_00_V2020_20u1))


## 1.4. Select a subset of 200 records ----
set.seed(310393)
random_occurrences <- sample(nrow(occurrences_norway), 200) #get indices of a randim subset of 200 rows
random_subset_occurrences <- occurrences_norway[random_occurrences, ] #subset dataframe

## 1.5. Convert occurrences to spatial dataframe ----
occurrences_sp <- st_as_sf(random_subset_occurrences, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(coarse_corine))

## 1.6. Convert occurrences to spatial vector ----
occurrences_vect <- vect(occurrences_sp)


## 1.7.Extract cell information for each occurrence ----

#Create additional layer with unique ID for the CORINE layers
ID_raster <- coarse_corine
values(ID_raster) <- 1:ncell(coarse_corine)

# Combine ID raster with CORINE
corine_ID <- c(coarse_corine, ID_raster)

# Extract raster values for occurrences and SSB IDs
corine_ID_occurrences <- terra::extract(corine_ID, occurrences_vect)
levels(as.factor(corine_ID_occurrences$U2006_CHA0006_00_V2020_20u1))

# 2. STEPWISE COMPARISION OF CELLS WITH THE SAME INITIAL LAND COVER AND SSB ID ----

## 2.1. Crate dummy dataframe to test stuff out ----
land_cover_categories <- c("urban", "forest", "agriculture")

n <- 200
cell_ID <- 1:n
land_cover_2000 <- sample(land_cover_categories, n, replace = TRUE)
land_cover_2006 <- sample(land_cover_categories, n, replace = TRUE)
SSB_ID <- sample(paste0("SSB_", 1:10), n, replace = TRUE)
species_richness <- sample(1:100, n, replace = TRUE)
cover_change <- sample(c("Y", "N"), n, replace = TRUE)

df <- data.frame(cell_ID, land_cover_2000, land_cover_2006, 
                 SSB_ID, species_richness, cover_change)



## 2.2.Subset data based on unique combination of land cover in 2000 and SSB ID ----
filtered_df <- df |>
  group_by(land_cover_2000, SSB_ID) |>
  filter(n() > 1) |>
  ungroup()


## 2.3. Plot comparison for each combination ----
ggplot(filtered_df, aes(x = cover_change, y = species_richness, fill = cover_change)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  facet_wrap(~ land_cover_2000 + SSB_ID) + # Optional: to separate plots by land_cover_2000 and SSB_ID
  labs(title = "Species Richness by Cover Change", 
       x = "Cover Change", 
       y = "Average Species Richness") +
  theme_minimal()

