##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 3.1_CORINE_status_GBIF_SSB_combine
# This script contains code which combines the CORINE land cover status layers
# with the SSB administrative grids and GBIF occurrences for future analyses
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

## 1.1. Download data (if needed) ----------------------------------------------

# CORINE Status Layers
drive_download(as_id("1TEUH2UUEXsdT-4eeWGSVornzu1paQjZW"),
               path = here("data", "derived_data", 
                           "norway_corine_status_modified_stack.tif"))

# SSB Grids (zipped)
drive_download(as_id("1_AcppTeFEjQi1lX1-tn9GJONf4dxdxeo"),
               path = here("data", "raw_data", 
                           "SSB050KM.zip"))
# Unzip SSB Grids
unzip(here("data", "raw_data", "SSB050KM.zip"),
      exdir = here("data", "raw_data"))

# Cleaned occurrences
drive_download(as_id(""),
               path = here("data", "derived_data", ""))

## 1.2. Read in data -----------------------------------------------------------

# CORINE Status Layers
norway_corine_status_modified_stack <- rast(here("data", "derived_data",
                                                 "norway_corine_status_modified_stack.tif"))

# SSB Grid
ssb_grids <- vect(here("data", "raw_data",
                       "SSB050KM", "ssb50km.shp"))

# Cleaned occurrence records
load(here("data", "derived_data","cleaned_occurrences_feb17_25.rda"))
occurrences_norway <- cleaned_occurrences_feb17_25

# 2. ADD GBIF OCCURRENCE RECORDS -----------------------------------------------

## 2.1. Re-project CORINE, SSB grid and municipalities to match occurrences ----

# Check projections
crs(norway_corine_status_modified_stack, proj = TRUE) 
  #"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(ssb_grids, proj = TRUE) #"+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
crs(norway_municipalities, proj = TRUE) # "+proj=longlat +datum=WGS84 +no_defs"

# Re-project SSB grids to match the projection of occurrences
norway_ssb_grids <- terra::project(ssb_grids,
                                   "+proj=longlat +datum=WGS84 +no_defs")

# Re-project CORINE STATUS to match the projection of occurrences
corine_status_wgs84 <- project(norway_corine_status_modified_stack, 
                        "+proj=longlat +datum=WGS84 +no_defs", method = "near")

# Re-project Norway municipalities
norway_municipalities_wgs84 <- project(norway_municipalities, 
                                       "+proj=longlat +datum=WGS84 +no_defs")

# Check that projections match
if (crs(norway_ssb_grids, proj = TRUE) ==  crs(corine_status_wgs84[[1]], proj = TRUE) 
    && crs(corine_status_wgs84[[1]], proj = TRUE) == 
    crs(norway_municipalities_wgs84, proj = TRUE)){
  print ("Projections correct")
} else {
  print ("Projections NOT correct")
}

# Check values for reprojected CORINE layer
levels(as.factor(as.data.frame(corine_status_wgs84[[1]])$U2006_CLC2000_V2020_20u1))

## 2.2. Convert occurrences to spatial object ----------------------------------

# Convert occurrences to spatial dataframe 
occurrences_sp <- st_as_sf(occurrences_norway, 
                           coords=c("decimalLongitude","decimalLatitude"),
                           crs=crs(corine_status_wgs84))

# Convert occurrences to spatial vector
occurrences_vect <- vect(occurrences_sp)

## 2.3. Extract SSB ID for occurrences -----------------------------------------
occurrenes_SSB <- terra::intersect(occurrences_vect, norway_ssb_grids)

## 2.4. Extract municipality for occurrences -----------------------------------
occurrences_municipalities <- terra::intersect(occurrenes_SSB,
                                               norway_municipalities_wgs84)

## 2.5. Create layer with a unique cell ID for each CORINE cell ----------------

#Create an empty raster with the same details as the first CORINE STATUS layer
ID_raster <- corine_status_wgs84[[1]]

# Assign each cell a unique number
values(ID_raster) <- 1:ncell(corine_status_wgs84[[1]])

# Change name of layer in ID_raster to avoid replication of the ones in CORINE
ID_raster <- ID_raster |>
  rename(cell_ID = U2006_CLC2000_V2020_20u1)

# Combine the ID raster with the CORINE STATUS raster
corine_ID <- c(corine_status_wgs84, ID_raster)

## 2.6. Extract land cover cell values for each occurrence record --------------

# Extract raster values for occurrences into dataframe
corine_status_occurrences_df <- terra::extract(corine_ID, occurrences_municipalities,
                                            df = TRUE)
# Save dataframes to file
save(corine_status_occurrences_df, file = here::here("data", "derived_data",
                                          "CORINE_Status_SSB_municipalities_occ_df.rda"))

## 2.7. Add extracted values to the occurrences --------------------------------

# Convert SpatVector to dataframe
occurrence_municipalities_df <- as.data.frame(occurrences_municipalities)

# Change names in corine_status_occurrences_df
corine_status_occurrences_df <- corine_status_occurrences_df |>
  mutate(land_cover2000 = U2006_CLC2000_V2020_20u1,
         land_cover2006 = U2012_CLC2006_V2020_20u1,
         land_cover2012 = U2018_CLC2012_V2020_20u1,
         land_cover2018 = U2018_CLC2018_V2020_20u1)

# Add columns from the dataframe with the extracted values
occurrence_municipalities_df <- bind_cols(occurrence_municipalities_df, 
                               select(corine_status_occurrences_df, 2:6))

# Check column names
colnames(occurrence_municipalities_df)

# Save to file
# save(occurrence_municipalities_df, file = here::here("data", "derived_data",
#                                           "occurrences_SSB_municipalities_land_cover.rda"))

save(occurrence_municipalities_df, 
     file = here::here("data", "derived_data",
                       "occurrences_SSB_municipalities_land_cover_extra_info_Jan2025.rda"))

# END OF SCRIPT ----------------------------------------------------------------