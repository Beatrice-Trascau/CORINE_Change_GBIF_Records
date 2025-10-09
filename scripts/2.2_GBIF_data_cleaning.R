##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.2_GBIF_data_cleaning
# This script contains code which cleans the downloaded GBIF occurrence records
##----------------------------------------------------------------------------##

# 1. LOAD DATA -----------------------------------------------------------------

# Source setup file
library(here)
source(here("scripts", "0_setup.R"))

# Download the occurrences if needed
# occurrence <- occ_download_get("0009294-250802193616735") |>
#   occ_download_import()

# Load data
load(here("data", "raw_data", "occurrence_redownload_August2025.txt"))

# 2. CLEAN RECORDS -------------------------------------------------------------

# Original number of occurrences in raw dataframe: 
nrow(occurrence) # 29 471 774

# 2.1. Remove material citations, fossil specimens, and living specimens -------

# Remove unwanted records
clean_occurrences1 <- occurrence |>
  filter(!basisOfRecord %in% c("MATERIAL_CITATION", "FOSSIL_SPECIMEN",
                               "LIVING_SPECIMEN"))

# Occurrences left after material citations, fossil specimens and living specimens removed
nrow(clean_occurrences1) # 29 380 790

# 2.2. Remove records that are not Animalia, Plantae or Fungi ------------------

# Keep only animals, plants & fungi
clean_occurrences2 <- clean_occurrences1 |>
  filter(kingdom %in% c("Animalia", "Plantae", "Fungi"))

# Check how many records are left
nrow(clean_occurrences2) # 29 129 192

# 2.3. Remove records with no registered species-level information -------------

# Remove records without species-level information
clean_occurrences3 <- clean_occurrences2 |>
  filter(specificEpithet != "")

# Check how many records are left
nrow(clean_occurrences3) # 28 269 295

# 2.4. Remove duplicate records ------------------------------------------------

# Keep only unique records
clean_occurrences4 <- clean_occurrences3 |>
  distinct()

# Check how many records are left
nrow(clean_occurrences4) # 28 269 295 - there were no duplicated records!

# 2.5. Remove flagged records --------------------------------------------------

# Identify flagged records
coordinate_flags <- clean_coordinates(x = clean_occurrences4,
                                      lon = "decimalLongitude",
                                      lat = "decimalLatitude",
                                      species = "species",
                                      test = c("equal", "gbif", "zeros"))

# Get a summary of the records
summary(coordinate_flags) # no flagged records

# 2.5. Remove records with specific coordinate uncertainty ---------------------

# Calculate diagonal of 100m x 100m grid cell
sqrt(100*100)

# Remove records with coord uncertainty >100m
clean_occurrences5 <- clean_occurrences4 |>
  filter(coordinateUncertaintyInMeters < 100 |
           is.na(coordinateUncertaintyInMeters))

# Check how many records are left 
nrow(clean_occurrences5) # 10 546 354

# 3. PREP DF FOR ANALYSIS ------------------------------------------------------

# Remove unnecessary columns
clean_occurrences <- clean_occurrences5 |>
  select(gbifID, identifiedBy, basisOfRecord, occurrenceStatus,
         eventDate, year, countryCode, stateProvince, county, municipality,
         locality, decimalLatitude, decimalLongitude, 
         coordinateUncertaintyInMeters, kingdom, phylum, class, order, family,
         genus, specificEpithet, speciesKey, species, organismQuantity,
         occurrenceStatus, scientificName, publisher, institutionID, collectionID,
         datasetID, institutionCode, datasetName, ownerInstitutionCode, recordedByID)

# Save cleaned occurrences
save(clean_occurrences, file = here::here("data", "derived_data",
                                          "clened_occurrences_redownloaded_August2025.rda"))

# END OF SCRIPT ----------------------------------------------------------------