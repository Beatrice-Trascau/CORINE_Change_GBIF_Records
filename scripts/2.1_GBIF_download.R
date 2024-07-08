##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 2.1_GBIF_download
# This script contains code which downloads the GBIF Occurrence records used in
# the analysis
##----------------------------------------------------------------------------##

# 1. CONNECT TO GBIF -----------------------------------------------------------

# Setting username, password and email in my .Renviron. 
usethis::edit_r_environ()
# GBIF_USER="my username"
# GBIF_PWD="my password"
# GBIF_EMAIL="my email"

# 2. CREATE DOWNLOAD REQUEST ---------------------------------------------------

# Send download request
download_key <- occ_download(
  pred("gadm", "NOR"), # Norway
  pred_gte("year", 1997), # Greater than or equal to year 1950
  pred_lte("year", 2018), # Lower than or equal to 2018
  pred("hasCoordinate", TRUE), 
  format = "DWCA") # Download as a Darwin Core Archive file

# Check progress
occ_download_wait(download_key)

# Download key: 
# Download link: DOI: 10.15468/dl.xp6v9x

# 3. IMPORT GBIF DOWNLOAD AND SAVE ---------------------------------------------

# Import data
occurrence <- occ_download_get(download_key) %>%
  occ_download_import()

# Save to file
save(occurrence, file = here::here("data","raw_data","occurrence.txt"))