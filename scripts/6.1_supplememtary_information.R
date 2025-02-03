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

# Save frequency table
write.csv(freq_table, here("data", "derived_data",
                           "norway_2018_pixel_percentages.csv"), 
          row.names = FALSE)