##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 0_setup
# This script contains code which loads/installs necessary packages and defines
# functions used in the analysis
##----------------------------------------------------------------------------##

# 1. FUNCTION TO LOAD/INSTALL PACKAGES NEEDED FOR ANALYIS ----------------------

# Define function
install_load_package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

# Define list of packages
package_vec <- c("here", "terra", "sf", "geodata", "mapview",
                 "tidyverse", "dplyr", "ggplot2", "ggalluvial",
                 "networkD3", "gt", "cowplot", "data.table",
                 "tidyterra", "patchwork", "styler", "scales",
                 "plotly", "lme4", "DHARMa", "glmmTMB", 
                 "tidyterra") # specify packages

# Execute the function
sapply(package_vec, install_load_package)

# 2. FUNCTION TO ONLY DOWNLOAD FILES THAT ARE NOT ALREADY IN THE FOLDERS -------

conditional_download <- function(url, target) {
  if (!file.exists(target)) {
    download.file(url=url, destfile=target)
  }
  else {
    print("File already downloaded!")
  }
}