##----------------------------------------------------------------------------##
# PAPER 1: CORINE LAND COVER CHANGES AND GBIF BIODIVERSITY RECORDS
# 0_setup
# This script contains code which loads/installs necessary packages
##----------------------------------------------------------------------------##

# Function to load/install packages needed for analysis
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

package_vec <- c("here", "terra", "sf", "geodata", "mapview",
                 "tidyverse", "dplyr", "ggplot2", "ggalluvial",
                 "networkD3", "gt", "cowplot", "data.table",
                 "tidyterra", "patchwork", "styler", "scales",
                 "plotly", "lme4", "DHARMa") # specify packages

## executing install & load for each package
sapply(package_vec, install.load.package)


# Function to only dowload files that are not already in the folders
conditional_download <- function(url, target) {
  if (!file.exists(target)) {
    download.file(url=url, destfile=target)
  }
  else {
    print("File already downloaded!")
  }
}