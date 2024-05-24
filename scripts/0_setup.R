install.load.package <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  require(x, character.only = TRUE)
}

package_vec <- c("here","dplyr","knitr",
                 "ggplot2","rgbif",
                 "here", "CoordinateCleaner",
                 "terra", "sf","mapview",
                 "PointedSDMs","stringr",
                 "stars", "geodata") # specify packages

## executing install & load for each package
sapply(package_vec, install.load.package)

# INLA is not on CRAN, so has to be installed separatly
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 
library("INLA")


conditional_download <- function(url, target) {
  if (!file.exists(target)) {
    download.file(url=url, destfile=target)
  }
  else {
    print("File already downloaded!")
  }
}