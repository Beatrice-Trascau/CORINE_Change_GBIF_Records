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
                 "plotly", "lme4", "DHARMa", "glmmTMB", "mgcv",
                 "tidyterra", "ggspatial", "htmlwidgets",
                 "htmltools", "patchwork", "webshot2",
                 "rgbif", "CoordinateCleaner", "DHARMa",
                 "writexl", "bbmle", "kableExtra") # specify packages

# Execute the function
sapply(package_vec, install_load_package)

# 2. FUNCTION TO ONLY DOWNLOAD FILES THAT ARE NOT ALREADY IN THE FOLDERS -------

download_files <- function(urls, filenames, dir = here("data", "raw_data")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  for (i in seq_along(urls)) {
    file_path <- file.path(dir, filenames[i])
    if (!file.exists(file_path)) {
      download.file(urls[i], file_path)
    }
  }
}

# 3. FUNCTION TO READ RASTERS --------------------------------------------------

read_rasters <- function(filenames, dir = here("data/raw_data")) {
  rasters <- lapply(filenames, function(x) {
    file_path <- file.path(dir, x)
    if (!file.exists(file_path)) {
      stop(paste("File does not exist:", file_path))
    }
    rast(file_path)
  })
  return(do.call(c, rasters))
}

# 4. FUNCTION TO CROP AND MASK RASTERS TO NORWAY -------------------------------

crop_mask_to_norway <- function(raster_stack, norway_shape) {
  return(crop(raster_stack, norway_shape, mask = TRUE))
}

# 5. FUNCTION TO MODIFY CLASSES IN RASTERS -------------------------------------

modify_class_values <- function(raster_stack, class_modifications) {
  modified_stack <- raster_stack
  for (mod in class_modifications) {
    modified_stack <- app(modified_stack, fun = function(x) {
      x[x %in% mod$from] <- mod$to
      return(x)
    })
  }
  return(modified_stack)
}

# 6. FUNCTION TO CALCULATE FREQUENCY OF LAND COVER CHANGES BETWEEN LAYERS ------

process_corine_change <- function(stack, index1, index2, source_year, target_year) {
  index1 <- as.numeric(index1)
  index2 <- as.numeric(index2)
  
  diff_raster <- stack[[index1]] - stack[[index2]]
  freq_df <- as.data.frame(freq(diff_raster))
  
  freq_df %>%
    mutate(source_year = source_year,
           target_year = target_year,
           difference = count) %>%
    select(-layer)
}

# 7. FUNCTION TO REMOVE YEAR FROM SANKEY LABELS AND ADD A HEADER ---------------

# Create function to remove the year part from labels and keep colours consitent
customize_sankey <- function(sankey, label_text) {
  htmlwidgets::onRender(sankey, sprintf('
    function(el, x) {
      var svg = d3.select(el).select("svg");

      // Add custom label
      svg.append("text")
        .attr("x", 10)
        .attr("y", 20)
        .attr("text-anchor", "start")
        .style("font-size", "20px")
        .style("font-weight", "bold")
        .text("%s");

      // Remove year from the node labels
      svg.selectAll(".node text").each(function(d) {
        var parts = d.name.split("_");
        var cover = parts.slice(1).join(" ").replace(/_/g, " ");
        d3.select(this).text(cover);
      });
    }
  ', label_text))
}

# 8. FUNCTION TO EXTRACT MODEL SUMMARIES TO DATAFRAME --------------------------
extract_summary_as_df <- function(model) {
  model_summary <- summary(model)
  coefs <- as.data.frame(model_summary$coefficients$cond)  
  coefs <- coefs |> rownames_to_column(var = "term")  
  return(coefs)
}

# 9. FUNCTION TO EXTRACT TABLE WITH KM2 FOR EACH TRANSITION --------------------
create_transition_table <- function(data) {
  data |>
    # Convert pixel counts to km² (each pixel is 0.01 km²)
    mutate(Area_km2 = count * 0.01) |>
    # Select & rename columns
    select(
      `Initial Land Cover`    = source_name,
      `Land Cover Changed To` = target_name,
      Year                    = source_year,
      `Quantity (km²)`        = Area_km2) |>
    # Sum area by transition and year
    group_by(`Initial Land Cover`, `Land Cover Changed To`, Year) |>
    summarize(`Quantity (km²)` = sum(`Quantity (km²)`), .groups = "drop") |>
    # Exclude rows with no actual change
    filter(`Initial Land Cover` != `Land Cover Changed To`)
}

# END OF SCRIPT ----------------------------------------------------------------