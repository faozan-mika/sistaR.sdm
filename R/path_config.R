#' Create a Path Configuration File
#'
#' @description
#' Creates a template configuration file for users to edit with their paths.
#'
#' @param filepath Path where the configuration file should be saved.
#'   If NULL, saves to current working directory.
#'
#' @return Invisibly returns the filepath
#' @export
#'
#' @examples
#' \dontrun{
#' create_path_config()
#' }
create_path_config <- function(filepath = NULL) {
  if (is.null(filepath)) {
    filepath <- file.path(getwd(), "sdm_paths_config.R")
  }

  config_content <- '
# ============================================
# sistaR.sdm - Data Path Configuration File
# ============================================
# Edit the paths below to point to your data files
# Use forward slashes (/) or double backslashes (\\\\) for Windows paths

# Climate data directory (should contain bio_1.tif through bio_19.tif)
climate_path <- "F:/Rstudio/latihan/species distribution model by weecology/climate/wc2.1_10m"

# Elevation file
elevation_path <- "F:/Rstudio/latihan/species distribution model tambahan LAND USE/wc2.1_10m_elev.tif"

# Soil data
soil_raster_path <- "F:/Rstudio/latihan/species distribution model tambahan LAND USE/HWSD2_RASTER/HWSD2.tif"
soil_db_path <- "F:/Rstudio/latihan/species distribution model tambahan LAND USE/HWSD2_DB/HWSD2.sqlite"

# Solar radiation directory
srad_path <- "F:/Rstudio/latihan/species distribution model by weecology/wc2.1_10m_srad"

# Wind speed directory
wind_path <- "F:/Rstudio/latihan/species distribution model by weecology/wc2.1_10m_wind"

# Vapor pressure directory
vapr_path <- "F:/Rstudio/latihan/species distribution model by weecology/wc2.1_10m_vapr"

# ============================================
# After editing, load with:
# source("sdm_paths_config.R")
# set_sdm_paths(list(
#   climate = climate_path,
#   elevation = elevation_path,
#   soil_raster = soil_raster_path,
#   soil_db = soil_db_path,
#   srad = srad_path,
#   wind = wind_path,
#   vapr = vapr_path
# ))
'

  writeLines(config_content, filepath)
  message("Configuration file created at: ", filepath)
  message("\nEdit this file with your actual data paths, then source it and run set_sdm_paths()")

  invisible(filepath)
}
