#' Set Custom Data Paths for sistaR.sdm
#'
#' @description
#' Set or update custom paths to environmental data files.
#'
#' @param paths List containing paths to environmental data. Can include:
#'   \itemize{
#'     \item{climate}{ - Directory containing climate .tif files}
#'     \item{elevation}{ - Path to elevation .tif file}
#'     \item{soil_raster}{ - Path to HWSD2 .tif raster file}
#'     \item{soil_db}{ - Path to HWSD2 .sqlite database file}
#'     \item{srad}{ - Directory containing solar radiation .tif files}
#'     \item{wind}{ - Directory containing wind speed .tif files}
#'     \item{vapr}{ - Directory containing vapor pressure .tif files}
#'   }
#'
#' @return Invisibly returns the previously set paths
#' @export
#'
#' @examples
#' \dontrun{
#' set_sdm_paths(list(
#'   climate = "D:/SDM_data/climate/wc2.1_10m",
#'   elevation = "D:/SDM_data/elevation/wc2.1_10m_elev.tif",
#'   soil_raster = "D:/SDM_data/soil/HWSD2_RASTER/HWSD2.tif",
#'   soil_db = "D:/SDM_data/soil/HWSD2_DB/HWSD2.sqlite"
#' ))
#' }
set_sdm_paths <- function(paths) {
  old_paths <- getOption("sistaR.sdm.data_paths")

  # Validate paths
  validated_paths <- list()

  if (!is.null(paths$climate)) {
    if (dir.exists(paths$climate)) {
      validated_paths$climate <- paths$climate
    } else {
      warning("Climate directory does not exist: ", paths$climate)
    }
  }

  if (!is.null(paths$elevation)) {
    if (file.exists(paths$elevation)) {
      validated_paths$elevation <- paths$elevation
    } else {
      warning("Elevation file does not exist: ", paths$elevation)
    }
  }

  if (!is.null(paths$soil_raster)) {
    if (file.exists(paths$soil_raster)) {
      validated_paths$soil_raster <- paths$soil_raster
    } else {
      warning("Soil raster file does not exist: ", paths$soil_raster)
    }
  }

  if (!is.null(paths$soil_db)) {
    if (file.exists(paths$soil_db)) {
      validated_paths$soil_db <- paths$soil_db
    } else {
      warning("Soil database file does not exist: ", paths$soil_db)
    }
  }

  if (!is.null(paths$srad)) {
    if (dir.exists(paths$srad)) {
      validated_paths$srad <- paths$srad
    } else {
      warning("Solar radiation directory does not exist: ", paths$srad)
    }
  }

  if (!is.null(paths$wind)) {
    if (dir.exists(paths$wind)) {
      validated_paths$wind <- paths$wind
    } else {
      warning("Wind speed directory does not exist: ", paths$wind)
    }
  }

  if (!is.null(paths$vapr)) {
    if (dir.exists(paths$vapr)) {
      validated_paths$vapr <- paths$vapr
    } else {
      warning("Vapor pressure directory does not exist: ", paths$vapr)
    }
  }

  options(sistaR.sdm.data_paths = validated_paths)

  message("Data paths set successfully!")
  invisible(old_paths)
}

#' Get Current Data Paths
#'
#' @description
#' Retrieve the currently set data paths.
#'
#' @return List of current data paths
#' @export
get_sdm_paths <- function() {
  paths <- getOption("sistaR.sdm.data_paths", list())

  if (length(paths) == 0) {
    message("No custom paths set. Paths can be set with set_sdm_paths()")
  }

  return(paths)
}

#' Check if Paths are Valid
#'
#' @description
#' Check if all specified data paths exist.
#'
#' @param paths Optional list of paths to check. If NULL, checks currently set paths.
#'
#' @return Logical indicating if all paths are valid
#' @export
check_sdm_paths <- function(paths = NULL) {
  if (is.null(paths)) {
    paths <- get_sdm_paths()
  }

  if (length(paths) == 0) {
    message("No paths to check")
    return(FALSE)
  }

  valid <- TRUE

  if (!is.null(paths$climate) && !dir.exists(paths$climate)) {
    warning("Climate directory not found: ", paths$climate)
    valid <- FALSE
  }

  if (!is.null(paths$elevation) && !file.exists(paths$elevation)) {
    warning("Elevation file not found: ", paths$elevation)
    valid <- FALSE
  }

  if (!is.null(paths$soil_raster) && !file.exists(paths$soil_raster)) {
    warning("Soil raster file not found: ", paths$soil_raster)
    valid <- FALSE
  }

  if (!is.null(paths$soil_db) && !file.exists(paths$soil_db)) {
    warning("Soil database file not found: ", paths$soil_db)
    valid <- FALSE
  }

  if (!is.null(paths$srad) && !dir.exists(paths$srad)) {
    warning("Solar radiation directory not found: ", paths$srad)
    valid <- FALSE
  }

  if (!is.null(paths$wind) && !dir.exists(paths$wind)) {
    warning("Wind speed directory not found: ", paths$wind)
    valid <- FALSE
  }

  if (!is.null(paths$vapr) && !dir.exists(paths$vapr)) {
    warning("Vapor pressure directory not found: ", paths$vapr)
    valid <- FALSE
  }

  if (valid) {
    message("All specified paths are valid!")
  }

  return(valid)
}
