#' Run the sistaR.sdm Shiny Application
#'
#' @param data.paths Optional list of custom data paths (kept for backward compatibility)
#' @param launch.browser Logical, whether to launch in browser
#' @export
sistaR.sdm <- function(data.paths = NULL, launch.browser = TRUE) {

  # Store custom data paths in options if provided (for backward compatibility)
  if (!is.null(data.paths)) {
    options(sistaR.sdm.data_paths = data.paths)
  }

  # Get the app directory
  app_dir <- system.file("app", package = "sistaR.sdm")

  if (app_dir == "") {
    stop("Could not find app directory. Please reinstall the package.", call. = FALSE)
  }

  # Run the app
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = launch.browser)
}
