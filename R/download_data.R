#' Download environmental data for sistaR.sdm from Zenodo
#'
#' @description
#' This function downloads the required environmental data files from Zenodo.
#' Users need to run this once before using the app. The data includes climate,
#' elevation, soil, solar radiation, wind speed, vapor pressure, and future climate layers.
#'
#' @param data_dir Directory to download data to. If NULL, uses a platform-specific
#'   default location (recommended).
#' @param force Logical. If TRUE, re-download files even if they already exist.
#'   Default is FALSE.
#'
#' @return Invisibly returns the path to the data directory.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download all data to default location
#' download_sdm_data()
#'
#' # Download to custom location
#' download_sdm_data(data_dir = "~/my_sdm_data")
#'
#' # Force re-download even if files exist
#' download_sdm_data(force = TRUE)
#' }
# ============================================================================
# HELPER FUNCTION (internal - not exported)
# ============================================================================

#' Download a single file with retry logic
#' @keywords internal
download_single_file <- function(filename, dest_file, base_url, file_sizes, force = FALSE) {

  # Check if file exists and is valid
  file_valid <- FALSE
  if (file.exists(dest_file) && !force) {
    if (grepl("\\.tif$", filename)) {
      tryCatch({
        terra::describe(dest_file)
        file_valid <- TRUE
        message("  ✓ Already exists and is valid: ", filename)
        return(TRUE)
      }, error = function(e) {
        message("  File exists but appears corrupted: ", filename)
        file_valid <- FALSE
      })
    } else {
      file_valid <- TRUE
      message("  ✓ Already exists: ", filename)
      return(TRUE)
    }
  }

  # Construct download URL
  file_url <- paste0(base_url, filename, "?download=1")

  message("  Downloading: ", filename)

  # Enhanced retry mechanism
  max_retries <- 15
  download_success <- FALSE

  for (attempt in 1:max_retries) {
    # Delete any partial file from previous attempt
    if (file.exists(dest_file)) {
      file.remove(dest_file)
    }

    tryCatch({
      # Use curl if available for better large file handling
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(
          url = file_url,
          destfile = dest_file,
          quiet = FALSE,
          mode = "wb"
        )
      } else {
        download.file(
          url = file_url,
          destfile = dest_file,
          mode = "wb",
          quiet = FALSE
        )
      }

      # Verify download for known files
      download_ok <- TRUE
      if (filename %in% names(file_sizes)) {
        expected_size <- file_sizes[[filename]]
        actual_size <- file.size(dest_file)

        # Allow 1% tolerance
        size_ratio <- actual_size / expected_size
        if (size_ratio < 0.99 || size_ratio > 1.01) {
          message(sprintf("     Size mismatch: got %.1f MB, expected %.1f MB",
                          actual_size / 1024^2, expected_size / 1024^2))
          download_ok <- FALSE
        } else {
          message(sprintf("     Size verification passed: %.1f MB", actual_size / 1024^2))
        }
      }

      # Verify .tif files can be read
      if (download_ok && grepl("\\.tif$", filename)) {
        terra::describe(dest_file)
      }

      if (download_ok) {
        download_success <- TRUE
        message("     ✅ Success")
        break
      }

    }, error = function(e) {
      # Clean up failed download
      if (file.exists(dest_file)) {
        file.remove(dest_file)
      }

      if (attempt < max_retries) {
        # Exponential backoff with jitter: 2, 4, 8, 16, 30, 30, 30...
        wait_time <- min(30, 2^attempt) + runif(1, 0, 2)
        message(sprintf("     Attempt %d/%d failed, retrying in %.0f seconds...",
                        attempt, max_retries, wait_time))
        Sys.sleep(wait_time)
      }
    })
  }

  if (!download_success) {
    warning("Failed to download: ", filename, " after ", max_retries, " attempts")
    return(FALSE)
  }

  return(TRUE)
}


# ============================================================================
# MAIN DOWNLOAD FUNCTION (downloads everything)
# ============================================================================

#' Download ALL environmental data for sistaR.sdm from Zenodo
#'
#' @description
#' This function downloads ALL required environmental data files from Zenodo.
#' This includes climate, elevation, soil, solar radiation, wind speed,
#' vapor pressure, and future climate layers.
#'
#' @param data_dir Directory to download data to. If NULL, uses a platform-specific
#'   default location (recommended).
#' @param force Logical. If TRUE, re-download files even if they already exist.
#'   Default is FALSE.
#'
#' @return Invisibly returns the path to the data directory.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download all data to default location
#' download_sdm_data()
#'
#' # Force re-download even if files exist
#' download_sdm_data(force = TRUE)
#' }
download_sdm_data <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set higher timeout for downloads (10 minutes = 600 seconds)
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n==========================================")
  message("sistaR.sdm - Download ALL Environmental Data")
  message("==========================================")
  message("Destination: ", data_dir)
  message("\nThis will download ALL data components:\n")
  message("  • Climate (19 files)")
  message("  • Elevation (1 file)")
  message("  • Soil (2 files)")
  message("  • Solar radiation (12 files)")
  message("  • Wind speed (12 files)")
  message("  • Vapor pressure (12 files)")
  message("  • Future climate (32 files)")
  message("\nTotal: ~250MB\n")

  # Create all directories
  dir.create(file.path(data_dir, "climate"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "elevation"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "soil", "HWSD2_RASTER"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "soil", "HWSD2_DB"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "srad"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "wind"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "vapr"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "future"), recursive = TRUE, showWarnings = FALSE)

  # Download each component
  results <- list()

  results$climate <- download_sdm_climate(data_dir = data_dir, force = force)
  results$elevation <- download_sdm_elevation(data_dir = data_dir, force = force)
  results$soil <- download_sdm_soil(data_dir = data_dir, force = force)
  results$srad <- download_sdm_srad(data_dir = data_dir, force = force)
  results$wind <- download_sdm_wind(data_dir = data_dir, force = force)
  results$vapr <- download_sdm_vapr(data_dir = data_dir, force = force)
  results$future <- download_sdm_future(data_dir = data_dir, force = force)

  # Save the data directory path
  options(sistaR.sdm.data_dir = data_dir)

  message("\n==========================================")
  message("Download complete!")
  message("Data saved to: ", data_dir)

  # Show summary
  failed <- sum(!unlist(results))
  if (failed > 0) {
    message("⚠️  ", failed, " components failed. You can retry them individually:")
    if (!results$climate) message("   • download_sdm_climate()")
    if (!results$elevation) message("   • download_sdm_elevation()")
    if (!results$soil) message("   • download_sdm_soil()")
    if (!results$srad) message("   • download_sdm_srad()")
    if (!results$wind) message("   • download_sdm_wind()")
    if (!results$vapr) message("   • download_sdm_vapr()")
    if (!results$future) message("   • download_sdm_future()")
  } else {
    message("✅ All components downloaded successfully!")
  }
  message("==========================================\n")

  invisible(data_dir)
}


# ============================================================================
# INDIVIDUAL COMPONENT FUNCTIONS
# ============================================================================

#' Download climate data (19 bioclimatic variables)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_climate()
#' }
download_sdm_climate <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading CLIMATE data (19 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "climate"), recursive = TRUE, showWarnings = FALSE)

  # File sizes (optional)
  file_sizes <- list()

  # Download files
  files <- paste0("wc2.1_10m_bio_", 1:19, ".tif")
  success <- TRUE

  for (filename in files) {
    dest_file <- file.path(data_dir, "climate", filename)
    ok <- download_single_file(filename, dest_file, base_url, file_sizes, force)
    success <- success && ok
  }

  if (success) {
    message("✅ Climate data downloaded successfully!")
  } else {
    message("❌ Some climate files failed. Try again with: download_sdm_climate(force = TRUE)")
  }

  return(success)
}


#' Download elevation data
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_elevation()
#' }
download_sdm_elevation <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading ELEVATION data ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "elevation"), recursive = TRUE, showWarnings = FALSE)

  # File sizes (optional)
  file_sizes <- list()

  # Download file
  filename <- "wc2.1_10m_elev.tif"
  dest_file <- file.path(data_dir, "elevation", filename)
  success <- download_single_file(filename, dest_file, base_url, file_sizes, force)

  if (success) {
    message("✅ Elevation data downloaded successfully!")
  } else {
    message("❌ Elevation download failed. Try again with: download_sdm_elevation(force = TRUE)")
  }

  return(success)
}


#' Download soil data (HWSD2.tif and HWSD2.sqlite)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_soil()
#' }
download_sdm_soil <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading SOIL data (2 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directories
  dir.create(file.path(data_dir, "soil", "HWSD2_RASTER"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "soil", "HWSD2_DB"), recursive = TRUE, showWarnings = FALSE)

  # File sizes for verification
  file_sizes <- list(
    "HWSD2.tif" = 94537726  # 90.2 MB
  )

  # Download HWSD2.tif
  filename1 <- "HWSD2.tif"
  dest_file1 <- file.path(data_dir, "soil", "HWSD2_RASTER", filename1)
  success1 <- download_single_file(filename1, dest_file1, base_url, file_sizes, force)

  # Download HWSD2.sqlite
  filename2 <- "HWSD2.sqlite"
  dest_file2 <- file.path(data_dir, "soil", "HWSD2_DB", filename2)
  success2 <- download_single_file(filename2, dest_file2, base_url, list(), force)

  success <- success1 && success2

  if (success) {
    message("✅ Soil data downloaded successfully!")
  } else {
    message("❌ Some soil files failed. Try again with: download_sdm_soil(force = TRUE)")
  }

  return(success)
}


#' Download solar radiation data (12 monthly files -> 6 indices)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_srad()
#' }
download_sdm_srad <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading SOLAR RADIATION data (12 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "srad"), recursive = TRUE, showWarnings = FALSE)

  # File sizes (optional)
  file_sizes <- list()

  # Download files
  files <- paste0("wc2.1_10m_srad_", sprintf("%02d", 1:12), ".tif")
  success <- TRUE

  for (filename in files) {
    dest_file <- file.path(data_dir, "srad", filename)
    ok <- download_single_file(filename, dest_file, base_url, file_sizes, force)
    success <- success && ok
  }

  if (success) {
    message("✅ Solar radiation data downloaded successfully!")
  } else {
    message("❌ Some solar radiation files failed. Try again with: download_sdm_srad(force = TRUE)")
  }

  return(success)
}


#' Download wind speed data (12 monthly files -> 6 indices)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_wind()
#' }
download_sdm_wind <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading WIND SPEED data (12 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "wind"), recursive = TRUE, showWarnings = FALSE)

  # File sizes (optional)
  file_sizes <- list()

  # Download files
  files <- paste0("wc2.1_10m_wind_", sprintf("%02d", 1:12), ".tif")
  success <- TRUE

  for (filename in files) {
    dest_file <- file.path(data_dir, "wind", filename)
    ok <- download_single_file(filename, dest_file, base_url, file_sizes, force)
    success <- success && ok
  }

  if (success) {
    message("✅ Wind speed data downloaded successfully!")
  } else {
    message("❌ Some wind speed files failed. Try again with: download_sdm_wind(force = TRUE)")
  }

  return(success)
}


#' Download vapor pressure data (12 monthly files -> 6 indices)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_vapr()
#' }
download_sdm_vapr <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading VAPOR PRESSURE data (12 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "vapr"), recursive = TRUE, showWarnings = FALSE)

  # File sizes (optional)
  file_sizes <- list()

  # Download files
  files <- paste0("wc2.1_10m_vapr_", sprintf("%02d", 1:12), ".tif")
  success <- TRUE

  for (filename in files) {
    dest_file <- file.path(data_dir, "vapr", filename)
    ok <- download_single_file(filename, dest_file, base_url, file_sizes, force)
    success <- success && ok
  }

  if (success) {
    message("✅ Vapor pressure data downloaded successfully!")
  } else {
    message("❌ Some vapor pressure files failed. Try again with: download_sdm_vapr(force = TRUE)")
  }

  return(success)
}


#' Download future climate data (32 files: 2 models × 4 scenarios × 4 periods)
#'
#' @param data_dir Directory to download data to. If NULL, uses the default.
#' @param force Logical. If TRUE, re-download even if files exist.
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' download_sdm_future()
#' }
download_sdm_future <- function(data_dir = NULL, force = FALSE) {

  if (is.null(data_dir)) {
    data_dir <- tools::R_user_dir("sistaR.sdm", which = "data")
  }

  # Set timeout
  original_timeout <- getOption("timeout")
  options(timeout = 1800)
  on.exit(options(timeout = original_timeout), add = TRUE)

  message("\n--- Downloading FUTURE CLIMATE data (32 files) ---")

  zenodo_record <- "18779062"
  base_url <- paste0("https://zenodo.org/records/", zenodo_record, "/files/")

  # Create directory
  dir.create(file.path(data_dir, "future"), recursive = TRUE, showWarnings = FALSE)

  # Generate all future filenames
  future_combinations <- expand.grid(
    model = c("ACCESS-CM2", "MIROC6"),
    scenario = c("ssp126", "ssp245", "ssp370", "ssp585"),
    period = c("2021-2040", "2041-2060", "2061-2080", "2081-2100"),
    stringsAsFactors = FALSE
  )

  files <- paste0("wc2.1_10m_bioc_",
                  future_combinations$model, "_",
                  future_combinations$scenario, "_",
                  future_combinations$period, ".tif")

  # File sizes (optional)
  file_sizes <- list()

  # Download files
  success <- TRUE
  for (filename in files) {
    dest_file <- file.path(data_dir, "future", filename)
    ok <- download_single_file(filename, dest_file, base_url, file_sizes, force)
    success <- success && ok
  }

  if (success) {
    message("✅ Future climate data downloaded successfully!")
  } else {
    message("❌ Some future climate files failed. Try again with: download_sdm_future(force = TRUE)")
  }

  return(success)
}


# ============================================================================
# CHECK FUNCTION
# ============================================================================

#' Check which environmental data components are available
#'
#' @param components Character vector specifying which components to check.
#'   Options: "all", "climate", "elevation", "soil", "srad", "wind", "vapr", "future".
#'   Default is "all".
#'
#' @return Invisibly returns a list with status of each component.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check all data
#' check_sdm_data()
#'
#' # Check only climate and soil
#' check_sdm_data(components = c("climate", "soil"))
#' }
check_sdm_data <- function(components = "all") {
  data_dir <- getOption("sistaR.sdm.data_dir",
                        default = tools::R_user_dir("sistaR.sdm", which = "data"))

  if (!dir.exists(data_dir)) {
    message("❌ Data directory not found: ", data_dir)
    message("   Please run download_sdm_data() or individual download functions")
    return(invisible(list()))
  }

  if ("all" %in% components) {
    components <- c("climate", "elevation", "soil", "srad", "wind", "vapr", "future")
  }

  status <- list()
  all_ok <- TRUE

  message("\n📊 Data Status:")

  # Check climate
  if ("climate" %in% components) {
    climate_files <- list.files(file.path(data_dir, "climate"), pattern = "\\.tif$")
    ok <- length(climate_files) >= 19
    status$climate <- ok
    message(sprintf("   %s Climate: %d/19 files", ifelse(ok, "✅", "❌"), length(climate_files)))
    all_ok <- all_ok && ok
  }

  # Check elevation
  if ("elevation" %in% components) {
    ok <- file.exists(file.path(data_dir, "elevation", "wc2.1_10m_elev.tif"))
    status$elevation <- ok
    message(sprintf("   %s Elevation", ifelse(ok, "✅", "❌")))
    all_ok <- all_ok && ok
  }

  # Check soil
  if ("soil" %in% components) {
    soil_raster <- file.path(data_dir, "soil", "HWSD2_RASTER", "HWSD2.tif")
    soil_db <- file.path(data_dir, "soil", "HWSD2_DB", "HWSD2.sqlite")
    raster_ok <- file.exists(soil_raster)
    db_ok <- file.exists(soil_db)
    ok <- raster_ok && db_ok
    status$soil <- ok
    message(sprintf("   %s Soil (raster: %s, db: %s)",
                    ifelse(ok, "✅", "❌"),
                    ifelse(raster_ok, "✅", "❌"),
                    ifelse(db_ok, "✅", "❌")))
    all_ok <- all_ok && ok
  }

  # Check solar radiation
  if ("srad" %in% components) {
    srad_files <- list.files(file.path(data_dir, "srad"), pattern = "\\.tif$")
    ok <- length(srad_files) >= 12
    status$srad <- ok
    message(sprintf("   %s Solar radiation: %d/12 files", ifelse(ok, "✅", "❌"), length(srad_files)))
    all_ok <- all_ok && ok
  }

  # Check wind
  if ("wind" %in% components) {
    wind_files <- list.files(file.path(data_dir, "wind"), pattern = "\\.tif$")
    ok <- length(wind_files) >= 12
    status$wind <- ok
    message(sprintf("   %s Wind speed: %d/12 files", ifelse(ok, "✅", "❌"), length(wind_files)))
    all_ok <- all_ok && ok
  }

  # Check vapor pressure
  if ("vapr" %in% components) {
    vapr_files <- list.files(file.path(data_dir, "vapr"), pattern = "\\.tif$")
    ok <- length(vapr_files) >= 12
    status$vapr <- ok
    message(sprintf("   %s Vapor pressure: %d/12 files", ifelse(ok, "✅", "❌"), length(vapr_files)))
    all_ok <- all_ok && ok
  }

  # Check future
  if ("future" %in% components) {
    future_files <- list.files(file.path(data_dir, "future"), pattern = "\\.tif$")
    ok <- length(future_files) >= 32
    status$future <- ok
    message(sprintf("   %s Future climate: %d/32 files", ifelse(ok, "✅", "❌"), length(future_files)))
    all_ok <- all_ok && ok
  }

  if (all_ok) {
    message("\n✅ All requested components are available!")
  } else {
    message("\n⚠️ Some components are missing or incomplete.")
    message("   You can download missing components with:")
    message('   • download_sdm_climate()')
    message('   • download_sdm_elevation()')
    message('   • download_sdm_soil()')
    message('   • download_sdm_srad()')
    message('   • download_sdm_wind()')
    message('   • download_sdm_vapr()')
    message('   • download_sdm_future()')
  }

  invisible(status)
}
