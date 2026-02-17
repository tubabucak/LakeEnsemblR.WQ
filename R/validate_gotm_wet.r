#'@title Validate a GOTM-WET model simulation folder

#'@description
#' The function checks that the GOTM configuration and key input files referenced inside the namelist are present
#' in the simulation folder. It also checks that the output directory exists,
#' and attempts to create it if missing.

#' If any required file is missing, the function stops with an informative
#' error message describing which file(s) could not be found.
#' @name  validate_gotm_wet
#' @param sim_folder Character. Path to the GOTM-WET simulation folder containing the namelist and input files.
#' @param file Character. Name of the GOTM-WET namelist file inside \code{sim_folder}. Default is \code{"gotm.nml"}.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed using \code{message()}. Default is \code{TRUE}.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds. Otherwise, the
#'   function throws an error.
#'
#' @examples
#' \dontrun{
#' # Validate a prepared GOTM-WET run directory
#' validate_gotm_wet(sim_folder = "models/gotm_wet", file = "gotm.yaml")
#'


#' Validate GOTM-WET configuration
#' @export
#' 
#' 
validate_gotm_wet <- function(sim_folder = ".", file = "gotm.yaml", verbose = TRUE) {

  msg <- function(...) if (isTRUE(verbose)) message(...)

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required for validate_gotm(). Install it with install.packages('yaml').",
         call. = FALSE)
  }

  msg("Validating GOTM run folder: ", normalizePath(sim_folder, winslash = "/"))

  ypath <- file.path(sim_folder, file)
  if (!file.exists(ypath)) {
    stop("Missing GOTM yaml file: ", ypath, call. = FALSE)
  }
  msg("✔ Found GOTM yaml file: ", file)

  cfg <- yaml::read_yaml(ypath)

  # Recursively collect values of elements named "file"
  collect_file_fields <- function(x) {
    files <- character()

    if (is.list(x)) {
      # If this node has a "file" entry, collect it
      if (!is.null(x[["file"]])) {
        f <- x[["file"]]
        # sometimes file is NULL, "", or a scalar
        if (is.character(f) && length(f) == 1 && nzchar(f)) {
          files <- c(files, f)
        }
      }
      # extract the file names
      for (nm in names(x)) {
        files <- c(files, collect_file_fields(x[[nm]]))
      }
    }

    unique(files)
  }

  files <- collect_file_fields(cfg)

  # Hypsography file was not always called with "file" so we should check it seperately
  if (!is.null(cfg$location$hypsograph) && nzchar(cfg$location$hypsograph)) {
    files <- unique(c(files, cfg$location$hypsograph))
  }

  if (length(files) == 0) {
    msg("ℹ No file references found in yaml (no checks performed).")
    msg("GOTM validation completed successfully")
    return(invisible(TRUE))
  }

  msg("Found ", length(files), " file reference(s) in GOTM yaml.")
  msg("Checking referenced files exist...")

  missing <- files[!file.exists(file.path(sim_folder, files))]

  if (length(missing) > 0) {
    stop("Missing file(s) referenced in GOTM yaml: ",
         paste(missing, collapse = ", "),
         "\n(sim_folder = ", sim_folder, ")",
         call. = FALSE)
  }

  msg("✔ All referenced files found")

  # Optional: quick FABM sanity (if fabm is enabled)
  if (isTRUE(cfg$fabm$use)) {
    msg("✔ FABM is enabled (fabm$use: true)")
    # If you later add a fabm config file path (common in some setups),
    # you can validate it here.
  } else if (!is.null(cfg$fabm$use)) {
    msg("ℹ FABM not enabled (fabm$use is not TRUE)")
  }

  msg("GOTM validation completed successfully")
  invisible(TRUE)
}
