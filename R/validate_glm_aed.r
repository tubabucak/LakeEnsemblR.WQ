#'@title Validate a GLM-AED model simulation folder

#'@description
#' The function checks that the GLM namelist file
#' exists and that key input files referenced inside the namelist are present
#' in the simulation folder. It also checks that the output directory exists,
#' and attempts to create it if missing.

#' If any required file is missing, the function stops with an informative
#' error message describing which file(s) could not be found.
#' @name  validate_glm_aed 
#' @param sim_folder Character. Path to the GLM simulation folder containing the namelist and input files.
#' @param file Character. Name of the GLM namelist file inside \code{sim_folder}. Default is \code{"glm3.nml"}.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed using \code{message()}. Default is \code{TRUE}.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds. Otherwise, the
#'   function throws an error.
#'
#' @examples
#' \dontrun{
#' # Validate a prepared GLM run directory
#' validate_glm_aed(sim_folder = "runs/lake_001", file = "glm3.nml")
#'
#' # Quiet mode
#' validate_glm_aed(sim_folder = "runs/lake_001", verbose = FALSE)
#' }


#' @export


validate_glm_aed <- function(sim_folder = ".", file = "glm3.nml", verbose = TRUE) {
  
  msg <- function(...) if (isTRUE(verbose)) message(...)
  
  msg("Validating GLM run folder: ", normalizePath(sim_folder, winslash = "/"))
  
  nml_path <- file.path(sim_folder, file)
  if (!file.exists(nml_path)) {
    stop("Missing nml file: ", nml_path, call. = FALSE)
  }
  msg("✔ Found nml file: ", file)
  
  txt <- readLines(nml_path, warn = FALSE)
  
  # Helper function to check the file names
  get_quoted_vec <- function(key) {
    matches <- txt[grepl(paste0("^\\s*", key, "\\s*="), txt)]
    if (length(matches) == 0) return(character())
    
    quoted <- regmatches(matches[1], gregexpr("'[^']+'", matches[1]))[[1]]
    gsub("^'|'$", "", quoted)
  }
  
  check_files <- function(label, files) {
    if (length(files) == 0) {
      msg(" No files listed for ", label)
      return(invisible(TRUE))
    }
    
    msg("Checking ", label, ": ", paste(files, collapse = ", "))
    missing <- files[!file.exists(file.path(sim_folder, files))]
    
    if (length(missing) > 0) {
      stop("Missing file(s) referenced in nml for ", label, ": ",
           paste(missing, collapse = ", "),
           "\n(sim_folder = ", sim_folder, ")",
           call. = FALSE)
    }
    
    msg("✔ All ", label, " files found")
    invisible(TRUE)
  }
  
  meteo <- get_quoted_vec("meteo_fl")
  inflow <- get_quoted_vec("inflow_fl")
  outflow <- get_quoted_vec("outflow_fl")
  wq <- get_quoted_vec("wq_nml_file")
  out_dir <- get_quoted_vec("out_dir")
  
  check_files("meteo_fl", meteo)
  check_files("inflow_fl", inflow)
  check_files("outflow_fl", outflow)
  check_files("wq_nml_file", wq)
  
  # output directory
  if (length(out_dir) > 0) {
    od <- file.path(sim_folder, out_dir[1])
    if (!dir.exists(od)) {
      msg("Creating output directory: ", od)
      ok <- dir.create(od, recursive = TRUE, showWarnings = FALSE)
      if (!ok) stop("Cannot create output directory: ", od, call. = FALSE)
    }
    msg("✔ Output directory OK: ", od)
  }
  
  msg("GLM validation completed successfully")
  invisible(TRUE)
}