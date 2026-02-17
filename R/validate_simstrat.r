#' @title Validate a Simstrat-AED2 model simulation folder
#'
#' @description
#' The function checks that the Simstrat configuration and key input files referenced
#' inside the simstrat.par are present in the simulation folder. It also checks that
#' the output directory exists, and attempts to create it if missing.
#'
#' If any required file is missing, the function stops with an informative
#' error message describing which file(s) could not be found.
#'
#' @name validate_simstrat
#' @param sim_folder Character. Path to the Simstrat-AED2 simulation folder containing
#'   the namelist and input files.
#' @param file Character. Name of the Simstrat-AED2 configuration file inside
#'   \code{sim_folder}. Default is \code{"simstrat.par"}.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed using
#'   \code{message()}. Default is \code{TRUE}.
#' @param check_time_coverage Logical. If \code{TRUE}, checks that time series inputs
#'   cover the simulation end time. Default is \code{TRUE}.
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds. Otherwise, the
#'   function throws an error.
#'
#' @examples
#' \dontrun{
#' validate_simstrat(sim_folder = "models/simstrat_aed2", file = "simstrat.par")
#' }
#'
#' @export
validate_simstrat <- function(sim_folder = ".",
                              file = "simstrat.par",
                              verbose = TRUE,
                              check_time_coverage = TRUE) {

  msg <- function(...) if (isTRUE(verbose)) message(...)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').",
         call. = FALSE)
  }

  par_path <- file.path(sim_folder, file)
  if (!file.exists(par_path)) stop("Missing Simstrat parameter file: ", par_path, call. = FALSE)
  msg("✔ Found Simstrat parameter file: ", file)

  # ---- helper: read .par (JSON-like) into R list ----
  read_simstrat_par <- function(path) {
    txt <- readLines(path, warn = FALSE)

    # remove comments starting with "!"
    txt <- sub("!.*$", "", txt)

    # collapse to one string
    s <- paste(txt, collapse = "\n")

    # quote unquoted keys: key : value -> "key" : value  (skip if already quoted)
    s <- gsub('([\\{,]\\s*)([A-Za-z0-9_ /\\-]+)\\s*:', '\\1"\\2":', s, perl = TRUE)

    # remove trailing commas before } or ]
    s <- gsub(",\\s*([\\}\\]])", "\\1", s, perl = TRUE)

    jsonlite::fromJSON(s, simplifyVector = FALSE)
  }

extract_time_values <- function(fp, n_head = 5000, n_tail = 5000) {
  # Head chunk
  head_lines <- readLines(fp, n = n_head, warn = FALSE)

  # Tail chunk (read last N lines without loading everything into memory at once)
  con <- file(fp, open = "r")
  on.exit(close(con), add = TRUE)
  all_lines <- readLines(con, warn = FALSE)
  tail_lines <- utils::tail(all_lines, n_tail)

  lines <- c(head_lines, tail_lines)

  # Keep only lines starting with a number (incl. negative/decimal)
  lines <- lines[grepl("^\\s*-?\\d+(\\.\\d+)?", lines)]
  if (length(lines) == 0) return(numeric(0))

  # Extract the first numeric token from each line
  tvals <- suppressWarnings(as.numeric(sub("^\\s*([-]?\\d+(?:\\.\\d+)?).*", "\\1", lines)))
  tvals[is.finite(tvals)]
}



  cfg <- tryCatch(
    read_simstrat_par(par_path),
    error = function(e) {
      stop("Failed to parse ", file, " as JSON-like config: ", conditionMessage(e), call. = FALSE)
    }
  )

  # ---- required sections ----
  if (is.null(cfg$Input)) stop("Missing 'Input' section in ", file, call. = FALSE)
  if (is.null(cfg$Simulation)) stop("Missing 'Simulation' section in ", file, call. = FALSE)

  # ---- check input files exist (only those that look like file paths) ----
  input_files <- cfg$Input
  input_paths <- Filter(function(x) is.character(x) && length(x) == 1 && nzchar(x), input_files)

  if (length(input_paths) > 0) {
    for (nm in names(input_paths)) {
      f <- file.path(sim_folder, input_paths[[nm]])
      if (!file.exists(f)) stop("Missing input file (", nm, "): ", f, call. = FALSE)
      msg("✔ Input OK: ", nm, " -> ", input_paths[[nm]])
    }
  }

  # ---- output path (create if needed) ----
  out_path <- cfg$Output$Path
  if (!is.null(out_path) && is.character(out_path) && nzchar(out_path)) {
    od <- file.path(sim_folder, out_path)
    if (!dir.exists(od)) {
      msg("Creating output directory: ", od)
      ok <- dir.create(od, recursive = TRUE, showWarnings = FALSE)
      if (!ok) stop("Cannot create output directory: ", od, call. = FALSE)
    }
    msg("✔ Output directory OK: ", od)
  }

  # ---- AED2 check if coupled ----
  if (isTRUE(cfg$ModelConfig$CoupleAED2)) {
    aed2_file <- cfg$AED2Config$AED2ConfigFile
    if (is.null(aed2_file) || !nzchar(aed2_file)) {
      stop("CoupleAED2 is TRUE but AED2ConfigFile is missing/empty.", call. = FALSE)
    }
    aed2_path <- file.path(sim_folder, aed2_file)
    if (!file.exists(aed2_path)) stop("Missing AED2 config file: ", aed2_path, call. = FALSE)
    msg("✔ AED2 config OK: ", aed2_file)
  }


  # ---- time coverage checks ----
if (isTRUE(check_time_coverage)) {
  start_d <- cfg$Simulation[["Start d"]]
  end_d   <- cfg$Simulation[["End d"]]

  check_time_file <- function(label, rel_path) {
    if (is.null(rel_path) || !nzchar(rel_path)) return(invisible(TRUE))
    fp <- file.path(sim_folder, rel_path)

    tvals <- extract_time_values(fp)
    if (length(tvals) == 0) {
      msg("ℹ Skipping time coverage check (no numeric time values found): ", label)
      return(invisible(TRUE))
    }

    # Simstrat convention: negative times may exist (data before day 0)
    t_nonneg <- tvals[tvals >= 0]
    if (length(t_nonneg) == 0) {
      stop(label, " has no non-negative time values (>=0): ", fp, call. = FALSE)
    }

    tmin_used <- min(t_nonneg)
    tmax_used <- max(t_nonneg)

    if (!is.null(start_d) && is.numeric(start_d) && is.finite(start_d) && tmin_used > start_d) {
      msg("⚠ ", label, " starts after simulation start (min t=", tmin_used, " > Start d=", start_d, ")")
    }

    if (!is.null(end_d) && is.numeric(end_d) && is.finite(end_d) && tmax_used < end_d) {
      stop(label, " does not cover simulation end (max t=", tmax_used, " < End d=", end_d, ")", call. = FALSE)
    }

    msg("✔ Time coverage OK for ", label,
        " (used: ", tmin_used, " .. ", tmax_used,
        "; raw min=", min(tvals), "; raw max=", max(tvals), ")")
    invisible(TRUE)
  }

  if (is.character(cfg$Input$Forcing)) check_time_file("Forcing", cfg$Input$Forcing)
  if (is.character(cfg$Input$Inflow))  check_time_file("Inflow",  cfg$Input$Inflow)
  if (is.character(cfg$Input$Outflow)) check_time_file("Outflow", cfg$Input$Outflow)
}

msg("Simstrat validation completed successfully")
invisible(TRUE)
}


