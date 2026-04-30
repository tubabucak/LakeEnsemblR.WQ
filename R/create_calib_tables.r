#' Create calibration tables
#'
#' Generates a master calibration CSV and per-module CSVs from the LakeEnsemblR.WQ
#' dictionary. Every parameter gets \code{include = FALSE} by default so users can
#' review and selectively opt-in. Lower and upper bounds are set to
#' \code{default * (1 - bounds_factor)} and \code{default * (1 + bounds_factor)}.
#'
#' @details
#' \strong{Workflow:}
#' \enumerate{
#'   \item Run \code{create_calibration_tables()} — generates
#'     \code{calibration_master.csv} (read-only reference) and one
#'     \code{calibration_<module>.csv} per active module.
#'   \item Open the per-module CSVs, set \code{include = TRUE} for parameters
#'     you want to calibrate, and adjust \code{lower} / \code{upper} /
#'     \code{initial} as needed.
#'   \item Call \code{\link{calib_setup_from_tables}} to read the edited CSVs
#'     and build the \code{calib_setup} data frame expected by
#'     \code{\link{run_lhc_wq}} and \code{\link{run_sensitivity}}.
#' }
#'
#' @param folder path; directory containing the config file.
#' @param config_file character; name of the LakeEnsemblR_WQ YAML config file.
#' @param folder_out path; output directory for the CSV files (created if needed).
#' @param models_coupled character vector; model couplings to include.
#' @param bounds_factor numeric; fractional deviation from default for bounds
#'   (default = 0.2, i.e. ±20\%).
#'
#' @return Invisibly returns the master calibration table as a data frame.
#'
#' @importFrom configr read.config
#' @importFrom plyr count
#' @importFrom stringr str_extract
#'
#' @examples
#' \dontrun{
#' create_calibration_tables(
#'   folder         = ".",
#'   config_file    = "LakeEnsemblR_WQ.yaml",
#'   folder_out     = "calibration",
#'   models_coupled = c("GOTM-Selmaprotbas", "GLM-AED2"),
#'   bounds_factor  = 0.2
#' )
#' # Then edit calibration/calibration_<module>.csv files,
#' # set include = TRUE for chosen parameters, and run:
#' calib_setup <- calib_setup_from_tables(folder_in = "calibration",
#'                                        model_coupled = "GOTM-Selmaprotbas")
#' }
#'
#' @export

create_calibration_tables <- function(folder = ".",
                                      config_file,
                                      folder_out     = folder,
                                      models_coupled = c("GLM-AED2", "GOTM-Selmaprotbas",
                                                         "GOTM-WET", "Simstrat-AED2"),
                                      bounds_factor  = 0.2) {

  if (!file.exists(folder_out)) {
    dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)
  }

  lst_config <- read.config(file.path(folder, config_file))

  # Map coupled model names to the short model name used in the dictionary
  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function(x) tolower(x[length(x)]))
  names(wq_models) <- models_coupled

  calib_table <- LakeEnsemblR_WQ_dictionary

  # Replicate rows for models that appear multiple times
  counts <- count(wq_models)
  counts$x <- as.character(counts$x)
  calib_table$dupl_freq <- sapply(calib_table$model,
                                   function(x) counts$freq[counts$x == x])
  calib_table <- calib_table[which(calib_table$dupl_freq > 0), ]
  dupl_rows <- rep(seq_len(nrow(calib_table)), times = calib_table$dupl_freq)
  calib_table <- calib_table[dupl_rows, ]

  while (any(duplicated(wq_models))) {
    wq_models[duplicated(wq_models)] <- paste0(wq_models[duplicated(wq_models)], ".1")
  }

  model_addition <- str_extract(row.names(calib_table), "\\.[\\d*]")
  model_addition[is.na(model_addition)] <- ""
  calib_table$model_coupled <- paste0(calib_table$model, model_addition)
  calib_table$model_coupled <- sapply(calib_table$model_coupled,
                                       function(x) names(wq_models)[wq_models == x])

  # Keep only models requested by the user and rows with a numeric default
  calib_table <- calib_table[calib_table$model_coupled %in% models_coupled, ]
  calib_table <- calib_table[!is.na(suppressWarnings(as.numeric(calib_table$default))), ]
  calib_table$default <- as.numeric(calib_table$default)

  # Build calibration columns
  calib_table$include <- FALSE
  calib_table$lower   <- calib_table$default * (1 - bounds_factor)
  calib_table$upper   <- calib_table$default * (1 + bounds_factor)
  calib_table$initial <- calib_table$default
  calib_table$log     <- FALSE

  # Column order for output
  out_cols <- c("include", "module", "domain", "process", "subprocess",
                "model_coupled", "parameter", "default", "lower", "upper",
                "initial", "log", "unit", "path", "note")
  calib_table <- calib_table[, out_cols]

  # --- Master reference file (all models, all parameters) ---
  master_file <- file.path(folder_out, "calibration_master.csv")
  write.csv(calib_table, master_file, row.names = FALSE)
  message("Created master reference: ", master_file)

  # --- Per-module editable files (filtered to active modules from config) ---
  modules <- names(lst_config)[!(names(lst_config) %in%
                                   c("models", "config_files", "run_settings", "input", "output"))]
  modules <- Filter(function(x) {
    !is.null(lst_config[[x]]) && isTRUE(lst_config[[x]][["use"]])
  }, modules)

  for (i in modules) {
    # Group-aware modules get one file per group
    if (i %in% c("phytoplankton", "zooplankton", "fish",
                  "macrophytes", "zoobenthos", "pathogens")) {
      groups <- if (!is.null(lst_config[[i]][["groups"]])) {
        names(lst_config[[i]][["groups"]])
      } else {
        i
      }
    } else {
      groups <- i
    }

    for (j in groups) {
      subset <- calib_table[calib_table$module == i, ]
      if (nrow(subset) > 0) {
        fname <- file.path(folder_out, paste0("calibration_", j, ".csv"))
        write.csv(subset, fname, row.names = FALSE)
        message("Created: ", fname)
      }
    }
  }

  message("\nEdit the per-module CSVs: set include = TRUE for parameters to calibrate.")
  message("Then call calib_setup_from_tables() to build the calib_setup for run_lhc_wq().")
  invisible(calib_table)
}