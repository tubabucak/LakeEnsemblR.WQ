#' Build calib_setup from edited calibration CSVs
#'
#' Reads the per-module calibration CSV files produced by
#' \code{\link{create_calibration_tables}}, filters to rows where
#' \code{include == TRUE}, and returns a \code{calib_setup} data frame in the
#' format expected by \code{\link{run_lhc_wq}} and \code{\link{run_sensitivity}}.
#'
#' @param folder_in character; path to the folder containing the
#'   \code{calibration_<module>.csv} files (same as \code{folder_out} used in
#'   \code{\link{create_calibration_tables}}).
#' @param model_coupled character; one coupled model name (e.g.
#'   \code{"GOTM-Selmaprotbas"}, \code{"GLM-AED2"}), or a character vector of
#'   coupled model names. Only parameters for these models are included in the
#'   output.
#' @param group_name character or \code{NULL}; for biological modules with
#'   multiple groups (phytoplankton, zooplankton, etc.) this maps to the
#'   \code{group_name} column expected by \code{run_lhc_wq}. When \code{NULL}
#'   (default) the column is set to \code{NA}.
#'
#' @return A data frame with columns \code{model_coupled}, \code{module},
#'   \code{pars}, \code{lb}, \code{ub}, \code{x0}, \code{log}, \code{file},
#'   \code{group_name}, \code{unit}, \code{note}. When a single model is
#'   supplied, the result can be passed directly to \code{run_lhc_wq} or
#'   \code{run_sensitivity}. When multiple models are supplied, the output is a
#'   combined reference table and should be filtered per model before running
#'   calibration.
#'
#' @details
#' The function looks for all files matching \code{calibration_*.csv} inside
#' \code{folder_in}, stacks them, filters to \code{include == TRUE} and the
#' requested \code{model_coupled}, then renames columns to match the
#' \code{calib_setup} contract used by the calibration runners. If multiple
#' models are supplied, the returned data frame includes \code{model_coupled} so
#' rows from different model couplings remain distinguishable.
#'
#' @examples
#' \dontrun{
#' # After editing calibration CSVs to set include = TRUE:
#' calib_setup <- calib_setup_from_tables(
#'   folder_in     = "calibration",
#'   model_coupled = "GOTM-Selmaprotbas"
#' )
#'
#' results <- run_lhc_wq(
#'   model          = "GOTM-Selmaprotbas",
#'   param_names    = calib_setup$pars,
#'   calib_setup    = calib_setup,
#'   yaml_file      = "metrics.yaml",
#'   model_dir      = "GOTM-Selmaprotbas",
#'   n_samples      = 50,
#'   wq_config_file = "LakeEnsemblR_WQ.yaml"
#' )
#' }
#'
#' @export

calib_setup_from_tables <- function(folder_in,
                                    model_coupled,
                                    group_name = NULL) {

  # Collect all per-module calibration CSVs (exclude the master file)
  csv_files <- list.files(folder_in,
                          pattern = "^calibration_.*\\.csv$",
                          full.names = TRUE)
  csv_files <- csv_files[basename(csv_files) != "calibration_master.csv"]

  if (length(csv_files) == 0) {
    stop("No calibration_<module>.csv files found in: ", folder_in,
         "\nRun create_calibration_tables() first.")
  }

  # Read and stack
  all_tables <- lapply(csv_files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$source_file <- basename(f)
    df$source_group <- sub("^calibration_(.*)\\.csv$", "\\1", basename(f))
    df
  })
  combined <- do.call(rbind, all_tables)

  # Support both the new schema with `include` and legacy tables without it.
  has_include <- "include" %in% names(combined)
  if (has_include) {
    combined$include <- as.logical(combined$include)
    selected <- combined[!is.na(combined$include) & combined$include &
                 combined$model_coupled %in% model_coupled, ]
  } else {
    warning(
      "No 'include' column found in calibration tables. Treating all rows as selected. ",
      "These appear to be legacy calibration CSVs. Re-run create_calibration_tables() ",
      "if you want the new include-based workflow."
    )
    selected <- combined[combined$model_coupled %in% model_coupled, ]
  }

  if (nrow(selected) == 0) {
    if (has_include) {
      stop("No parameters with include = TRUE found for model(s): ",
           paste(model_coupled, collapse = ", "), ".\n",
           "Open the calibration CSVs in '", folder_in,
           "' and set include = TRUE for the parameters you want to calibrate.")
    }

    stop("No calibration rows found for model(s): ",
         paste(model_coupled, collapse = ", "),
         " in '", folder_in, "'.")
  }

  missing_models <- setdiff(model_coupled, unique(selected$model_coupled))
  if (length(missing_models) > 0) {
    warning("No calibration rows were found for: ",
            paste(missing_models, collapse = ", "))
  }

  # Guard: warn about rows where lower >= upper
  bad_bounds <- selected$lower >= selected$upper
  if (any(bad_bounds, na.rm = TRUE)) {
    warning("The following parameters have lower >= upper — check their bounds:\n  ",
            paste(selected$parameter[bad_bounds], collapse = ", "))
  }

  group_modules <- c("phytoplankton", "zooplankton", "fish",
                     "macrophytes", "zoobenthos", "pathogens")
  inferred_group_name <- ifelse(selected$module %in% group_modules,
                                selected$source_group,
                                NA_character_)
  if (!is.null(group_name)) {
    inferred_group_name[] <- group_name
  }

  # Build calib_setup in the format run_lhc_wq / run_sensitivity expect
  calib_setup <- data.frame(
    model_coupled = selected$model_coupled,
    module     = selected$module,
    pars       = selected$parameter,
    lb         = selected$lower,
    ub         = selected$upper,
    x0         = selected$initial,
    log        = as.logical(selected$log),
    file       = selected$path,
    group_name = inferred_group_name,
    unit       = selected$unit,
    note       = selected$note,
    stringsAsFactors = FALSE
  )

  message("calib_setup built: ", nrow(calib_setup), " parameter(s) for ", model_coupled)
  calib_setup
}
