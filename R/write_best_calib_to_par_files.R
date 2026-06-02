#' Write best calibration parameter set back to par_file CSVs
#'
#' After running \code{\link{run_lhc_wq}} with \code{obs_file} supplied (which
#' returns a data frame of parameter values + performance statistics), this
#' function picks the best-performing row and writes the calibrated parameter
#' values into the \code{par_file} CSVs referenced in the
#' \code{LakeEnsemblR_WQ.yaml} config.  This closes the loop between the
#' calibration workflow and the parameter-override files consumed by
#' \code{\link{export_config_wq}}.
#'
#' @param lhc_results data.frame; the output of \code{run_lhc_wq()} when
#'   \code{obs_file} is supplied.  Must contain one column per calibrated
#'   parameter and at least one performance-metric column (e.g. \code{NSE},
#'   \code{KGE}).
#' @param calib_setup data.frame; the calibration setup table used when running
#'   \code{run_lhc_wq()}, as produced by \code{\link{calib_setup_from_tables}}.
#'   Must contain columns \code{pars}, \code{model_coupled}, \code{domain},
#'   \code{process}, \code{subprocess}, and optionally \code{group_name}.
#' @param config_file character; name of the \code{LakeEnsemblR_WQ.yaml} master
#'   config file.
#' @param folder character; path to the directory containing
#'   \code{config_file}. Defaults to \code{"."}.
#' @param metric character; name of the performance metric column in
#'   \code{lhc_results} to use for selecting the best run. Defaults to
#'   \code{"KGE"}.  The metric column name may be prefixed by a variable name
#'   (e.g. \code{"oxygen_NSE"}); the function looks for an exact match first,
#'   then falls back to columns that contain \code{metric}.
#' @param minimize logical; if \code{TRUE} the row with the \emph{lowest} metric
#'   value is chosen (e.g. RMSE).  If \code{FALSE} (default) the row with the
#'   \emph{highest} value is chosen (e.g. KGE, NSE).
#' @param write_target character; where to write best values. One of
#'   \code{"par_file"} (default, updates module \code{par_file} CSVs) or
#'   \code{"config"} (uses \code{set_value_config()} to write directly to
#'   model config files).
#' @param verbose logical; if \code{TRUE} print a message for each parameter
#'   that is updated.  Defaults to \code{TRUE}.
#'
#' @return Invisibly returns a named list of the best parameter values that were
#'   written.
#'
#' @details
#' The function resolves each calibrated parameter to its \code{par_file} CSV
#' by matching against \code{model_coupled}, \code{domain}, \code{process},
#' \code{subprocess}, and \code{parameter} columns in the CSV.  For biological
#' group modules (phytoplankton, zooplankton, etc.) the \code{group_name} column
#' in \code{calib_setup} is used to identify the correct group CSV.
#'
#' Parameters present in \code{calib_setup} but not found as a matching row in
#' the target CSV will generate a warning (not an error) so that the remaining
#' parameters are still written.
#'
#' @examples
#' \dontrun{
#' calib_setup <- calib_setup_from_tables(
#'   folder_in     = "calibration",
#'   model_coupled = "GOTM-Selmaprotbas"
#' )
#'
#' lhc_results <- run_lhc_wq(
#'   model          = "GOTM-Selmaprotbas",
#'   param_names    = calib_setup$pars,
#'   calib_setup    = calib_setup,
#'   yaml_file      = "metrics.yaml",
#'   model_dir      = "GOTM-Selmaprotbas",
#'   n_samples      = 50,
#'   wq_config_file = "LakeEnsemblR_WQ.yaml",
#'   obs_file       = "observed_data.csv"
#' )
#'
#' write_best_calib_to_par_files(
#'   lhc_results  = lhc_results,
#'   calib_setup  = calib_setup,
#'   config_file  = "LakeEnsemblR_WQ.yaml",
#'   folder       = ".",
#'   metric       = "KGE"
#' )
#' }
#'
#' @importFrom configr read.config
#' @export

write_best_calib_to_par_files <- function(lhc_results,
                                          calib_setup,
                                          config_file,
                                          folder    = ".",
                                          metric    = "KGE",
                                          minimize  = FALSE,
                                          write_target = c("par_file", "config"),
                                          verbose   = TRUE) {

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
  write_target <- match.arg(write_target)

  # ---- input checks --------------------------------------------------------
  if (!is.data.frame(lhc_results) || nrow(lhc_results) == 0) {
    stop("'lhc_results' must be a non-empty data frame (output of run_lhc_wq() ",
         "with obs_file supplied).")
  }

  required_calib_cols <- c("pars", "model_coupled", "domain", "process",
                           "subprocess")
  missing_cols <- setdiff(required_calib_cols, names(calib_setup))
  if (length(missing_cols) > 0) {
    stop("'calib_setup' is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # ---- identify parameter columns in lhc_results ---------------------------
  param_names <- intersect(calib_setup$pars, names(lhc_results))
  if (length(param_names) == 0) {
    stop("None of the parameters in 'calib_setup$pars' were found as columns ",
         "in 'lhc_results'. Check that the same calib_setup was used for both.")
  }

  # ---- pick best row/run from lhc_results ----------------------------------
  best_row <- NULL
  best_label <- NULL

  attr_best <- attr(lhc_results, "best_parameter_set")
  if (is.data.frame(attr_best) && nrow(attr_best) > 0) {
    has_params <- all(param_names %in% names(attr_best))
    if (has_params) {
      best_row <- attr_best[1, , drop = FALSE]
      best_label <- paste0("attribute:best_parameter_set (sample_index=", as.integer(best_row$sample_index[1] %||% NA_integer_), ")")
    }
  }

  if (is.null(best_row) && "is_best" %in% names(lhc_results)) {
    best_rows <- lhc_results[!is.na(lhc_results$is_best) & lhc_results$is_best %in% TRUE, , drop = FALSE]
    if (nrow(best_rows) > 0) {
      if ("sample_index" %in% names(best_rows)) {
        best_sample <- best_rows$sample_index[1]
        best_rows <- best_rows[best_rows$sample_index == best_sample, , drop = FALSE]
      }
      best_row <- best_rows[1, , drop = FALSE]
      best_label <- paste0("column:is_best", if ("sample_index" %in% names(best_row)) paste0(" (sample_index=", as.integer(best_row$sample_index[1]), ")") else "")
    }
  }

  if (is.null(best_row)) {
    metric_cols <- grep(metric, names(lhc_results), value = TRUE, ignore.case = TRUE)
    if (length(metric_cols) == 0) {
      stop("No column matching '", metric, "' found in lhc_results. ",
           "Available columns: ", paste(names(lhc_results), collapse = ", "))
    }

    if ("sample_index" %in% names(lhc_results)) {
      samples <- sort(unique(lhc_results$sample_index))
      samples <- samples[!is.na(samples)]
      if (length(samples) == 0) {
        stop("sample_index column exists but contains no valid values.")
      }

      sample_scores <- vapply(samples, function(s) {
        sub <- lhc_results[lhc_results$sample_index == s, , drop = FALSE]
        vals <- rowMeans(sub[, metric_cols, drop = FALSE], na.rm = TRUE)
        w <- if ("n_pairs" %in% names(sub)) suppressWarnings(as.numeric(sub$n_pairs)) else rep(1, length(vals))
        w[is.na(w) | w <= 0] <- 1
        ok <- is.finite(vals) & is.finite(w)
        if (!any(ok)) return(NA_real_)
        stats::weighted.mean(vals[ok], w = w[ok], na.rm = TRUE)
      }, numeric(1))

      if (all(is.na(sample_scores))) {
        stop("All values in metric column(s) '", paste(metric_cols, collapse = ", "),
             "' are NA. Cannot select best run.")
      }

      best_sample <- if (minimize) samples[which.min(sample_scores)] else samples[which.max(sample_scores)]
      best_rows <- lhc_results[lhc_results$sample_index == best_sample, , drop = FALSE]
      best_row <- best_rows[1, , drop = FALSE]
      best_label <- paste0("metric:", metric, " (sample_index=", as.integer(best_sample), ")")
    } else {
      metric_values <- rowMeans(lhc_results[, metric_cols, drop = FALSE], na.rm = TRUE)

      if (all(is.na(metric_values))) {
        stop("All values in metric column(s) '", paste(metric_cols, collapse = ", "),
             "' are NA. Cannot select best run.")
      }

      best_idx <- if (minimize) which.min(metric_values) else which.max(metric_values)
      best_row <- lhc_results[best_idx, , drop = FALSE]
      best_label <- paste0("metric:", metric, " (row=", best_idx, ")")
    }
  }

  if (verbose) {
    message("Best run selected from ", best_label)
  }

  # ---- read config ---------------------------------------------------------
  cfg_path <- file.path(folder, config_file)
  if (!file.exists(cfg_path)) {
    stop("config_file not found at: ", cfg_path)
  }

  lst_config <- configr::read.config(cfg_path)

  if (write_target == "config") {
    if (is.null(lst_config[["config_files"]])) {
      stop("For write_target='config', config_file must be a LakeEnsemblR_WQ master config with a 'config_files' section. ",
           "Received: ", cfg_path)
    }
  }

  group_modules <- c("phytoplankton", "zooplankton", "fish",
                     "macrophytes", "zoobenthos", "pathogens")

  # Build a lookup: module (+ optional group_name) -> par_file path
  .par_file_path <- function(module, group_name = NA) {
    mod_cfg <- lst_config[[module]]
    if (is.null(mod_cfg)) return(NULL)

    if (module %in% group_modules) {
      if (!is.na(group_name) && !is.null(mod_cfg[["groups"]][[group_name]])) {
        pf <- mod_cfg[["groups"]][[group_name]][["par_file"]]
      } else {
        # Try first group as fallback
        pf <- mod_cfg[["groups"]][[1]][["par_file"]]
      }
    } else {
      pf <- mod_cfg[["par_file"]]
    }

    if (is.null(pf) || pf == "") return(NULL)
    file.path(folder, pf)
  }

  # ---- write best values ---------------------------------------------------
  written <- list()

  for (p in param_names) {
    best_value <- best_row[[p]]
    rows_p     <- calib_setup[calib_setup$pars == p, , drop = FALSE]

    for (k in seq_len(nrow(rows_p))) {
      model_coupled <- rows_p$model_coupled[k]
      domain        <- rows_p$domain[k]
      process       <- rows_p$process[k]
      subprocess    <- rows_p$subprocess[k]
      module        <- if ("module" %in% names(rows_p)) rows_p$module[k] else NA_character_
      grp           <- if ("group_name" %in% names(rows_p)) rows_p$group_name[k] else NA_character_
      ok_write <- FALSE

      if (write_target == "par_file") {
        par_path <- .par_file_path(module, grp)

        if (is.null(par_path)) {
          warning("Could not resolve par_file for module '", module,
                  "' (group: '", grp, "'). Skipping parameter '", p, "'.")
          next
        }

        if (!file.exists(par_path)) {
          warning("par_file not found: ", par_path,
                  ". Skipping parameter '", p, "'.")
          next
        }

        csv <- read.csv(par_path, stringsAsFactors = FALSE)

        # Match on parameter + model_coupled + domain + process + subprocess
        match_idx <- which(
          csv$parameter    == p              &
          csv$model_coupled == model_coupled &
          csv$domain        == domain        &
          csv$process       == process       &
          csv$subprocess    == subprocess
        )

        if (length(match_idx) == 0) {
          # Try matching on parameter + model_coupled only (looser)
          match_idx <- which(
            csv$parameter     == p &
            csv$model_coupled == model_coupled
          )
        }

        if (length(match_idx) == 0) {
          warning("Parameter '", p, "' for model '", model_coupled,
                  "' not found in ", par_path, ". Skipping.")
          next
        }

        csv$value[match_idx] <- best_value
        write.csv(csv, par_path, row.names = FALSE, quote = TRUE)
        ok_write <- TRUE

        if (verbose) {
          message("  Updated '", p, "' = ", best_value,
                  " in ", par_path)
        }
      } else {
        group_position <- if ("group_position" %in% names(rows_p)) rows_p$group_position[k] else NULL
        write_res <- tryCatch(
          set_value_config(
            config_file = config_file,
            module = module,
            group_name = grp,
            group_position = group_position,
            domain = domain,
            process = process,
            subprocess = subprocess,
            model_coupled = model_coupled,
            parameter = p,
            value = best_value,
            folder = folder,
            verbose = verbose
          ),
          error = function(e) {
            warning("Failed writing parameter '", p, "' via set_value_config: ", conditionMessage(e))
            FALSE
          }
        )
        ok_write <- !identical(write_res, FALSE)

        if (verbose && ok_write) {
          message("  Updated '", p, "' = ", best_value,
                  " in config for ", model_coupled, " (module: ", module, ")")
        }
      }

      if (ok_write) {
        written[[p]] <- best_value
      }
    }
  }

  if (verbose) {
    target_label <- if (write_target == "par_file") "par_file CSVs" else "model config files"
    message("Done. ", length(written), " parameter(s) written to ", target_label, ".")
  }

  invisible(written)
}
