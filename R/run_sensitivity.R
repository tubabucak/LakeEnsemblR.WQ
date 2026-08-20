#' Run Sensitivity Analysis for a Model Parameter
#'
#' Performs a one-at-a-time sensitivity analysis for a specified model parameter,
#' optionally targeting a specific group (e.g., a phytoplankton group in a parameter CSV
#' or a FABM instance for GOTM-WET/GOTM-Selmaprotbas). The parameter value is iteratively
#' changed across a defined range, the model is run, and the selected metrics are
#' calculated at each step.
#'
#' @param param_name Character. Name of the parameter to modify (must match `pars` in `calib_setup`).
#' @param calib_setup Data frame. Calibration setup table containing parameter names, bounds (`lb`, `ub`),
#' initial values (`x0`), file/path names (`file`), and optionally a `group_name` column for group-specific
#' parameters. This is the same table produced by \code{\link{calib_setup_from_tables}} and consumed by
#' \code{\link{run_lhc_wq}}.
#' @param yaml_file Character. Path to the YAML file defining which metrics to extract (output.yaml).
#' @param model_dir Character. Path to the model directory where the input files and outputs are located.
#' @param n_steps Integer. Number of steps (iterations) in the parameter value sequence (default = 10).
#' @param model Character. One of \code{"GLM-AED2"}, \code{"GOTM-WET"},
#'   \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"}. Determines both how
#'   \code{param_name} is written to its target file and which model engine is run.
#'   Default \code{"GLM-AED2"} for backwards compatibility.
#' @param model_filter Character or \code{NULL}. Model identifier used by `cal_metrics()` to
#' filter the results. If \code{NULL} (default), auto-derived from \code{model} (e.g.
#' \code{"GLM"}, \code{"WET"}, \code{"SELMAPROTBAS"}, \code{"SIMSTRAT"}).
#' @param group_name Character or `NULL`. Optional if a certain group (e.g. a phytoplankton
#' group in a parameter CSV, or a FABM instance such as `"diatoms"` for GOTM-WET/GOTM-Selmaprotbas)
#' should be selected. If provided, only updates the specified group/column in the parameter CSV
#' (e.g., `"cyano"`, `"green"`, `"diatom"`), or substitutes into a `{group_name}` placeholder in a
#' GOTM-WET/GOTM-Selmaprotbas `path`. If `NULL`, all relevant rows in `calib_setup` for `param_name`
#' are used.
#' @param yaml_file_model Character or \code{NULL}. GOTM yaml filename (e.g. \code{"gotm.yaml"}),
#'   only used/required when \code{model} is \code{"GOTM-WET"} or \code{"GOTM-Selmaprotbas"}. If
#'   \code{NULL} (default), falls back to \code{"gotm.yaml"}.
#' @param par_file Character or \code{NULL}. Simstrat par filename, only used/required when
#'   \code{model} is \code{"Simstrat-AED2"}. If \code{NULL} (default), falls back to
#'   \code{"simstrat.par"}.
#' @param wq_config_file Character or \code{NULL}. Path to the LakeEnsemblR_WQ config file
#'   (e.g. \code{"LakeEnsemblR_WQ.yaml"}), passed through to \code{cal_metrics()}. Required
#'   when \code{output_mode = "metrics"} -- \code{cal_metrics()} uses it to expand
#'   phytoplankton/zooplankton group templates in the metrics dictionary and has no default
#'   of its own. Ignored when \code{output_mode = "raw"}.
#' @param output_mode Character. \code{"metrics"} (default) runs each step through
#'   \code{cal_metrics()} -- the harmonized/derived-metric pipeline used elsewhere in this
#'   package, requiring \code{wq_config_file} and an entry for the metric(s) of interest in
#'   \code{yaml_file}. \code{"raw"} instead pulls one or more model output variables directly
#'   via \code{get_output_wq()}, bypassing the metrics dictionary/template machinery entirely
#'   -- useful for a quick look at how a raw output variable (e.g. \code{"sO2W"} for GOTM-WET)
#'   responds to \code{param_name} without needing \code{wq_config_file} or an \code{Output.yaml}
#'   entry for it.
#' @param vars Character vector. Required when \code{output_mode = "raw"} -- the model output
#'   variable name(s) to extract at each step (as they appear in the model's native output,
#'   e.g. \code{"sO2W"} for GOTM-WET/GOTM-Selmaprotbas, \code{"OXY_oxy"} for GLM-AED2/Simstrat-AED2).
#'   Passed through to \code{get_output_wq()}'s \code{vars} argument.
#' @param obs_depths Numeric vector or \code{NULL}. Only used when \code{output_mode = "raw"}.
#'   Passed through to \code{get_output_wq()} -- depths to interpolate the raw output to, when
#'   \code{depth_01 = 1}. \code{NULL} (default) returns output at the model's native depths.
#' @param depth_01 Integer. Only used when \code{output_mode = "raw"}. Passed through to
#'   \code{get_output_wq()}: \code{0} if \code{vars} has no depth dimension, \code{1} (default)
#'   if it does.
#' @param conversion_factor Numeric. Only used when \code{output_mode = "raw"}. Passed through
#'   to \code{get_output_wq()}'s unit conversion factor. Default \code{1} (no conversion).
#' @param target_variable Character vector or \code{NULL}. Only used when
#'   \code{output_mode = "metrics"}. If supplied, the \code{cal_metrics()} output for each step
#'   is filtered down to just these metric name(s) (i.e. \code{names(metrics)} in the returned
#'   \code{metrics} list, e.g. \code{"DO_gramsPerCubicMeter"}) instead of returning every metric
#'   defined in \code{yaml_file}. \code{NULL} (default) keeps all of them. Has no effect in
#'   \code{output_mode = "raw"} -- there, \code{vars} already selects exactly what's extracted.
#' @param verbose Logical. Print progress messages and pass through to the model engine's
#' own \code{verbose} argument. Default \code{TRUE}.
#'
#' @return A list with one element per parameter value step. Each element contains:
#' \describe{
#'   \item{param_value}{The value used in this iteration.}
#'   \item{metrics}{The output from `cal_metrics()` for this iteration. Only present when
#'   \code{output_mode = "metrics"}.}
#'   \item{output}{The output from `get_output_wq()` for this iteration (raw model variable(s),
#'   one data frame per variable in \code{vars}). Only present when \code{output_mode = "raw"}.}
#' }
#'
#' @details
#' The function supports `.nml` and `.csv` parameter files (used by \code{"GLM-AED2"} and
#' \code{"Simstrat-AED2"}), as well as FABM `.yaml`/`.yml` parameter files (used by
#' \code{"GOTM-WET"} and \code{"GOTM-Selmaprotbas"}, where `calib_setup$file` holds the FABM
#' instance/key path, e.g. `"abiotic_water/parameters/hO2Nitr"`, written into `fabm.yaml`
#' via \code{LakeEnsemblR::input_yaml_multiple()}). For `.csv` files, it automatically
#' handles quoted column names and parameter names (e.g., `'p_name'`, `'R_growth'`) by stripping quotes.
#' When `group_name` is provided, it only updates that specific group column (`.csv`) or substitutes
#' it into the `{group_name}` placeholder of the `path` (`.yaml`/`.yml`). Otherwise, all columns
#' (from column 2 onward, `.csv`) are updated for the matching parameter row.
#'
#' @importFrom readr read_csv write_csv
#' @importFrom glmtools read_nml set_nml write_nml
#' @importFrom LakeEnsemblR input_yaml_multiple
#'
#' @examples
#' \dontrun{
#' results <- run_sensitivity("R_growth", calib_setup, yaml_file = "metrics.yaml",
#'                            model_dir = "model/", n_steps = 10, model = "GLM-AED2",
#'                            group_name = "cyano", wq_config_file = "LakeEnsemblR_WQ.yaml",
#'                            target_variable = "DO_gramsPerCubicMeter")
#'
#' # raw model output instead of cal_metrics() -- no wq_config_file needed
#' results <- run_sensitivity("hO2Nitr", calib_setup, yaml_file = "Output.yaml",
#'                            model_dir = "GOTM-WET/", n_steps = 10, model = "GOTM-WET",
#'                            output_mode = "raw", vars = "sO2W")
#' }
#'
#' @export

run_sensitivity <- function(param_name, calib_setup, yaml_file, model_dir, n_steps = 10,
                            model = "GLM-AED2", model_filter = NULL, group_name = NULL,
                            yaml_file_model = NULL, par_file = NULL, wq_config_file = NULL,
                            output_mode = "metrics", vars = NULL, obs_depths = NULL,
                            depth_01 = 1, conversion_factor = 1, target_variable = NULL,
                            verbose = TRUE) {

  model_upper <- toupper(model)
  supported <- c("GLM-AED2", "GOTM-WET", "GOTM-SELMAPROTBAS", "SIMSTRAT-AED2")
  if (!model_upper %in% supported) {
    stop("'model' must be one of: ", paste(supported, collapse = ", "),
         "\nProvided: ", model)
  }

  output_mode <- match.arg(output_mode, c("metrics", "raw"))
  if (output_mode == "metrics" && (is.null(wq_config_file) || !nzchar(wq_config_file))) {
    stop("'wq_config_file' is required when output_mode = 'metrics' -- cal_metrics() has ",
         "no default for it. Pass wq_config_file, or use output_mode = 'raw' with 'vars' ",
         "to skip cal_metrics() entirely.")
  }
  if (output_mode == "raw" && (is.null(vars) || length(vars) == 0L)) {
    stop("'vars' is required when output_mode = 'raw' -- it names the raw model output ",
         "variable(s) to extract at each step (e.g. 'sO2W' for GOTM-WET).")
  }

  # Auto-derive model_filter from model if not provided -- same mapping used
  # by run_lhc_wq()/run_lhc_wq_parallel() so cal_metrics() sees a consistent
  # model key regardless of which calibration entry point was used.
  if (is.null(model_filter)) {
    model_filter <- switch(model_upper,
      "GLM-AED2"          = "GLM",
      "GOTM-WET"          = "WET",
      "GOTM-SELMAPROTBAS" = "SELMAPROTBAS",
      "SIMSTRAT-AED2"     = "SIMSTRAT"
    )
    if (isTRUE(verbose)) {
      message("[Sensitivity] Auto-derived model_filter='", model_filter, "' from model='", model, "'")
    }
  }

  if (is.null(yaml_file_model) || length(yaml_file_model) == 0L) yaml_file_model <- "gotm.yaml"
  if (is.null(par_file) || length(par_file) == 0L) par_file <- "simstrat.par"

  if (model_upper %in% c("GOTM-WET", "GOTM-SELMAPROTBAS")) {
    gotm_yaml_path <- file.path(model_dir, yaml_file_model)
    if (!file.exists(gotm_yaml_path)) {
      stop("Could not find GOTM yaml file: ", gotm_yaml_path)
    }
  }
  if (model_upper == "SIMSTRAT-AED2") {
    sim_par_path <- file.path(model_dir, par_file)
    if (!file.exists(sim_par_path)) {
      stop("Could not find Simstrat par file: ", sim_par_path)
    }
  }

  # Filter relevant rows
  if (!param_name %in% calib_setup$pars) {
    stop("Parameter not found in calib_setup dataframe: ", param_name)
  }

  param_rows <- calib_setup[calib_setup$pars == param_name, ]

  # If group_name is provided, filter further
  if (!is.null(group_name)) {
    param_rows <- param_rows[param_rows$group_name == group_name, ]
    if (nrow(param_rows) == 0) {
      stop("No matching entry for param '", param_name, "' with group_name '", group_name, "'")
    }
  }

  param_values <- seq(param_rows$lb[1], param_rows$ub[1], length.out = n_steps)
  results <- list()

  for (i in seq_along(param_values)) {
    cat("\nIteration", i, ": setting", param_name, "to", param_values[i], "\n")

    for (k in seq_len(nrow(param_rows))) {
      file_or_path <- as.character(param_rows$file[k])

      if (model_upper %in% c("GOTM-WET", "GOTM-SELMAPROTBAS")) {
        # calib_setup$file holds a FABM instance/key path (e.g.
        # "abiotic_water/parameters/hO2Nitr"), not a literal file name --
        # the target file is always fabm.yaml (fallback: lake_ensemblr.yaml).
        yaml_target <- file.path(model_dir, "fabm.yaml")
        if (!file.exists(yaml_target)) yaml_target <- file.path(model_dir, "lake_ensemblr.yaml")

        path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
        group_col_k <- if ("group_name" %in% names(param_rows)) param_rows$group_name[k] else NA_character_
        if (!is.na(group_col_k)) path_parts[path_parts == "{group_name}"] <- group_col_k

        arglist <- as.list(path_parts)
        names(arglist) <- paste0("key", seq_along(path_parts))
        arglist$value <- param_values[i]
        arglist$file <- yaml_target
        arglist$verbose <- FALSE
        do.call(LakeEnsemblR::input_yaml_multiple, args = arglist)

      } else if (grepl("\\.nml$", file_or_path, ignore.case = TRUE)) {
        param_path <- file.path(model_dir, file_or_path)
        nml <- glmtools::read_nml(param_path)
        nml <- glmtools::set_nml(nml, param_name, param_values[i])
        glmtools::write_nml(nml, param_path)

      } else if (grepl("\\.csv$", file_or_path, ignore.case = TRUE)) {
        param_path <- file.path(model_dir, file_or_path)
        df <- readr::read_csv(param_path, show_col_types = FALSE)
        # Clean column names by removing quotes
        names(df) <- gsub("^['\"]|['\"]$", "", names(df))

        # Identify the parameter name column
        pname_col <- intersect(c("p_name", "pname"), names(df))
        if (length(pname_col) == 0) {
          stop("Could not identify parameter name column ('p_name' or 'pname') in CSV")
        }

        # Clean values in the parameter name column by removing quotes
        df[[pname_col]] <- gsub("^['\"]|['\"]$", "", df[[pname_col]])

        idx <- which(df[[pname_col]] == param_name)
        if (length(idx) == 0) stop("Parameter not found in CSV file: ", param_name)

        group_col_k <- if ("group_name" %in% names(param_rows)) param_rows$group_name[k] else NA_character_
        if (!is.na(group_col_k) && group_col_k %in% names(df)) {
          df[idx, group_col_k] <- param_values[i]
        } else {
          df[idx, 2:ncol(df)] <- param_values[i]
        }
        readr::write_csv(df, param_path)

      } else {
        stop("Unsupported file type for model '", model, "': ", file_or_path)
      }
    }

    out <- switch(model_upper,
      "GLM-AED2" = {
        if (!requireNamespace("GLM3r", quietly = TRUE)) {
          stop("Package 'GLM3r' is required to run GLM-AED2.")
        }
        GLM3r::run_glm(sim_folder = model_dir, verbose = verbose)
      },
      "GOTM-WET" = {
        if (!requireNamespace("WETr", quietly = TRUE)) {
          stop("Package 'WETr' is required to run GOTM-WET.")
        }
        WETr::run_wet(sim_folder = model_dir, yaml_file = yaml_file_model, verbose = verbose)
      },
      "GOTM-SELMAPROTBAS" = {
        if (!requireNamespace("SelmaprotbasR", quietly = TRUE)) {
          stop("Package 'SelmaprotbasR' is required to run GOTM-Selmaprotbas.")
        }
        SelmaprotbasR::run_gotm_sp(sim_folder = model_dir, yaml_file = yaml_file_model, verbose = verbose)
      },
      "SIMSTRAT-AED2" = {
        if (!requireNamespace("SimstratR", quietly = TRUE)) {
          stop("Package 'SimstratR' is required to run Simstrat-AED2.")
        }
        SimstratR::run_simstrat(sim_folder = model_dir, par_file = par_file, verbose = verbose)
      }
    )
    # system2()-based runners only ever surface a non-zero exit status as a
    # warning, never an R error -- escalate it here so a crashed model engine
    # isn't silently treated as a successful iteration whose metrics just get
    # rescored from stale output.
    status <- attr(out, "status")
    if (!is.null(status) && !isTRUE(status == 0)) {
      out_text <- if (is.character(out) && length(out) > 0) paste(out, collapse = "\n") else "(no stdout captured)"
      stop("Model engine exited with non-zero status (", status, "). ",
           "The simulation likely failed to run -- check for missing input ",
           "files or a config error. Engine output:\n", out_text)
    }

    if (output_mode == "raw") {
      output <- get_output_wq(
        config_file       = yaml_file,
        model             = model_filter,
        vars              = vars,
        obs_depths        = obs_depths,
        depth_01          = depth_01,
        conversion_factor = conversion_factor
      )
      results[[i]] <- list(param_value = param_values[i], output = output)
    } else {
      metrics <- cal_metrics(yaml_file, model_filter = model_filter, wq_config_file = wq_config_file)
      if (!is.null(target_variable)) {
        matched <- intersect(names(metrics), target_variable)
        if (length(matched) == 0L) {
          warning("None of target_variable (", paste(target_variable, collapse = ", "),
                  ") were found among cal_metrics() metric names (",
                  paste(names(metrics), collapse = ", "), "). Returning empty metrics for this step.")
        }
        metrics <- metrics[matched]
      }
      results[[i]] <- list(param_value = param_values[i], metrics = metrics)
    }
  }

  return(results)
}
