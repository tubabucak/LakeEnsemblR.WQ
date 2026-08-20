#' Run Multi-Parameter Sensitivity Analysis Using Latin Hypercube Sampling
#'
#' This function performs a multi-parameter sensitivity analysis using
#' Latin Hypercube Sampling (LHS): at each iteration, ALL parameters in
#' \code{param_names} are varied simultaneously (unlike \code{\link{run_sensitivity}},
#' which varies one parameter at a time). It modifies the input files for a coupled
#' lake model, runs the model, and extracts either \code{cal_metrics()} output or raw
#' model output for each iteration.
#'
#' Parameter sampling is based on Latin Hypercube Sampling (via \code{lhs::randomLHS}).
#' Bounds are, per parameter, either \code{x0 * (1 +/- rel_change)} (when \code{rel_change}
#' is supplied) or \code{calib_setup}'s own \code{lb}/\code{ub} columns (when
#' \code{rel_change = NULL}, the default) -- the same bounds convention
#' \code{\link{run_lhc_wq}}/\code{\link{run_sensitivity}} use.
#'
#' @param param_names Character vector or \code{NULL}. Parameter names to vary in the
#'   sensitivity analysis. May contain duplicate names (e.g. one physical parameter calibrated
#'   independently for two phytoplankton groups); each occurrence is matched to its own row in
#'   \code{calib_setup} in order, the same way \code{\link{run_lhc_wq}} disambiguates duplicate
#'   \code{pars} names. \code{NULL} (default) uses every row of \code{calib_setup$pars} as-is,
#'   in order -- i.e. by default every parameter in \code{calib_setup} is varied.
#' @param calib_setup A data frame containing calibration setup information. Must include columns
#'   \code{pars} (parameter names), \code{file}/path, and \code{lb}/\code{ub} (used when
#'   \code{rel_change = NULL}) or \code{x0} (used when \code{rel_change} is supplied); optionally
#'   \code{group_name} for group-specific CSV columns or FABM instance paths.
#' @param rel_change Numeric or \code{NULL}. When supplied, bounds are \code{x0 * (1 -
#'   rel_change)} to \code{x0 * (1 + rel_change)} for every parameter (e.g. \code{0.1} for
#'   +/-10 percent). When \code{NULL} (default), bounds come from \code{calib_setup$lb}/\code{ub}.
#' @param yaml_file Path to the YAML file used for metric extraction by \code{cal_metrics()}/
#'   model-folder resolution by \code{get_output_wq()} (output.yaml).
#' @param model_dir Path to the directory containing the lake model files and subdirectories.
#' @param n_steps Number of LHS iterations (i.e., model realizations).
#' @param model Character. One of \code{"GLM-AED2"}, \code{"GOTM-WET"},
#'   \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"}. Determines both how each parameter is
#'   written to its target file and which model engine is run. Default \code{"GLM-AED2"} for
#'   backwards compatibility.
#' @param model_filter Character or \code{NULL}. Model identifier used by \code{cal_metrics()}/
#'   \code{get_output_wq()}. If \code{NULL} (default), auto-derived from \code{model}.
#' @param yaml_file_model Character or \code{NULL}. GOTM yaml filename, only used/required when
#'   \code{model} is \code{"GOTM-WET"} or \code{"GOTM-Selmaprotbas"}. \code{NULL} (default) falls
#'   back to \code{"gotm.yaml"}.
#' @param par_file Character or \code{NULL}. Simstrat par filename, only used/required when
#'   \code{model} is \code{"Simstrat-AED2"}. \code{NULL} (default) falls back to \code{"simstrat.par"}.
#' @param wq_config_file Character or \code{NULL}. Path to the LakeEnsemblR_WQ config file, passed
#'   through to \code{cal_metrics()}. Required when \code{output_mode = "metrics"}; ignored when
#'   \code{output_mode = "raw"}.
#' @param output_mode Character. \code{"metrics"} (default) runs each step through
#'   \code{cal_metrics()} (requires \code{wq_config_file}). \code{"raw"} instead pulls one or more
#'   model output variables directly via \code{get_output_wq()} (requires \code{vars}), bypassing
#'   the metrics dictionary/template machinery entirely.
#' @param vars Character vector. Required when \code{output_mode = "raw"} -- the model output
#'   variable name(s) to extract at each step, passed through to \code{get_output_wq()}.
#' @param obs_depths Numeric vector or \code{NULL}. Only used when \code{output_mode = "raw"};
#'   passed through to \code{get_output_wq()}.
#' @param depth_01 Integer. Only used when \code{output_mode = "raw"}; passed through to
#'   \code{get_output_wq()}.
#' @param conversion_factor Numeric. Only used when \code{output_mode = "raw"}; passed through to
#'   \code{get_output_wq()}.
#' @param target_variable Character vector or \code{NULL}. Only used when
#'   \code{output_mode = "metrics"}. If supplied, filters the \code{cal_metrics()} output for each
#'   step down to just these metric name(s) instead of returning every metric in \code{yaml_file}.
#' @param verbose Logical. Print progress messages and pass through to the model engine's own
#'   \code{verbose} argument. Default \code{TRUE}.
#'
#' @return A list of length \code{n_steps}. Each element contains \code{params} (the named list of
#'   sampled values for that iteration) plus either \code{metrics} (\code{output_mode = "metrics"})
#'   or \code{output} (\code{output_mode = "raw"}) -- see \code{\link{run_sensitivity}} for the
#'   exact shape of each.
#'
#' @importFrom lhs randomLHS
#' @importFrom readr read_csv write_csv
#' @importFrom glmtools read_nml set_nml write_nml
#' @importFrom LakeEnsemblR input_yaml_multiple
#' @export
#'
#' @examples
#' \dontrun{
#' # Joint sensitivity of DO to 10 parameters at once, GOTM-WET, raw output
#' results <- run_multi_param_sensitivity(
#'   param_names = c("hO2Nitr", "kNitrW", "cTurbDifO2", "kO2Dif", "fRedMaxS",
#'                   "kDMinW", "kDMinS", "hO2BOD", "cMuMax", "kDResp"),
#'   calib_setup = calib_setup,
#'   yaml_file   = "Output.yaml",
#'   model_dir   = "GOTM-WET",
#'   n_steps     = 20,
#'   model       = "GOTM-WET",
#'   output_mode = "raw",
#'   vars        = "sO2W"
#' )
#' }

run_multi_param_sensitivity <- function(param_names = NULL, calib_setup, rel_change = NULL,
                                        yaml_file, model_dir, n_steps = 10, model = "GLM-AED2",
                                        model_filter = NULL, yaml_file_model = NULL,
                                        par_file = NULL, wq_config_file = NULL,
                                        output_mode = "metrics", vars = NULL, obs_depths = NULL,
                                        depth_01 = 1, conversion_factor = 1,
                                        target_variable = NULL, verbose = TRUE) {

  if (is.null(param_names) || length(param_names) == 0L) {
    param_names <- calib_setup$pars
    if (isTRUE(verbose)) {
      message("[Sensitivity] param_names not supplied -- using all ", length(param_names),
              " parameter(s) from calib_setup$pars: ", paste(param_names, collapse = ", "))
    }
  }

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
         "variable(s) to extract at each step.")
  }

  # Auto-derive model_filter from model if not provided.
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

  missing_params <- setdiff(param_names, calib_setup$pars)
  if (length(missing_params) > 0) {
    stop("The following parameters were not found in calib_setup$pars: ",
         paste(missing_params, collapse = ", "))
  }

  # Disambiguate duplicate `pars` names across groups -- e.g. one physical
  # parameter calibrated independently for two phytoplankton groups -- the
  # same occurrence-matching approach run_lhc_wq() uses, so each position in
  # param_names is pinned to its own distinct calib_setup row instead of
  # every duplicate-named row collapsing onto whichever is found first.
  .occurrence_index <- function(x) stats::ave(seq_along(x), x, FUN = seq_along)
  .pname_occurrence  <- .occurrence_index(param_names)
  .setup_occurrence  <- .occurrence_index(calib_setup$pars)
  row_for_param <- vapply(seq_along(param_names), function(j) {
    p   <- param_names[j]
    occ <- .pname_occurrence[j]
    cand <- which(calib_setup$pars == p & .setup_occurrence == occ)
    if (length(cand) == 0L) cand <- which(calib_setup$pars == p)
    cand[1]
  }, integer(1))
  param_key <- make.unique(param_names)

  results <- list()
  n_params <- length(param_names)

  # Generate LHS matrix
  lhs_matrix <- lhs::randomLHS(n_steps, n_params)  # values in [0,1]

  # Prepare bounds for each parameter, indexed positionally by row_for_param
  # (not by name -- see above).
  bounds <- lapply(seq_along(param_names), function(j) {
    row <- calib_setup[row_for_param[j], , drop = FALSE]
    if (!is.null(rel_change)) {
      if (is.null(row$x0) || is.na(row$x0[1])) {
        stop("rel_change was supplied but calib_setup$x0 is missing/NA for parameter '",
             param_names[j], "'.")
      }
      c(min = row$x0[1] * (1 - rel_change), max = row$x0[1] * (1 + rel_change))
    } else {
      c(min = row$lb[1], max = row$ub[1])
    }
  })
  names(bounds) <- param_key

  for (i in seq_len(n_steps)) {
    cat("\nIteration", i, ":\n")

    param_values <- setNames(
      vapply(seq_along(param_names), function(j) {
        min_val <- bounds[[j]]["min"]
        max_val <- bounds[[j]]["max"]
        lhs_matrix[i, j] * (max_val - min_val) + min_val
      }, numeric(1)),
      param_key
    )
    for (j in seq_along(param_names)) {
      cat(" -", param_key[j], "=", round(param_values[j], 6), "\n")
    }

    # Apply all parameter updates for this iteration.
    for (j in seq_along(param_names)) {
      p <- param_names[j]
      row <- calib_setup[row_for_param[j], , drop = FALSE]
      file_or_path <- as.character(row$file[1])
      value <- param_values[[j]]

      if (model_upper %in% c("GOTM-WET", "GOTM-SELMAPROTBAS")) {
        # calib_setup$file holds a FABM instance/key path (e.g.
        # "abiotic_water/parameters/hO2Nitr"), not a literal file name --
        # the target file is always fabm.yaml (fallback: lake_ensemblr.yaml).
        yaml_target <- file.path(model_dir, "fabm.yaml")
        if (!file.exists(yaml_target)) yaml_target <- file.path(model_dir, "lake_ensemblr.yaml")

        path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
        group_col <- if ("group_name" %in% names(row)) row$group_name[1] else NA_character_
        if (!is.na(group_col)) path_parts[path_parts == "{group_name}"] <- group_col

        arglist <- as.list(path_parts)
        names(arglist) <- paste0("key", seq_along(path_parts))
        arglist$value <- value
        arglist$file <- yaml_target
        arglist$verbose <- FALSE
        do.call(LakeEnsemblR::input_yaml_multiple, args = arglist)

      } else if (grepl("\\.nml$", file_or_path, ignore.case = TRUE)) {
        param_path <- file.path(model_dir, file_or_path)
        nml <- glmtools::read_nml(param_path)
        nml <- glmtools::set_nml(nml, p, value)
        glmtools::write_nml(nml, param_path)

      } else if (grepl("\\.csv$", file_or_path, ignore.case = TRUE)) {
        param_path <- file.path(model_dir, file_or_path)
        df <- readr::read_csv(param_path, show_col_types = FALSE)
        names(df) <- gsub("^['\"]|['\"]$", "", names(df))
        pname_col <- intersect(c("p_name", "pname"), names(df))
        if (length(pname_col) == 0) stop("Could not identify parameter name column ('p_name' or 'pname') in CSV")
        df[[pname_col]] <- gsub("^['\"]|['\"]$", "", df[[pname_col]])
        idx <- which(df[[pname_col]] == p)
        if (length(idx) == 0) stop("Parameter not found in CSV file: ", p)

        group_col <- if ("group_name" %in% names(row)) row$group_name[1] else NA_character_
        if (!is.na(group_col) && group_col %in% names(df)) {
          df[idx, group_col] <- value
        } else {
          df[idx, 2:ncol(df)] <- value
        }
        readr::write_csv(df, param_path)

      } else {
        stop("Unsupported file type for model '", model, "': ", file_or_path)
      }
    }

    # Run the model.
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
      results[[i]] <- list(params = as.list(param_values), output = output)
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
      results[[i]] <- list(params = as.list(param_values), metrics = metrics)
    }
  }

  return(results)
}
