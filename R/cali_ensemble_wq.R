#' Calibrate an ensemble of LakeEnsemblR.WQ models
#'
#' High-level calibration wrapper inspired by LakeEnsemblR
#' \code{cali_ensemble()}, implemented for the LakeEnsemblR.WQ calibration
#' workflow. The function runs \code{run_lhc_wq()} model-by-model, using either
#' a combined \code{calib_setup} table (with \code{model_coupled}) or a named
#' list of per-model setup tables.
#'
#' @param models Character vector of models to calibrate. Supported values are
#'   \code{"GLM-AED2"}, \code{"GOTM-WET"}, \code{"GOTM-Selmaprotbas"}, and
#'   \code{"Simstrat-AED2"}. Matching is case-insensitive.
#' @param calib_setup Either a data frame in \code{calib_setup} format or a
#'   named list of such data frames. If a data frame is supplied and contains
#'   \code{model_coupled}, rows are split by model.
#' @param yaml_file Character scalar, vector, or named list. Path(s) to metric
#'   YAML file(s) passed to \code{run_lhc_wq()}.
#' @param folder Character. Base folder used to resolve relative paths and for
#'   optional write-back operations.
#' @param model_dirs Character vector/list of model directories. Can be scalar
#'   (reused), length \code{length(models)} (positional), or named by model.
#'   Defaults to \code{file.path(folder, models)}.
#' @param param_names Optional. Either a character vector reused for all models,
#'   or a named list/vector by model. If \code{NULL}, uses
#'   \code{unique(calib_setup$pars)} per model.
#' @param model_filter Optional model filter(s) passed to \code{run_lhc_wq()}.
#'   Accepts scalar, length-\code{length(models)} vector, or named vector/list.
#' @param wq_config_file Character scalar, vector, or named list of WQ config
#'   path(s) passed to \code{run_lhc_wq()}.
#' @param n_samples Integer. Number of LHS samples per model.
#' @param yaml_file_model Character scalar/vector/list. GOTM YAML file name(s)
#'   passed to \code{run_lhc_wq()}.
#' @param par_file Character scalar/vector/list. Simstrat \code{.par} file
#'   name(s) passed to \code{run_lhc_wq()}.
#' @param obs_file Optional observed-data CSV path. When supplied, each model
#'   returns the flattened stats data.frame from \code{run_lhc_wq()}.
#' @param obs_to_model_units Logical. Passed to \code{run_lhc_wq()}.
#' @param spin_up_days Numeric or \code{NULL}. Passed to \code{run_lhc_wq()}.
#' @param stats_by_depth Logical. Passed to \code{run_lhc_wq()}.
#' @param return_best Logical. Passed to \code{run_lhc_wq()}.
#' @param best_metric Character. Passed to \code{run_lhc_wq()}.
#' @param parallel Logical. Passed to \code{run_lhc_wq()}.
#' @param n_workers Integer or \code{NULL}. Passed to \code{run_lhc_wq()}.
#' @param parallel_dir Character. Passed to \code{run_lhc_wq()}.
#' @param keep_worker_dirs Logical. Passed to \code{run_lhc_wq()}.
#' @param verbose Logical. Print progress messages.
#' @param on_error Character. One of \code{"skip"} or \code{"stop"}.
#' @param save_results Logical. If \code{TRUE}, save each model result as RDS.
#' @param output_dir Character. Directory for saved RDS files when
#'   \code{save_results = TRUE}. Defaults to \code{folder}.
#' @param output_prefix Character. Prefix for saved result files when
#'   \code{save_results = TRUE}.
#' @param write_best Logical. If \code{TRUE}, call
#'   \code{write_best_calib_to_par_files()} for each successful model.
#' @param write_target Character. Passed to
#'   \code{write_best_calib_to_par_files()}.
#' @param config_file Character. Required when \code{write_best = TRUE}; path to
#'   the WQ master config used by \code{write_best_calib_to_par_files()}.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{Named list of per-model outputs from \code{run_lhc_wq()}.}
#'   \item{summary}{Data frame with one row per requested model and run status.}
#'   \item{best_parameter_sets}{Named list of per-model best summaries (or \code{NULL}).}
#'   \item{write_back}{Named list with best-write status per model (when enabled).}
#' }
#' @export
cali_ensemble_wq <- function(models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2"),
                             calib_setup,
                             yaml_file,
                             folder = ".",
                             model_dirs = NULL,
                             param_names = NULL,
                             model_filter = NULL,
                             wq_config_file = NULL,
                             n_samples = 50,
                             yaml_file_model = "gotm.yaml",
                             par_file = "simstrat.par",
                             obs_file = NULL,
                             obs_to_model_units = TRUE,
                             spin_up_days = NULL,
                             stats_by_depth = FALSE,
                             return_best = TRUE,
                             best_metric = "KGE",
                             parallel = FALSE,
                             n_workers = NULL,
                             parallel_dir = tempdir(),
                             keep_worker_dirs = FALSE,
                             verbose = TRUE,
                             on_error = c("skip", "stop"),
                             save_results = FALSE,
                             output_dir = NULL,
                             output_prefix = "lhc_results",
                             write_best = FALSE,
                             write_target = c("par_file", "config"),
                             config_file = NULL) {

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
  msg <- function(...) if (isTRUE(verbose)) message(...)

  on_error <- match.arg(on_error)
  write_target <- match.arg(write_target)

  .canon_model <- function(x) {
    x0 <- toupper(gsub("[_ ]", "-", trimws(as.character(x))))
    map <- c(
      "GLM-AED2" = "GLM-AED2",
      "GOTM-WET" = "GOTM-WET",
      "GOTM-SELMAPROTBAS" = "GOTM-Selmaprotbas",
      "GOTM-SELMA" = "GOTM-Selmaprotbas",
      "SIMSTRAT-AED2" = "Simstrat-AED2"
    )
    out <- unname(map[x0])
    ifelse(is.na(out), as.character(x), out)
  }

  .model_key <- function(x) {
    toupper(gsub("[_ ]", "-", trimws(as.character(x))))
  }

  .resolve_by_model <- function(x, models, arg_name) {
    if (is.null(x)) {
      return(stats::setNames(as.list(rep(list(NULL), length(models))), models))
    }

    model_keys <- .model_key(models)

    if (is.list(x)) {
      if (!is.null(names(x)) && any(nzchar(names(x)))) {
        x_keys <- .model_key(names(x))
        out <- vector("list", length(models))
        names(out) <- models
        for (i in seq_along(models)) {
          hit <- which(x_keys == model_keys[i])[1]
          out[[i]] <- if (is.na(hit)) NULL else x[[hit]]
        }
        return(out)
      }
      if (length(x) == 1L) {
        return(stats::setNames(rep(x, length(models)), models))
      }
      if (length(x) == length(models)) {
        names(x) <- models
        return(x)
      }
      stop("'", arg_name, "' must be scalar, length(models), or named by model.")
    }

    if (length(x) == 1L) {
      return(stats::setNames(as.list(rep(x, length(models))), models))
    }

    if (length(x) == length(models) && is.null(names(x))) {
      out <- as.list(x)
      names(out) <- models
      return(out)
    }

    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      x_keys <- .model_key(names(x))
      out <- vector("list", length(models))
      names(out) <- models
      for (i in seq_along(models)) {
        hit <- which(x_keys == model_keys[i])[1]
        out[[i]] <- if (is.na(hit)) NULL else x[[hit]]
      }
      return(out)
    }

    stop("'", arg_name, "' must be scalar, length(models), or named by model.")
  }

  models <- vapply(models, .canon_model, character(1))
  supported <- c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2")
  bad <- setdiff(models, supported)
  if (length(bad) > 0L) {
    stop("Unsupported model(s): ", paste(bad, collapse = ", "),
         "\nSupported models: ", paste(supported, collapse = ", "))
  }

  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (isTRUE(save_results) && is.null(output_dir)) {
    output_dir <- folder
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (isTRUE(write_best) && is.null(config_file)) {
    stop("'config_file' must be provided when write_best = TRUE.")
  }

  if (isTRUE(write_best) && is.null(obs_file)) {
    stop("'obs_file' must be provided when write_best = TRUE.")
  }

  # Resolve per-model arguments.
  yaml_by_model <- .resolve_by_model(yaml_file, models, "yaml_file")
  wq_cfg_by_model <- .resolve_by_model(wq_config_file, models, "wq_config_file")
  filter_by_model <- .resolve_by_model(model_filter, models, "model_filter")
  yaml_model_by_model <- .resolve_by_model(yaml_file_model, models, "yaml_file_model")
  par_file_by_model <- .resolve_by_model(par_file, models, "par_file")

  if (is.null(model_dirs)) {
    model_dirs <- stats::setNames(file.path(folder, models), models)
  }
  model_dir_by_model <- .resolve_by_model(model_dirs, models, "model_dirs")

  # Resolve calibration setup per model.
  setup_by_model <- vector("list", length(models))
  names(setup_by_model) <- models

  if (is.list(calib_setup) && !is.data.frame(calib_setup)) {
    setup_by_model <- .resolve_by_model(calib_setup, models, "calib_setup")
  } else if (is.data.frame(calib_setup)) {
    if ("model_coupled" %in% names(calib_setup)) {
      cs_model <- vapply(calib_setup$model_coupled, .canon_model, character(1))
      for (m in models) {
        setup_by_model[[m]] <- calib_setup[cs_model == m, , drop = FALSE]
      }
    } else {
      if (length(models) != 1L) {
        stop("When 'calib_setup' is one data.frame for multiple models, it must include a 'model_coupled' column.")
      }
      setup_by_model[[models[1]]] <- calib_setup
    }
  } else {
    stop("'calib_setup' must be either a data.frame or a named list of data.frames.")
  }

  # Resolve per-model parameter names.
  param_by_model <- vector("list", length(models))
  names(param_by_model) <- models
  if (is.null(param_names)) {
    for (m in models) {
      cs <- setup_by_model[[m]]
      param_by_model[[m]] <- if (is.null(cs) || nrow(cs) == 0L) character(0) else unique(cs$pars)
    }
  } else if (is.list(param_names)) {
    param_by_model <- .resolve_by_model(param_names, models, "param_names")
  } else {
    for (m in models) {
      param_by_model[[m]] <- as.character(param_names)
    }
  }

  results <- stats::setNames(vector("list", length(models)), models)
  best_sets <- stats::setNames(vector("list", length(models)), models)
  write_back <- stats::setNames(vector("list", length(models)), models)
  summary_rows <- vector("list", length(models))

  for (i in seq_along(models)) {
    m <- models[i]
    cs <- setup_by_model[[m]]
    ps <- param_by_model[[m]]
    mdir <- model_dir_by_model[[m]]
    if (is.null(mdir)) {
      mdir <- file.path(folder, m)
    }
    mdir <- as.character(mdir[1])
    if (!grepl("^([A-Za-z]:|/)", mdir)) {
      mdir <- file.path(folder, mdir)
    }

    if (is.null(cs) || nrow(cs) == 0L) {
      msg("[cali_ensemble_wq] Skipping ", m, ": no calib_setup rows.")
      summary_rows[[i]] <- data.frame(
        model = m,
        success = FALSE,
        message = "No calib_setup rows for model",
        n_rows = NA_integer_,
        n_stats = NA_integer_,
        sample_index_best = NA_integer_,
        stringsAsFactors = FALSE
      )
      if (on_error == "stop") {
        stop("No calib_setup rows for model: ", m)
      }
      next
    }

    if (length(ps) == 0L) {
      msg("[cali_ensemble_wq] Skipping ", m, ": no parameter names.")
      summary_rows[[i]] <- data.frame(
        model = m,
        success = FALSE,
        message = "No parameter names for model",
        n_rows = NA_integer_,
        n_stats = NA_integer_,
        sample_index_best = NA_integer_,
        stringsAsFactors = FALSE
      )
      if (on_error == "stop") {
        stop("No parameter names for model: ", m)
      }
      next
    }

    msg("[cali_ensemble_wq] Running model: ", m,
        " (", length(ps), " parameters, n_samples=", n_samples, ")")

    # GLM/SIMSTRAT parameter files are edited in-place during each LHC run.
    # Running parallel workers against one shared model_dir can cause file
    # collisions and intermittent parse failures (e.g., malformed .nml files).
    # Keep GOTM models parallel, but force per-model sequential execution for
    # GLM-AED2 and Simstrat-AED2 unless the user explicitly calls run_lhc_wq()
    # with an isolated worker-copy workflow.
    parallel_i <- isTRUE(parallel)
    n_workers_i <- n_workers
    if (parallel_i && m %in% c("GLM-AED2", "Simstrat-AED2")) {
      msg("[cali_ensemble_wq] Parallel disabled for ", m,
          " to avoid in-place file write collisions. Running sequentially.")
      parallel_i <- FALSE
      n_workers_i <- NULL
    }

    yaml_i <- yaml_by_model[[m]] %||% yaml_file[1]
    yaml_i <- as.character(yaml_i[1])

    wq_cfg_i <- wq_cfg_by_model[[m]] %||% wq_config_file[1]
    if (is.null(wq_cfg_i) || length(wq_cfg_i) == 0L || !nzchar(as.character(wq_cfg_i[1]))) {
      wq_cfg_i <- NULL
    } else {
      wq_cfg_i <- as.character(wq_cfg_i[1])
    }

    model_filter_i <- filter_by_model[[m]]
    if (!is.null(model_filter_i) && length(model_filter_i) > 0L) {
      model_filter_i <- as.character(model_filter_i[1])
    } else {
      model_filter_i <- NULL
    }

    yaml_model_i <- yaml_model_by_model[[m]] %||% yaml_file_model[1]
    yaml_model_i <- as.character(yaml_model_i[1])

    par_file_i <- par_file_by_model[[m]] %||% par_file[1]
    par_file_i <- as.character(par_file_i[1])

    one_result <- tryCatch(
      run_lhc_wq(
        model = m,
        param_names = ps,
        calib_setup = cs,
        yaml_file = yaml_i,
        model_dir = mdir,
        n_samples = n_samples,
        model_filter = model_filter_i,
        wq_config_file = wq_cfg_i,
        yaml_file_model = yaml_model_i,
        par_file = par_file_i,
        verbose = verbose,
        save_results = FALSE,
        output_file = "",
        obs_file = obs_file,
        obs_to_model_units = obs_to_model_units,
        spin_up_days = spin_up_days,
        stats_by_depth = stats_by_depth,
        return_best = return_best,
        best_metric = best_metric,
        parallel = parallel_i,
        n_workers = n_workers_i,
        parallel_dir = parallel_dir,
        keep_worker_dirs = keep_worker_dirs
      ),
      error = function(e) e
    )

    if (inherits(one_result, "error")) {
      msg("[cali_ensemble_wq] Model failed: ", m, " -> ", conditionMessage(one_result))
      summary_rows[[i]] <- data.frame(
        model = m,
        success = FALSE,
        message = conditionMessage(one_result),
        n_rows = NA_integer_,
        n_stats = NA_integer_,
        sample_index_best = NA_integer_,
        stringsAsFactors = FALSE
      )
      if (on_error == "stop") {
        stop("Calibration failed for model ", m, ": ", conditionMessage(one_result))
      }
      next
    }

    results[[m]] <- one_result

    best_row <- attr(one_result, "best_parameter_set")
    best_sets[[m]] <- if (is.data.frame(best_row) && nrow(best_row) > 0L) best_row else NULL

    if (is.data.frame(one_result)) {
      n_stats_val <- suppressWarnings(as.integer(max(one_result$n_stats, na.rm = TRUE)))
      if (!is.finite(n_stats_val)) n_stats_val <- NA_integer_

      best_idx <- NA_integer_
      if (is.data.frame(best_sets[[m]]) && "sample_index" %in% names(best_sets[[m]])) {
        best_idx <- suppressWarnings(as.integer(best_sets[[m]]$sample_index[1]))
      }

      summary_rows[[i]] <- data.frame(
        model = m,
        success = TRUE,
        message = "ok",
        n_rows = nrow(one_result),
        n_stats = n_stats_val,
        sample_index_best = best_idx,
        stringsAsFactors = FALSE
      )
    } else {
      n_ok <- sum(vapply(one_result, function(x) isTRUE(x$model_ok), logical(1)), na.rm = TRUE)
      summary_rows[[i]] <- data.frame(
        model = m,
        success = TRUE,
        message = "ok",
        n_rows = length(one_result),
        n_stats = n_ok,
        sample_index_best = NA_integer_,
        stringsAsFactors = FALSE
      )
    }

    if (isTRUE(save_results)) {
      out_name <- paste0(output_prefix, "_", gsub("[^A-Za-z0-9_.-]", "_", m), ".rds")
      out_path <- file.path(output_dir, out_name)
      saveRDS(one_result, out_path)
      msg("[cali_ensemble_wq] Saved: ", out_path)
    }

    if (isTRUE(write_best)) {
      if (!is.data.frame(one_result) || nrow(one_result) == 0L) {
        write_back[[m]] <- list(ok = FALSE, message = "No obs/stats result to write best values")
      } else {
        wb <- tryCatch({
          write_best_calib_to_par_files(
            lhc_results = one_result,
            calib_setup = cs,
            config_file = config_file,
            folder = folder,
            metric = best_metric,
            minimize = toupper(best_metric[1]) %in% c("RMSE", "NRMSE", "PBIAS"),
            write_target = write_target,
            verbose = verbose
          )
          list(ok = TRUE, message = "ok")
        }, error = function(e) {
          list(ok = FALSE, message = conditionMessage(e))
        })
        write_back[[m]] <- wb
        if (!isTRUE(wb$ok) && on_error == "stop") {
          stop("Best-parameter write-back failed for model ", m, ": ", wb$message)
        }
      }
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  structure(
    list(
      results = results,
      summary = summary_df,
      best_parameter_sets = best_sets,
      write_back = write_back
    ),
    class = "cali_ensemble_wq_result"
  )
}
