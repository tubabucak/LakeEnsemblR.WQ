#' Run Latin Hypercube Calibration in Parallel
#'
#' Runs \code{run_lhc_wq()} in parallel by distributing LHS samples across
#' multiple workers. Each worker executes its assigned samples sequentially
#' on the shared model directory.
#'
#' @param model Character. One of \code{"GLM-AED2"}, \code{"GOTM-WET"},
#'   \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"}.
#' @param param_names Character vector. Parameter names to vary.
#' @param calib_setup Data frame with calibration bounds and target files.
#' @param yaml_file Character. Path to the output metrics YAML file.
#' @param model_dir Character. Path to the model simulation directory.
#' @param n_samples Integer. Number of LHS samples to run.
#' @param model_filter Character or \code{NULL}. Optional model key for
#'   \code{cal_metrics()}.
#' @param wq_config_file Character or \code{NULL}. Path to WQ config file.
#' @param yaml_file_model Character. GOTM yaml filename.
#' @param par_file Character. Simstrat par filename.
#' @param verbose Logical. Print progress messages.
#' @param save_results Logical. If \code{TRUE}, save the combined result to
#'   \code{output_file} in the original \code{model_dir}.
#' @param output_file Character. RDS output filename when
#'   \code{save_results = TRUE}.
#' @param obs_file Character or \code{NULL}. Optional observed-data CSV.
#' @param obs_to_model_units Logical. Passed to \code{run_lhc_wq()} when
#'   \code{obs_file} is provided. If \code{TRUE} (default), observed values
#'   are converted from harmonized/global units back to model-specific units
#'   before computing statistics.
#' @param spin_up_days Numeric or \code{NULL}. Passed to \code{run_lhc_wq()}.
#'   Number of days after simulation start to exclude from observed-data
#'   comparison in \code{obs_file} mode.
#' @param stats_by_depth Logical. Passed to \code{run_lhc_wq()} in
#'   \code{obs_file} mode. If \code{TRUE}, compute depth-wise statistics.
#' @param return_best Logical. Passed to \code{run_lhc_wq()} in
#'   \code{obs_file} mode. If \code{TRUE}, mark the best parameter set.
#' @param best_metric Character. Objective metric used when
#'   \code{return_best = TRUE}. One of \code{"KGE"}, \code{"NSE"},
#'   \code{"RMSE"}, \code{"NRMSE"}, or \code{"PBIAS"}.
#' @param n_workers Integer. Number of parallel workers. Defaults to all
#'   physical cores minus one, capped at \code{n_samples}.
#' @param parallel_dir Character. Ignored (for backward compatibility).
#' @param keep_worker_dirs Logical. Ignored (for backward compatibility).
#'
#' @return Same structure as \code{run_lhc_wq()}, with results combined across
#'   workers in iteration order.
#' @export
run_lhc_wq_parallel <- function(model,
                                param_names,
                                calib_setup,
                                yaml_file,
                                model_dir,
                                n_samples       = 50,
                                model_filter    = NULL,
                                wq_config_file  = NULL,
                                yaml_file_model = "gotm.yaml",
                                par_file        = "simstrat.par",
                                verbose         = TRUE,
                                save_results    = FALSE,
                                output_file     = "lhc_results.rds",
                                obs_file        = NULL,
                                obs_to_model_units = TRUE,
                                spin_up_days    = NULL,
                                stats_by_depth  = FALSE,
                                return_best     = TRUE,
                                best_metric     = "KGE",
                                n_workers       = NULL,
                                parallel_dir    = tempdir(),
                                keep_worker_dirs = FALSE) {

  # Determine number of workers
  if (is.null(n_workers)) {
    n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
  }
  n_workers <- max(1L, min(as.integer(n_workers[1]), as.integer(n_samples)))

  if (isTRUE(verbose)) {
    message("[LHC] Parallel setup: n_samples=", n_samples,
            ", n_workers=", n_workers)
  }

  # If only one worker, just run sequentially
  if (n_workers <= 1L) {
    if (isTRUE(verbose)) {
      message("[LHC] Only 1 worker requested; running sequentially.")
    }
    return(run_lhc_wq(
      model           = model,
      param_names     = param_names,
      calib_setup     = calib_setup,
      yaml_file       = yaml_file,
      model_dir       = model_dir,
      n_samples       = n_samples,
      model_filter    = model_filter,
      wq_config_file  = wq_config_file,
      yaml_file_model = yaml_file_model,
      par_file        = par_file,
      verbose         = verbose,
      save_results    = save_results,
      output_file     = output_file,
      obs_file        = obs_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days    = spin_up_days,
      stats_by_depth  = stats_by_depth,
      return_best     = return_best,
      best_metric     = best_metric
    ))
  }

  # Generate the full LHS matrix once
  n_params <- length(param_names)
  lhs_matrix <- lhs::randomLHS(n_samples, n_params)

  # Split samples across workers
  sample_splits <- split(seq_len(n_samples), 
                        rep(seq_len(n_workers), length.out = n_samples))

  if (isTRUE(verbose)) {
    split_msg <- vapply(seq_along(sample_splits), function(i) {
      paste0("w", i, "=", length(sample_splits[[i]]), " samples")
    }, character(1))
    message("[LHC] Worker assignment: ", paste(split_msg, collapse = ", "))
  }

  # Create cluster
  cl <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Load required packages on workers
  parallel::clusterEvalQ(cl, {
    for (pkg in c("lhs", "readr", "yaml", "dplyr", "glmtools", "gotmtools",
                  "configr", "ncdf4", "lubridate", "reshape2", "GLM3r")) {
      suppressMessages(require(pkg, character.only = TRUE))
    }
    NULL
  })

  # Export functions and data to workers
  parallel::clusterExport(
    cl,
    varlist = c(
      "run_lhc_wq", ".cal_lhc_obs_stats", "cal_metrics", "cal_stats",
      "load_config", "get_output_wq", "expand_templates",
      ".load_metrics_dictionary_wq",
      "lhs_matrix", "sample_splits", "model", "param_names", "calib_setup",
      "yaml_file", "model_dir", "model_filter", "wq_config_file",
      "yaml_file_model", "par_file", "verbose", "obs_file",
      "obs_to_model_units", "spin_up_days", "stats_by_depth"
    ),
    envir = environment()
  )

  if (isTRUE(verbose)) {
    message("[LHC] Starting ", n_workers, " parallel workers...")
  }

  # Each worker runs its subset of samples
  result_parts <- parallel::parLapply(cl, seq_len(n_workers), function(worker_idx) {
    run_lhc_wq(
      model              = model,
      param_names        = param_names,
      calib_setup        = calib_setup,
      yaml_file          = yaml_file,
      model_dir          = model_dir,
      n_samples          = n_samples,
      model_filter       = model_filter,
      wq_config_file     = wq_config_file,
      yaml_file_model    = yaml_file_model,
      par_file           = par_file,
      verbose            = verbose,
      save_results       = FALSE,
      output_file        = NULL,
      obs_file           = obs_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days       = spin_up_days,
      stats_by_depth     = stats_by_depth,
      return_best        = FALSE,  # Compute globally at end
      best_metric        = best_metric,
      lhs_matrix         = lhs_matrix,
      sample_indices     = sample_splits[[worker_idx]]
    )
  })

  if (isTRUE(verbose)) {
    message("[LHC] Parallel workers finished. Combining results...")
  }

  # Combine results from all workers in sample order
  if (is.null(obs_file)) {
    # Non-obs mode: list of results
    results <- vector("list", n_samples)
    for (worker_idx in seq_len(n_workers)) {
      worker_results <- result_parts[[worker_idx]]
      sample_indices <- sample_splits[[worker_idx]]
      results[sample_indices] <- worker_results
    }
    if (isTRUE(verbose)) {
      n_ok <- sum(vapply(results, function(x) isTRUE(x$model_ok), logical(1)), na.rm = TRUE)
      message("[LHC] Combined ", length(results), " iterations (model_ok=", n_ok, ")")
    }
  } else {
    # Obs mode: combine data frames
    results <- dplyr::bind_rows(result_parts)
    if (isTRUE(verbose)) {
      n_ok <- sum(results$model_ok %in% TRUE, na.rm = TRUE)
      message("[LHC] Combined ", nrow(results), " rows (model_ok=", n_ok, ")")
    }

    # Identify best iteration globally across all workers
    if (isTRUE(return_best) && nrow(results) > 0) {
      best_metric_upper <- toupper(as.character(best_metric[1]))
      valid_best_metrics <- c("KGE", "NSE", "RMSE", "NRMSE", "PBIAS")
      if (!best_metric_upper %in% valid_best_metrics) {
        stop("'best_metric' must be one of: ", paste(valid_best_metrics, collapse = ", "),
             "\nProvided: ", best_metric_upper)
      }

      score_sign <- if (best_metric_upper %in% c("RMSE", "NRMSE")) -1 else 1
      score_transform <- function(x) {
        if (best_metric_upper == "PBIAS") {
          return(-abs(x))
        }
        score_sign * x
      }

      score_rows <- results[results$model_ok %in% TRUE, , drop = FALSE]
      score_rows <- score_rows[!is.na(score_rows[[best_metric_upper]]), , drop = FALSE]

      if (nrow(score_rows) > 0 && "sample_index" %in% names(score_rows)) {
        iter_ids <- sort(unique(score_rows$sample_index))
        iter_scores <- lapply(iter_ids, function(iter_id) {
          sub <- score_rows[score_rows$sample_index == iter_id, , drop = FALSE]
          if (nrow(sub) == 0) return(NULL)

          vals <- score_transform(sub[[best_metric_upper]])
          w <- suppressWarnings(as.numeric(sub$n_pairs))
          w[is.na(w) | w <= 0] <- 1
          ok <- is.finite(vals) & is.finite(w)
          if (!any(ok)) return(NULL)

          objective <- stats::weighted.mean(vals[ok], w = w[ok], na.rm = TRUE)
          first_row <- sub[1, , drop = FALSE]

          out <- first_row[, c("sample_index", "n_stats", "n_obs_vars", 
                               "n_obs_vars_with_stats"), drop = FALSE]
          out$best_metric <- best_metric_upper
          out$objective_score <- objective
          out$objective_value <- if (best_metric_upper == "PBIAS") -objective 
                                 else objective * score_sign
          for (p in param_names) {
            out[[p]] <- first_row[[p]]
          }
          out
        })

        iter_scores <- Filter(Negate(is.null), iter_scores)
        if (length(iter_scores) > 0) {
          score_table <- do.call(rbind, iter_scores)
          best_idx <- which.max(score_table$objective_score)[1]
          best_summary <- score_table[best_idx, , drop = FALSE]
          best_iter <- as.integer(best_summary$sample_index[1])

          results$is_best <- results$sample_index == best_iter
          attr(results, "best_parameter_set") <- best_summary
          attr(results, "best_metric") <- best_metric_upper

          if (isTRUE(verbose)) {
            message("[LHC] Global best parameter set at sample_index=", best_iter,
                    " using ", best_metric_upper)
          }
        } else {
          results$is_best <- FALSE
          attr(results, "best_parameter_set") <- NULL
          attr(results, "best_metric") <- best_metric_upper
        }
      } else {
        results$is_best <- FALSE
        attr(results, "best_parameter_set") <- NULL
        attr(results, "best_metric") <- best_metric_upper
      }
    }
  }

  if (isTRUE(save_results)) {
    out_path <- file.path(model_dir, output_file)
    saveRDS(results, out_path)
    if (isTRUE(verbose)) {
      message("[LHC] Results saved to: ", out_path)
    }
  }

  results
}