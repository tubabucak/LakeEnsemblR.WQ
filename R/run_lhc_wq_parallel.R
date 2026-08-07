#' Run Latin Hypercube Calibration in Parallel
#'
#' Runs \code{run_lhc_wq()} in parallel by distributing LHS samples across
#' multiple workers. Each worker gets its own isolated sandbox copy of
#' \code{model_dir} (under \code{parallel_dir}) and runs/scores its assigned
#' samples there -- \code{model_dir} itself is never written to.
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
#' @param yaml_file_model Character or \code{NULL}. GOTM yaml filename. If
#'   \code{NULL}, derived from \code{ler_config_file} (see \code{run_lhc_wq()}).
#' @param par_file Character or \code{NULL}. Simstrat par filename. If
#'   \code{NULL}, derived from \code{ler_config_file} (see \code{run_lhc_wq()}).
#' @param ler_config_file Character or \code{NULL}. Path to the LakeEnsemblR
#'   config file, used to auto-derive \code{yaml_file_model}/\code{par_file}
#'   when they are not explicitly set. Passed through to \code{run_lhc_wq()}.
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
#' @param target_variables Character vector or \code{NULL}. Passed to
#'   \code{run_lhc_wq()} -- restricts scoring to these
#'   \code{variable_global_name} value(s) from \code{obs_file} instead of
#'   averaging across every observed variable. \code{NULL} (default) uses all.
#' @param n_workers Integer. Number of parallel workers. Defaults to all
#'   physical cores minus one, capped at \code{n_samples}.
#' @param use_de Logical. If \code{TRUE}, run a Differential Evolution
#'   refinement phase (via \code{run_lhc_wq()}) after the parallel LHC phase
#'   completes, seeded from the LHC results already computed here.
#' @param de_parallel Logical. Passed to \code{run_lhc_wq()}'s DE phase --
#'   parallelizes DE evaluations across workers.
#' @param de_n_workers Integer or \code{NULL}. Number of workers for the DE
#'   phase when \code{de_parallel = TRUE}.
#' @param de_iterations Integer. Number of DE generations. Passed to
#'   \code{run_lhc_wq()}'s DE phase.
#' @param de_popsize Integer or \code{NULL}. DE population size. Passed to
#'   \code{run_lhc_wq()}'s DE phase.
#' @param de_f Numeric. DE differential weight. Passed to \code{run_lhc_wq()}'s
#'   DE phase.
#' @param de_cr Numeric. DE crossover probability. Passed to
#'   \code{run_lhc_wq()}'s DE phase.
#' @param de_seed_from_lhc Logical. If \code{TRUE} (default), seed the DE
#'   population from the best parallel-LHC results instead of re-sampling.
#' @param parallel_dir Character or \code{NULL}. Parent directory for the
#'   isolated \code{worker_<pid>_<timestamp>_N/} sandbox copies of
#'   \code{model_dir}. If \code{NULL} (default), uses
#'   \code{dirname(model_dir)} -- a sibling of \code{model_dir} under the
#'   project root, so relative parent-path references in model config files
#'   (e.g. shared meteo forcing files) still resolve correctly.
#' @param keep_worker_dirs Logical. If \code{TRUE}, worker sandbox
#'   directories are left on disk after the run instead of being deleted --
#'   useful for debugging, but accumulates disk usage across repeated runs.
#'   Default \code{FALSE} (deleted automatically).
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
                                yaml_file_model = NULL,
                                par_file        = NULL,
                                ler_config_file = NULL,
                                verbose         = TRUE,
                                save_results    = FALSE,
                                output_file     = "lhc_results.rds",
                                obs_file        = NULL,
                                obs_to_model_units = TRUE,
                                spin_up_days    = NULL,
                                stats_by_depth  = FALSE,
                                return_best     = TRUE,
                                best_metric     = "KGE",
                                target_variables = NULL,
                                n_workers       = NULL,
                                use_de           = FALSE,
                                de_parallel      = FALSE,
                                de_n_workers     = NULL,
                                de_iterations    = 50,
                                de_popsize       = NULL,
                                de_f             = 0.8,
                                de_cr            = 0.9,
                                de_seed_from_lhc = TRUE,
                                parallel_dir    = NULL,
                                keep_worker_dirs = FALSE) {

  # run_lhc_wq() defaults parallel_dir to NULL and passes it through
  # explicitly, which overrides any default= on this function's own
  # signature -- so a plain default here is not enough. Must be resolved
  # here in the body, or worker code below does file.path(NULL, "worker_1")
  # -> character(0) -> if(dir.exists(character(0))) -> "argument is of
  # length zero".
  #
  # Default to a sibling of model_dir (same directory model_dir itself
  # lives in), NOT tempdir(): tempdir() is disconnected from the project
  # root, so sibling support files that live next to model_dir (e.g.
  # LakeEnsemblR_bathymetry_standard.csv, the metrics dict CSV) never get
  # copied in, and any "../" relative reference inside gotm.yaml/fabm.yaml
  # to shared project files can't resolve from an unrelated location.
  if (is.null(parallel_dir)) {
    parallel_dir <- dirname(model_dir)
  }

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
      ler_config_file = ler_config_file,
      verbose         = verbose,
      save_results    = save_results,
      output_file     = output_file,
      obs_file        = obs_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days    = spin_up_days,
      stats_by_depth  = stats_by_depth,
      target_variables = target_variables,
      return_best     = return_best,
      best_metric     = best_metric,
      # Forward DE settings here too -- without this, use_de = TRUE would be
      # silently dropped (defaulting to FALSE) whenever this single-worker
      # fallback is taken, exactly the same silent-skip failure mode fixed
      # below in the multi-worker DE branch.
      use_de           = use_de,
      de_parallel      = de_parallel,
      de_n_workers     = de_n_workers,
      de_iterations    = de_iterations,
      de_popsize       = de_popsize,
      de_f             = de_f,
      de_cr            = de_cr,
      de_seed_from_lhc = de_seed_from_lhc
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

  # Create cluster. PSOCK workers have no console attached, so any
  # message()/cat()/error from inside worker code is silently discarded
  # unless outfile points somewhere -- without this, a worker-side failure
  # just looks like an opaque "checkForRemoteErrors" message with no visible
  # cause.
  worker_log <- tempfile("run_lhc_wq_parallel_workers_", tmpdir = tempdir(), fileext = ".log")
  if (isTRUE(verbose)) {
    message("[LHC] Worker progress/errors written to: ", worker_log)
  }
  cl <- parallel::makeCluster(n_workers, outfile = worker_log)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Load required packages on workers, including THIS package itself.
  # Loading LakeEnsemblR.WQ via library() -- rather than clusterExport()'ing
  # run_lhc_wq() and its internal helpers as bare function objects -- avoids
  # a class of bug where a worker can silently receive a stale copy of the
  # function (observed directly: clusterExport(cl, "run_lhc_wq", ...)
  # shipped an outdated body() to the worker even though the master
  # session's own "run_lhc_wq" was confirmed current, seemingly a
  # devtools::load_all()-vs-clusterExport namespace resolution mismatch).
  # library() gives every worker one canonical, independently-loaded copy
  # of the installed package -- requires devtools::install() (not just
  # load_all()) before testing the parallel path after code changes.
  parallel::clusterEvalQ(cl, {
    for (pkg in c("lhs", "readr", "yaml", "dplyr", "glmtools", "gotmtools",
                  "configr", "ncdf4", "lubridate", "reshape2", "GLM3r",
                  "LakeEnsemblR.WQ")) {
      suppressMessages(require(pkg, character.only = TRUE))
    }
    NULL
  })

  # Worker dir names include a run-unique tag (pid + timestamp), NOT just
  # "worker_1"/"worker_2". A fixed name means a rerun depends on unlink()
  # then file.copy(overwrite=TRUE) fully replacing whatever is already
  # there -- on Windows that silently fails to refresh (no error, just a
  # no-op) if anything holds the old directory/file open, e.g. an editor tab
  # on worker_1/fabm.yaml -- so a rerun can end up silently re-scoring the
  # PREVIOUS run's stale files while looking like it did something.
  # Guaranteed-fresh directories per call sidesteps that class of bug
  # entirely.
  worker_dir_tag <- paste0(Sys.getpid(), "_", format(Sys.time(), "%Y%m%d%H%M%OS3"))

  # Export data to workers. Function objects (run_lhc_wq() and its internal
  # helpers) are NOT exported here -- they now come from library() loading
  # the installed package fresh on each worker (see clusterEvalQ above),
  # which is what avoids the stale-copy problem clusterExport() had.
  parallel::clusterExport(
    cl,
    varlist = c(
      "lhs_matrix", "sample_splits", "model", "param_names", "calib_setup",
      "yaml_file", "model_dir", "model_filter", "wq_config_file",
      "yaml_file_model", "par_file", "ler_config_file", "n_samples", "verbose", "obs_file",
      "obs_to_model_units", "spin_up_days", "stats_by_depth",
      "parallel_dir", "worker_dir_tag",
      "keep_worker_dirs", "best_metric", "return_best", "target_variables"
    ),
    envir = environment()
  )

  if (isTRUE(verbose)) {
    message("[LHC] Starting ", n_workers, " parallel workers...")
  }
result_parts <- parallel::parLapply(cl, seq_len(n_workers), function(worker_idx) {

  # ------------------------------------------------------------
  # Create isolated worker directory
  # ------------------------------------------------------------
  worker_dir <- file.path(parallel_dir, paste0("worker_", worker_dir_tag, "_", worker_idx))

  if (dir.exists(worker_dir)) {
    unlink(worker_dir, recursive = TRUE)
  }
  dir.create(worker_dir, recursive = TRUE)

  # ✅ Copy CONTENTS of model_dir
  file.copy(
    from = list.files(model_dir, full.names = TRUE),
    to   = worker_dir,
    recursive = TRUE,
    overwrite = TRUE
  )

  cat("[Worker", worker_idx, "] using:", worker_dir, "\n")

  # ------------------------------------------------------------
  # Sandbox a worker-local copy of yaml_file (e.g. Output.yaml) with its
  # model_folders entry repointed at worker_dir. Without this, scoring
  # (.cal_lhc_obs_stats() -> get_output_wq() -> load_config(yaml_file))
  # resolves the model's output folder from the ORIGINAL yaml_file, which
  # still points at the shared model_dir -- so every worker would score
  # against the same stale/shared output.nc regardless of which worker
  # actually ran which sample, silently producing identical stats for every
  # LHC iteration despite genuinely different sampled parameters.
  # ------------------------------------------------------------
  worker_yaml_file <- yaml_file
  yaml_file_path <- if (is.character(yaml_file) && nzchar(yaml_file)) normalizePath(yaml_file, winslash = "/", mustWork = FALSE) else NULL

  # Copy sibling support files referenced by the config (bathymetry, metrics
  # dictionary, LER config, etc.) into worker_dir -- these live next to
  # model_dir at the project root, not inside model_dir, so the earlier
  # "copy contents of model_dir" step above never picks them up. Without
  # this, load_config()/downstream code fails with "File not found" for
  # e.g. LakeEnsemblR_bathymetry_standard.csv as soon as it tries to read
  # anything outside model_dir itself.
  copy_config_support <- function(config_path) {
    if (is.null(config_path) || !nzchar(config_path) || !file.exists(config_path)) return()
    cfg <- tryCatch(load_config(config_path), error = function(e) NULL)
    if (is.null(cfg)) return()
    config_root <- dirname(config_path)
    copy_one <- function(src) {
      if (is.null(src) || !nzchar(src) || !file.exists(src)) return()
      src_abs <- normalizePath(src, winslash = "/", mustWork = TRUE)
      root_abs <- normalizePath(config_root, winslash = "/", mustWork = TRUE)
      rel_path <- if (startsWith(src_abs, paste0(root_abs, "/"))) {
        sub(paste0("^", root_abs, "/"), "", src_abs)
      } else {
        basename(src_abs)
      }
      dest_path <- file.path(worker_dir, rel_path)
      # Never let a support-file copy land at "output.yaml"/"Output.yaml"
      # (any case) inside worker_dir. GOTM (and possibly other engines)
      # reserves that exact filename for ITS OWN native output-stream
      # config, already placed there by the earlier "copy contents of
      # model_dir" step. cfg$metric_yaml_file in particular is frequently
      # just this package's own Output.yaml self-referencing its own
      # filename -- copying it here would silently clobber GOTM's file on
      # Windows (case-insensitive filesystem, so "Output.yaml" and
      # "output.yaml" are literally the same file), corrupting it and
      # making the model engine fail with a yaml parse error while
      # model_ok stays TRUE and stats silently re-score stale output.nc.
      if (tolower(basename(dest_path)) == "output.yaml") {
        return()
      }
      dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
      file.copy(src_abs, dest_path, overwrite = TRUE)
    }
    # metric_yaml_file is excluded: it is this package's own Output.yaml
    # config file, already sandboxed separately below under a safe,
    # non-colliding name ("__lhc_worker_output_config.yaml"). Copying it
    # again here under its literal filename is both redundant and the
    # source of the output.yaml collision described above.
    lapply(unique(c(cfg$bathy_file, cfg$LER_config_file, cfg$metrics_dict_file)), copy_one)
  }
  copy_config_support(yaml_file_path)
  wq_config_file_path <- if (is.character(wq_config_file) && nzchar(wq_config_file)) normalizePath(wq_config_file, winslash = "/", mustWork = FALSE) else NULL
  copy_config_support(wq_config_file_path)

  if (!is.null(yaml_file_path) && file.exists(yaml_file_path)) {
    # Name this "__lhc_worker_output_config.yaml", NOT basename(yaml_file)
    # (e.g. "Output.yaml"). Windows filesystems are case-insensitive, and
    # worker_dir already contains a verbatim copy of model_dir's own native
    # config files -- GOTM in particular ships its OWN "output.yaml"
    # (lowercase; controls its NetCDF output streams, unrelated to this
    # package's "Output.yaml"). Writing our sandboxed copy as "Output.yaml"
    # silently clobbers GOTM's "output.yaml" on Windows (same file,
    # different case), corrupting it and making the GOTM engine itself fail
    # with a yaml parse error ("single- and double-quoted strings are not
    # supported") -- which then masquerades as "no obs stats" downstream
    # because the model never actually reruns; the same stale output.nc from
    # the initial directory copy just keeps getting rescored every time.
    worker_yaml_path <- file.path(worker_dir, "__lhc_worker_output_config.yaml")
    file.copy(yaml_file_path, worker_yaml_path, overwrite = TRUE)
    yaml_content <- yaml::read_yaml(worker_yaml_path)

    model_upper_w <- toupper(model)
    model_short_w <- if (grepl("GLM", model_upper_w)) {
      "GLM"
    } else if (grepl("SIMSTRAT", model_upper_w)) {
      "SIMSTRAT"
    } else if (grepl("SELMA", model_upper_w)) {
      "SELMAPROTBAS"
    } else if (grepl("WET", model_upper_w)) {
      "WET"
    } else {
      NA_character_
    }
    model_key_w <- names(yaml_content$model_folders)[toupper(names(yaml_content$model_folders)) == model_short_w][1]
    if (!is.na(model_key_w)) {
      # Keep ONLY the entry for the model actually being run. load_config()
      # validates dir.exists() for every entry in model_folders, and the
      # worker sandbox only ever contains a copy of THIS model's directory
      # -- other models' entries still point at the original project-root
      # paths, which don't exist under worker_dir, so load_config() would
      # stop() on them before the repointed entry is ever used.
      #
      # The original entry typically points at an "Output" SUBFOLDER inside
      # the model dir (e.g. "GOTM-Selmaprotbas/Output"), not the model dir
      # itself -- get_output_wq() does file.path(model_folder, "output.nc")
      # with no subfolder of its own, so it relies entirely on model_folders
      # already including that suffix. Blindly overwriting with worker_dir
      # (dropping the suffix) breaks nc_open() with "No such file or
      # directory" even though the model itself ran successfully. Preserve
      # whatever subpath (if any) followed the original model_dir.
      orig_entry <- as.character(yaml_content$model_folders[[model_key_w]])
      # mustWork = TRUE here (not FALSE): model_dir necessarily already
      # exists at this point (we've been copying its contents into
      # worker_dir above), and resolving it with mustWork = TRUE makes
      # Windows return the path in its REAL on-disk casing rather than
      # verbatim as typed. This matters because the comparison below is
      # case-sensitive (startsWith()) while Windows paths are not -- e.g.
      # a caller passing model_dir = "SIMSTRAT-AED2" when the real folder
      # (and the model_folders entry, e.g. "Simstrat-AED2/output") is
      # actually "Simstrat-AED2" would otherwise silently fail this match,
      # suffix would come back "", and get_output_wq() would look for
      # output.nc directly in worker_dir instead of worker_dir/output --
      # the same "no such file" failure mode as the missing-suffix bug
      # this whole block exists to prevent, just triggered by case instead.
      orig_entry_norm <- normalizePath(orig_entry, winslash = "/", mustWork = FALSE)
      model_dir_norm <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)
      suffix <- if (startsWith(tolower(orig_entry_norm), tolower(paste0(model_dir_norm, "/")))) {
        substring(orig_entry_norm, nchar(model_dir_norm) + 2L)
      } else {
        ""
      }
      yaml_content$model_folders <- yaml_content$model_folders[model_key_w]
      worker_dir_abs <- normalizePath(worker_dir, winslash = "/", mustWork = FALSE)
      yaml_content$model_folders[[model_key_w]] <- if (nzchar(suffix)) {
        file.path(worker_dir_abs, suffix)
      } else {
        worker_dir_abs
      }
    }
    if (is.null(yaml_content$folder) || !nzchar(yaml_content$folder)) {
      yaml_content$folder <- normalizePath(worker_dir, winslash = "/", mustWork = FALSE)
    }
    # load_config() hard-requires files$metric_yaml_file to literally exist
    # on disk at file.path(folder, metric_yaml_file) -- it's almost always
    # this package's own Output.yaml self-referencing its own filename. In
    # the original layout that's harmless (folder = project root, a
    # different directory from the model's own native output.yaml one
    # level down in model_dir). Here folder = worker_dir, the SAME
    # directory GOTM's native output.yaml already got copied into -- so
    # pointing metric_yaml_file at a separate "Output.yaml" copy would
    # collide with it again (see the output.yaml clobbering notes above).
    # Point the self-reference at THIS sandboxed file instead (under its
    # own collision-safe name) -- it's guaranteed to exist right here,
    # no extra copy needed, and no name collision is possible.
    if (!is.null(yaml_content$files) && !is.null(yaml_content$files$metric_yaml_file)) {
      yaml_content$files$metric_yaml_file <- basename(worker_yaml_path)
    }
    yaml::write_yaml(yaml_content, worker_yaml_path)
    worker_yaml_file <- worker_yaml_path
  }

  # ------------------------------------------------------------
  # Run LHC
  # ------------------------------------------------------------
 res <- tryCatch(
  run_lhc_wq(
    model              = model,
    param_names        = param_names,
    calib_setup        = calib_setup,
    yaml_file          = worker_yaml_file,
    model_dir          = worker_dir,
    n_samples          = n_samples,
    model_filter       = model_filter,
    wq_config_file     = wq_config_file,
    yaml_file_model    = yaml_file_model,
    par_file           = par_file,
    ler_config_file    = ler_config_file,
    verbose            = verbose,
    save_results       = FALSE,
    output_file        = NULL,
    obs_file           = obs_file,
    obs_to_model_units = obs_to_model_units,
    spin_up_days       = spin_up_days,
    stats_by_depth     = stats_by_depth,
    return_best        = FALSE,
    best_metric        = best_metric,
    target_variables   = target_variables,
    lhs_matrix         = lhs_matrix,
    sample_indices     = sample_splits[[worker_idx]],
    use_de             = FALSE   # 
  ),
  error = function(e) {
    message("[Worker ", worker_idx, "] ERROR: ", conditionMessage(e))
    return(NULL)
  }
)


  # ✅ Cleanup (optional)
  if (!keep_worker_dirs) {
    unlink(worker_dir, recursive = TRUE)
  }

  res
})

  if (isTRUE(verbose)) {
    message("[LHC] Parallel workers finished. Combining results...")
  }

  # Combine results from all workers in sample order. This always runs --
  # not gated behind use_de or verbose -- because the DE phase below seeds
  # from this combined table instead of re-sampling LHC from scratch.
  if (is.null(obs_file)) {
    # Non-obs mode: list of results
    results <- vector("list", n_samples)
    for (worker_idx in seq_len(n_workers)) {
      worker_results <- result_parts[[worker_idx]]
      worker_sample_indices <- sample_splits[[worker_idx]]
      results[worker_sample_indices] <- worker_results
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

  # ------------------------------------------------------------------------
  # DE phase: seed from the parallel LHC results just combined above instead
  # of re-sampling LHC from scratch. Previously this branch lived *inside*
  # `if (isTRUE(verbose))` above (so use_de = TRUE was silently skipped
  # whenever verbose = FALSE) and called run_lhc_wq() without passing the
  # results just computed here -- which made it generate its own brand new
  # random lhs_matrix and re-run the full LHC loop sequentially before DE,
  # discarding the parallel workers' output entirely and roughly doubling
  # the number of model runs for no benefit.
  # ------------------------------------------------------------------------
  if (isTRUE(use_de)) {
    if (isTRUE(verbose)) {
      message("\n[DE] Starting differential evolution phase after LHC (seeded from parallel LHC results)...")
    }

    # reuse same function but now WITHOUT parallel, seeding from the results
    # already computed by the parallel workers above.
    de_results <- run_lhc_wq(
      model = model,
      param_names = param_names,
      calib_setup = calib_setup,
      yaml_file = yaml_file,
      model_dir = model_dir,
      n_samples = n_samples,
      model_filter = model_filter,
      wq_config_file = wq_config_file,
      yaml_file_model = yaml_file_model,
      par_file = par_file,
      ler_config_file = ler_config_file,
      verbose = verbose,
      save_results = FALSE,
      output_file = NULL,
      obs_file = obs_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days = spin_up_days,
      stats_by_depth = stats_by_depth,
      return_best = return_best,
      best_metric = best_metric,
      target_variables = target_variables,
      lhs_matrix = lhs_matrix,
      precomputed_lhc_results = results,
      parallel = FALSE,
      use_de = TRUE,
      de_parallel = de_parallel,
      de_n_workers = de_n_workers,
      de_iterations = de_iterations,
      de_popsize = de_popsize,
      de_f = de_f,
      de_cr = de_cr,
      de_seed_from_lhc = de_seed_from_lhc
    )

    if (isTRUE(save_results)) {
      out_path <- file.path(model_dir, output_file)
      saveRDS(de_results, out_path)
      if (isTRUE(verbose)) {
        message("[LHC] Results saved to: ", out_path)
      }
    }

    return(de_results)
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