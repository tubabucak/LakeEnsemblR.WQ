#' Run an ensemble of LakeEnsemblR.WQ models selected
#'
#' Runs selected models via their own runner packages, and optionally skipping invalid
#' models with a warning, then optionally post-processes the produced NetCDF outputs.
#'
#' @param config_file Character. Path to master LakeEnsebmlerWQ.yaml configuration file
#' @param models Character vector. Models to be run (e.g. c("GLM-AED","GOTM-WET","GOTM_SELMA","SIMSTRAT-AED2")).
#' @param folder Character. Base folder containing model run directories
#' @param parallel Logical. Run in parallel (not implemented yet).
#' @param ncores Integer. Number of cores (not used yet).
#' @param verbose Logical. Print progress messages.
#' @param on_error Character. "skip" (default) skips invalid/failed models with warnings,
#'   "stop" aborts on first invalid/failed model.
#' @param validate Logical. If TRUE, run model-specific validation first. This may be important to prevent getting error on a later stage
#' @param post_process Logical. If TRUE, call your harmonization/post-processing on successful runs. (not integrated yet)
#'
#' @return A list containing validation results, run results, and NetCDF paths for successful runs.
#' @export
run_ensemble_wq <- function(config_file,
                            models = c("GLM-AED2","GOTM-WET","GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
                            folder = ".",
                            parallel = FALSE,
                            ncores = NULL,
                            verbose = TRUE,
                            on_error = c("skip", "stop"),
                            validate = TRUE,
                            post_process = FALSE) {

  on_error <- match.arg(on_error)

# helper function
  `%||%` <- function(x, y) if (!is.null(x)) x else y


  msg <- function(...) if (isTRUE(verbose)) message(...)

  # ---- 0) Master config validation  ----

  # cfg <- read_master_config_wq(config_file)
  # validate_master_config_wq(cfg, models, folder)
  cfg <- NULL
  #msg("Reading master config: ", config_file)

  # ---- 1) Extract model folders  ----

  model_folders <- stats::setNames(file.path(folder, models), models)


.find_latest_netcdf <- function(sim_folder,
                                candidates = c("output", "results", ".", "Output", "Outputs"),
                                pattern = "\\.nc$") {
  dirs <- unique(file.path(sim_folder, candidates))
  nc_files <- unlist(lapply(dirs, function(p) {
    if (dir.exists(p)) list.files(p, pattern = pattern, full.names = TRUE) else character()
  }), use.names = FALSE)

  if (length(nc_files) == 0) return(NA_character_)
  nc_files[which.max(file.info(nc_files)$mtime)]
}


# Validate the model input files
 get_validator <- function(model) {

  validators <- list(

    `GLM-AED2` = function(sim_folder, cfg = NULL, config_file = NULL, ...) {
      # Call your real validation function
      validate_glm_aed(sim_folder = sim_folder, file = "glm3.nml", verbose = TRUE)
      TRUE
    },

    `GOTM-WET` = function(sim_folder, cfg = NULL, config_file = NULL, ...) {
      validate_gotm_wet(sim_folder = sim_folder, file="gotm.yaml", verbose = TRUE)
      TRUE
    },

    `GOTM-Selmaprotbas` = function(sim_folder, cfg = NULL,  config_file = NULL, ...) {
      validate_gotm_wet(sim_folder = sim_folder, file = "gotm.yaml", verbose = TRUE)
      TRUE
    },

    `SIMSTRAT-AED2` = function(sim_folder, cfg = NULL, config_file = NULL, ...) {
      validate_simstrat(sim_folder = sim_folder, file= "simstrat.par", verbose = TRUE)
      TRUE
    }
  )

  if (!model %in% names(validators)) {
    stop("No validator registered for model: ", model, call. = FALSE)
  }

  validators[[model]]
}

# Run lake models
get_runner <- function(model) {

  runners <- list(

    `GLM-AED2` = function(sim_folder, cfg = NULL, config_file = NULL, verbose = TRUE, ...) {
      if (!requireNamespace("GLM3r", quietly = TRUE)) {
        stop("Package 'GLM3r' is required to run GLM-AED2.", call. = FALSE)
      }
      if (!dir.exists(sim_folder)) stop("Missing sim_folder: ", sim_folder, call. = FALSE)

      res <- GLM3r::run_glm(sim_folder = sim_folder, verbose = verbose, ...)

      nc_path <- .find_latest_netcdf(sim_folder)
      list(model = "GLM-AED2", sim_folder = sim_folder, nc_path = nc_path,
           ok = !is.na(nc_path) && file.exists(nc_path), runner_return = res)
    },

    `GOTM-WET` = function(sim_folder, cfg = NULL, config_file = NULL, verbose = TRUE, yaml_file = "gotm.yaml", args = character(), ...) {
      if (!requireNamespace("WETr", quietly = TRUE)) {
        stop("Package 'WETr' is required to run GOTM-WET.", call. = FALSE)
      }
      if (!dir.exists(sim_folder)) stop("Missing sim_folder: ", sim_folder, call. = FALSE)

      res <- WETr::run_wet(sim_folder = sim_folder, yaml_file = yaml_file, verbose = verbose, args = args)

      nc_path <- .find_latest_netcdf(sim_folder)
      list(model = "GOTM-WET", sim_folder = sim_folder, nc_path = nc_path,
           ok = !is.na(nc_path) && file.exists(nc_path), runner_return = res,
           yaml_file = yaml_file)
    },

    `GOTM-Selmaprotbas` = function(sim_folder, cfg = NULL, config_file = NULL, verbose = TRUE, yaml_file = "gotm.yaml", args = character(), ...) {
      if (!requireNamespace("SelmaprotbasR", quietly = TRUE)) {
        stop("Package 'SelmaprotbasR' is required to run GOTM-SELMA.", call. = FALSE)
      }
      if (!dir.exists(sim_folder)) stop("Missing sim_folder: ", sim_folder, call. = FALSE)

      res <- SelmaprotbasR::run_gotm_sp(sim_folder = sim_folder, yaml_file = yaml_file, verbose = verbose, args = args)

      nc_path <- .find_latest_netcdf(sim_folder)
      list(model = "GOTM-Selmaprotbas", sim_folder = sim_folder, nc_path = nc_path,
           ok = !is.na(nc_path) && file.exists(nc_path), runner_return = res,
           yaml_file = yaml_file)
    },

    `SIMSTRAT-AED2` = function(sim_folder, cfg = NULL, config_file = NULL, verbose = TRUE, par_file = "simstrat.par", ...) {
      if (!requireNamespace("SimstratR", quietly = TRUE)) {
        stop("Package 'SimstratR' is required to run Simstrat-AED2.", call. = FALSE)
      }
      if (!dir.exists(sim_folder)) stop("Missing sim_folder: ", sim_folder, call. = FALSE)

      res <- SimstratR::run_simstrat(sim_folder = sim_folder, par_file = par_file, verbose = verbose, ...)

      nc_path <- .find_latest_netcdf(sim_folder)
      list(model = "SIMSTRAT-AED2", sim_folder = sim_folder, nc_path = nc_path,
           ok = !is.na(nc_path) && file.exists(nc_path), runner_return = res,
           par_file = par_file)
    }
  )

  if (!model %in% names(runners)) {
    stop("No runner registered for model: ", model, call. = FALSE)
  }
  runners[[model]]
}


  # ---- helper: safe execution ----
  safe_call <- function(fun, ...) {
    tryCatch(
      list(ok = TRUE, value = fun(...), error = NULL),
      error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
    )
  }

  # Now validatation of input files----
  validation <- stats::setNames(vector("list", length(models)), models)

  if (isTRUE(validate)) {
    msg("Validating models...")
    for (m in models) {
      vfun <- get_validator(m)
      sim_folder <- model_folders[[m]]

      res <- safe_call(vfun, sim_folder = sim_folder, cfg = cfg, config_file = config_file)
      validation[[m]] <- res

      if (!isTRUE(res$ok)) {
        if (on_error == "stop") {
          stop("Validation failed for ", m, ": ", res$error, call. = FALSE)
        }
      }
    }

    invalid <- vapply(validation, function(x) !isTRUE(x$ok), logical(1))
    if (any(invalid)) {
      warning(
        paste0(
          "Skipping invalid models: ", paste(names(invalid)[invalid], collapse = ", "),
          "\nReasons:\n",
          paste(sprintf("- %s: %s", names(invalid)[invalid],
                        vapply(validation[invalid], `[[`, "", "error")),
                collapse = "\n")
        ),
        call. = FALSE
      )
    }
  } else {
    # If not validating, treat all as valid
    for (m in models) validation[[m]] <- list(ok = TRUE, value = TRUE, error = NULL)
  }

  models_to_run <- names(validation)[vapply(validation, function(x) isTRUE(x$ok), logical(1))]
  if (length(models_to_run) == 0) {
    msg("No valid models to run.")
    return(list(cfg = cfg, validation = validation, run_results = list(), nc_paths = character()))
  }

  # ---- 4) Run phase ----
  if (isTRUE(parallel)) {
    msg("Parallel execution not implemented in this minimal skeleton; running sequentially.")
  }

  msg("Running models: ", paste(models_to_run, collapse = ", "))
  run_results <- stats::setNames(vector("list", length(models_to_run)), models_to_run)



  for (m in models_to_run) {
    msg("▶ Running ", m, " ...")
    rfun <- get_runner(m)
    sim_folder <- model_folders[[m]]

    res <- safe_call(rfun, sim_folder = sim_folder, cfg = cfg, config_file = config_file, verbose = verbose)
    run_results[[m]] <- res

  #   if (!isTRUE(res$ok)) {
  #     if (on_error == "stop") {
  #       stop("Run failed for ", m, ": ", res$error, call. = FALSE)
  #     } else {
  #       warning("Model ", m, " failed during run: ", res$error, call. = FALSE)
  #     }
  #   }
  # }
if (!isTRUE(res$ok)) {
    msg("✖ ", m, " threw an error: ", res$error)
    if (on_error == "stop") stop("Run failed for ", m, ": ", res$error, call. = FALSE)
    next
  }

  # check the runner’s own ok flag
  runner_ok <- isTRUE(res$value$ok)
  nc <- res$value$nc_path %||% NA_character_

  if (runner_ok) {
    msg("✔ Finished ", m, " (NetCDF: ", nc, ")")
  } else {
    msg("⚠ Finished ", m, " but no valid NetCDF detected (nc_path: ", nc, ")")
    if (on_error == "stop") stop("Run did not produce expected outputs for ", m, call. = FALSE)
  }
}
  # Keep only successful run return objects
 # successful_runs <- vapply(run_results, function(x) isTRUE(x$ok) && !is.null(x$value), logical(1))
 successful_runs <- vapply(
  run_results,
  function(x) isTRUE(x$ok) && !is.null(x$value) && isTRUE(x$value$ok),
  logical(1)
)
  run_values <- lapply(run_results[successful_runs], `[[`, "value")

  # Extract nc_paths where available
  nc_paths <- vapply(run_values, function(x) {
    if (!is.null(x$nc_path) && isTRUE(nzchar(x$nc_path)) && file.exists(x$nc_path)) x$nc_path else NA_character_
  }, character(1))
  nc_paths <- nc_paths[!is.na(nc_paths)]

  if (length(nc_paths) == 0) {
    msg("No NetCDF outputs detected from successful runs.")
  } else {
   # msg("NetCDF outputs detected for: ", paste(names(nc_paths), collapse = ", "))
  }

  # ---- 5) Optional post-processing phase (placeholder) ----
  # post_processed <- NULL
  # if (isTRUE(post_process) && length(nc_paths) > 0) {
  #   msg("Post-processing is enabled (placeholder).")
  #   # TODO: call your harmonization function, e.g.:
  #   # post_processed <- post_process_wq(nc_paths = nc_paths, dictionary = dictionary, out_file = ...)
  # }

  invisible(list(
    cfg = cfg,
    model_folders = model_folders,
    validation = validation,
    run_results = run_results,
    successful_models = names(run_values),
    nc_paths = nc_paths
   # post_processed = post_processed
  ))
}
