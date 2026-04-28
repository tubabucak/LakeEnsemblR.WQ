#' Run Latin Hypercube Calibration for Water Quality Models
#'
#' Performs a Latin Hypercube Sampling (LHS) based parameter space exploration
#' for water quality models coupled to lake hydrodynamic models. Supports
#' GLM-AED2, GOTM-WET, GOTM-Selmaprotbas, and Simstrat-AED2.
#'
#' For each LHS iteration the function:
#' \enumerate{
#'   \item Samples parameter values from the bounds defined in \code{calib_setup}.
#'   \item Writes the sampled values to the relevant \code{.nml} or \code{.csv} files.
#'   \item Runs the selected model.
#'   \item Extracts metrics via \code{\link{cal_metrics}}.
#' }
#'
#' @param model Character. One of \code{"GLM-AED2"}, \code{"GOTM-WET"},
#'   \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"} (case-insensitive).
#' @param param_names Character vector. Names of parameters to vary (must match
#'   the \code{pars} column of \code{calib_setup}).
#' @param calib_setup Data frame. Calibration setup table with at minimum columns:
#'   \code{pars} (parameter names), \code{lb} (lower bound), \code{ub} (upper bound),
#'   \code{file} (path relative to \code{model_dir}), and optionally
#'   \code{group_name} (e.g. for phytoplankton groups in CSV parameter files).
#' @param yaml_file Character. Path to the YAML metrics file passed to
#'   \code{\link{cal_metrics}}.
#' @param model_dir Character. Path to the model simulation directory.
#' @param n_samples Integer. Number of LHS samples (model runs) to perform
#'   (default = 50).
#' @param model_filter Character. Optional model filter string passed to
#'   \code{\link{cal_metrics}}. If \code{NULL} (default), auto-derived from
#'   \code{model} (e.g., "GLM-AED2" → "GLM", "GOTM-WET" → "GOTM").
#' @param wq_config_file Character. Path to the WQ config file passed to
#'   \code{\link{cal_metrics}}.
#' @param yaml_file_model Character. Name of the GOTM yaml file used when
#'   running GOTM-based models (default = \code{"gotm.yaml"}).
#'   Ignored for GLM-AED2 and Simstrat-AED2.
#' @param par_file Character. Name of the Simstrat \code{.par} file
#'   (default = \code{"simstrat.par"}). Ignored for other models.
#' @param verbose Logical. Print progress messages (default = \code{TRUE}).
#' @param save_results Logical. If \code{TRUE}, save the results as an
#'   \code{.rds} file inside \code{model_dir} after all iterations
#'   (default = \code{FALSE}).
#' @param output_file Character. File name for the saved results when
#'   \code{save_results = TRUE} (default = \code{"lhc_results.rds"}).
#' @param obs_file Character or \code{NULL}. Path to the standard observed data
#'   CSV with columns \code{datetime}, \code{depth}, \code{variable_global_name},
#'   and \code{value} (all values in global units as defined in the metrics
#'   dictionary). When provided, each model run is evaluated against the
#'   observations and only a flat \code{data.frame} of parameter values plus
#'   performance statistics is returned (default = \code{NULL}).
#'
#' @return When \code{obs_file = NULL}: a list of length \code{n_samples}.
#'   Each element is a list with:
#' \describe{
#'   \item{\code{params}}{Named list of sampled parameter values for this iteration.}
#'   \item{\code{metrics}}{Output of \code{cal_metrics()} for this iteration.}
#'   \item{\code{model_ok}}{Logical; \code{TRUE} if the model run produced output.}
#' }
#' When \code{obs_file} is provided: a \code{data.frame} where each row is one
#' successful LHC iteration. Columns are the sampled parameter values followed
#' by performance statistics (NSE, RMSE, NRMSE, PBIAS, KGE) for every
#' variable–depth combination present in the observed data.
#'
#' @importFrom lhs randomLHS
#' @importFrom readr read_csv write_csv
#' @importFrom GLM3r run_glm
#'
#' @seealso \code{\link{cal_stats}}
#'
#' @examples
#' \dontrun{
#' results <- run_lhc_wq(
#'   model       = "GLM-AED2",
#'   param_names = c("Kw", "Knitrif"),
#'   calib_setup = calib_setup,
#'   yaml_file   = "metrics.yaml",
#'   model_dir   = "GLM-AED2",
#'   n_samples   = 30,
#'   wq_config_file = "LakeEnsemblR_WQ.yaml"
#' )
#'
#' results <- run_lhc_wq(
#'   model       = "GOTM-Selmaprotbas",
#'   param_names = c("spm_initial", "p_vel"),
#'   calib_setup = calib_setup,
#'   yaml_file   = "metrics.yaml",
#'   model_dir   = "GOTM-Selmaprotbas",
#'   n_samples   = 50,
#'   wq_config_file = "LakeEnsemblR_WQ.yaml"
#' )
#' }
#'
#' @export

# ---------------------------------------------------------------------------
# Internal helper: compare simulated vs. observed and return statistics.
# Simulated output from get_output_wq() is already in global units
# (conversion_factor has been applied: model_units * CF = global_units).
# Observed data is also in global units, so the two series are directly
# comparable without any additional unit conversion.
# ---------------------------------------------------------------------------
.cal_lhc_obs_stats <- function(obs_data, dict, model_short, yaml_file,
                               wq_config_file = NULL) {

  .parse_dt <- function(x) {
    as.POSIXct(
      x,
      tz = "UTC",
      tryFormats = c(
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d"
      )
    )
  }

  .match_obs_sim <- function(obs_df, sim_df) {
    matched <- merge(obs_df, sim_df, by = "datetime")
    if (nrow(matched) >= 2L) {
      return(list(data = matched[, c("value", "sim_value")], mode = "datetime"))
    }

    obs_df$date <- as.Date(obs_df$datetime, tz = "UTC")
    sim_df$date <- as.Date(sim_df$datetime, tz = "UTC")
    obs_df <- obs_df[!is.na(obs_df$date), c("date", "value")]
    sim_df <- sim_df[!is.na(sim_df$date), c("date", "sim_value")]

    # Aggregate to one value per day before joining
    obs_daily <- stats::aggregate(value ~ date, data = obs_df, FUN = mean)
    sim_daily <- stats::aggregate(sim_value ~ date, data = sim_df, FUN = mean)
    matched_daily <- merge(obs_daily, sim_daily, by = "date")

    if (nrow(matched_daily) >= 2L) {
      return(list(data = matched_daily[, c("value", "sim_value")], mode = "date"))
    }

    list(data = NULL, mode = "none")
  }

  .match_obs_sim_depth <- function(obs_df, sim_df) {
    matched <- merge(obs_df, sim_df, by = c("datetime", "depth"))
    if (nrow(matched) >= 2L) {
      return(list(data = matched[, c("value", "sim_value")], mode = "datetime_depth"))
    }

    obs_df$date <- as.Date(obs_df$datetime, tz = "UTC")
    sim_df$date <- as.Date(sim_df$datetime, tz = "UTC")
    obs_df <- obs_df[!is.na(obs_df$date), c("date", "depth", "value")]
    sim_df <- sim_df[!is.na(sim_df$date), c("date", "depth", "sim_value")]

    obs_daily <- stats::aggregate(value ~ date + depth, data = obs_df, FUN = mean)
    sim_daily <- stats::aggregate(sim_value ~ date + depth, data = sim_df, FUN = mean)
    matched_daily <- merge(obs_daily, sim_daily, by = c("date", "depth"))

    if (nrow(matched_daily) >= 2L) {
      return(list(data = matched_daily[, c("value", "sim_value")], mode = "date_depth"))
    }

    list(data = NULL, mode = "none")
  }

  model_dict <- dict[toupper(dict$model) == toupper(model_short), , drop = FALSE]
  if (!is.null(wq_config_file) && nrow(model_dict) > 0) {
    model_dict <- expand_templates(model_dict, wq_config_file)
  }

  if (nrow(model_dict) == 0) {
    return(list())
  }

  unresolved_template <- grepl("\\{group\\}|\\{idx:02d\\}",
                               model_dict$variable_model_name)
  model_dict <- model_dict[!unresolved_template, , drop = FALSE]

  cfg <- load_config(yaml_file)
  model_key <- names(cfg$model_folders)[toupper(names(cfg$model_folders)) == toupper(model_short)][1]
  available_nc_vars <- NULL
  if (!is.na(model_key) && toupper(model_short) != "SIMSTRAT") {
    nc_file <- file.path(cfg$model_folders[[model_key]], "output.nc")
    if (file.exists(nc_file)) {
      nc <- ncdf4::nc_open(nc_file)
      on.exit(ncdf4::nc_close(nc), add = TRUE)
      available_nc_vars <- names(nc$var)
    }
  }

  obs_vars   <- unique(obs_data$variable_global_name)
  stats_out  <- list()

  for (gvar in obs_vars) {
    dict_sub <- model_dict[model_dict$variable_global_name == gvar, , drop = FALSE]
    if (nrow(dict_sub) == 0) next

    obs_sub    <- obs_data[obs_data$variable_global_name == gvar, ]
    obs_depths <- sort(unique(obs_sub$depth))

    depth_01_flag <- as.integer(dict_sub$depth_01[1])
    sim_pieces <- list()

    for (row_idx in seq_len(nrow(dict_sub))) {
      var_model_name <- dict_sub$variable_model_name[row_idx]
      cf <- suppressWarnings(as.numeric(as.character(dict_sub$conversion_factor[row_idx])))
      if (is.na(cf) || cf == 0) cf <- 1

      if (!is.null(available_nc_vars) && !var_model_name %in% available_nc_vars) {
        next
      }

      sim_data <- tryCatch(
        get_output_wq(
          config_file       = yaml_file,
          model             = model_short,
          vars              = var_model_name,
          obs_depths        = if (depth_01_flag == 1L) obs_depths else NULL,
          depth_01          = depth_01_flag,
          conversion_factor = cf
        ),
        error = function(e) NULL
      )

      if (is.null(sim_data) || length(sim_data) == 0) next
      sim_piece <- sim_data[[1]]
      if (is.null(sim_piece) || nrow(sim_piece) == 0) next
      sim_pieces[[length(sim_pieces) + 1]] <- sim_piece
    }

    if (length(sim_pieces) == 0) next

    sim_df <- sim_pieces[[1]]
    if (length(sim_pieces) > 1) {
      for (piece_idx in 2:length(sim_pieces)) {
        piece <- sim_pieces[[piece_idx]]
        shared_cols <- intersect(names(sim_df), names(piece))
        value_cols <- setdiff(shared_cols, "datetime")
        sim_df <- merge(sim_df, piece, by = "datetime", all = FALSE)
        for (col_name in value_cols) {
          col_x <- paste0(col_name, ".x")
          col_y <- paste0(col_name, ".y")
          if (col_x %in% names(sim_df) && col_y %in% names(sim_df)) {
            sim_df[[col_name]] <- sim_df[[col_x]] + sim_df[[col_y]]
            sim_df[[col_x]] <- NULL
            sim_df[[col_y]] <- NULL
          }
        }
      }
    }

    sim_df$datetime  <- .parse_dt(sim_df$datetime)
    obs_sub$datetime <- .parse_dt(obs_sub$datetime)

    if (depth_01_flag == 1L) {
      sim_long <- do.call(
        rbind,
        lapply(obs_depths, function(dep) {
          dep_col <- paste0("Depth_", dep)
          if (!dep_col %in% names(sim_df)) {
            return(NULL)
          }
          out <- sim_df[, c("datetime", dep_col)]
          names(out)[2] <- "sim_value"
          out$depth <- dep
          out
        })
      )

      if (is.null(sim_long) || nrow(sim_long) == 0) {
        next
      }

      obs_long <- obs_sub[, c("datetime", "depth", "value")]
      matched <- .match_obs_sim_depth(obs_long, sim_long)
      if (is.null(matched$data)) next

      st <- cal_stats(matched$data$value, matched$data$sim_value)
      stats_out[[gvar]] <- list(NSE   = st$NSE,   RMSE  = st$RMSE,
                                NRMSE = st$NRMSE, PBIAS = st$PBIAS,
                                KGE   = st$KGE)
    } else {
      # No depth dimension – direct time-series comparison
      obs_ts           <- obs_sub[, c("datetime", "value")]
      sim_ts           <- sim_df[, 1:2]
      names(sim_ts)[2] <- "sim_value"

      matched <- .match_obs_sim(obs_ts, sim_ts)
      if (is.null(matched$data)) next

      st  <- cal_stats(matched$data$value, matched$data$sim_value)
      stats_out[[gvar]] <- list(NSE   = st$NSE,   RMSE  = st$RMSE,
                                 NRMSE = st$NRMSE, PBIAS = st$PBIAS,
                                 KGE   = st$KGE)
    }
  }

  stats_out
}

run_lhc_wq <- function(model,
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
                       obs_file        = NULL) {

  # Input validation
  model_upper <- toupper(model)
  supported <- c("GLM-AED2", "GOTM-WET", "GOTM-SELMAPROTBAS", "SIMSTRAT-AED2")
  if (!model_upper %in% supported) {
    stop("'model' must be one of: ", paste(supported, collapse = ", "),
         "\nProvided: ", model)
  }

  # Auto-derive model_filter from model if not provided
  if (is.null(model_filter)) {
    if (grepl("GLM", model_upper)) {
      model_filter <- "GLM"
    } else if (grepl("GOTM", model_upper)) {
      if (grepl("WET", model_upper)) {
        model_filter <- "GOTM"
      } else if (grepl("SELMA", model_upper)) {
        model_filter <- "SELMAPROTBAS"
      } else {
        model_filter <- "GOTM"
      }
    } else if (grepl("SIMSTRAT", model_upper)) {
      model_filter <- "SIMSTRAT"
    } else {
      model_filter <- "all"
    }
    if (verbose) {
      message("[LHC] Auto-derived model_filter='" , model_filter, "' from model='" , model, "'")
    }
  }

  missing_params <- setdiff(param_names, calib_setup$pars)
  if (length(missing_params) > 0) {
    stop("The following parameters were not found in calib_setup$pars: ",
         paste(missing_params, collapse = ", "))
  }

  required_cols <- c("pars", "lb", "ub", "file")
  missing_cols <- setdiff(required_cols, names(calib_setup))
  if (length(missing_cols) > 0) {
    stop("calib_setup is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!dir.exists(model_dir)) {
    stop("model_dir does not exist: ", model_dir)
  }

  # Model-specific config file checks
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

  # Map full model name to the short model name used in the metrics dictionary
  model_short <- switch(model_upper,
    "GLM-AED2"          = "GLM",
    "GOTM-WET"          = "WET",
    "GOTM-SELMAPROTBAS" = "SELMAPROTBAS",
    "SIMSTRAT-AED2"     = "SIMSTRAT"
  )

  # Load observed data and metrics dictionary once (if obs_file is supplied)
  obs_data_loaded <- NULL
  dict_loaded     <- NULL
  if (!is.null(obs_file)) {
    if (!file.exists(obs_file)) {
      stop("obs_file does not exist: ", obs_file)
    }
    obs_data_loaded <- utils::read.csv(obs_file, stringsAsFactors = FALSE)
    required_obs_cols <- c("datetime", "depth", "variable_global_name", "value")
    missing_obs_cols  <- setdiff(required_obs_cols, names(obs_data_loaded))
    if (length(missing_obs_cols) > 0) {
      stop("obs_file is missing required columns: ",
           paste(missing_obs_cols, collapse = ", "))
    }

    # Load metrics dictionary from configured file or bundled defaults.
    cfg_for_dict <- load_config(yaml_file)
    dict_loaded <- .load_metrics_dictionary_wq(
      dict_file = cfg_for_dict$metrics_dict_file,
      metric_yaml_file = cfg_for_dict$metric_yaml_file
    )

    # Warn early when observed dates do not overlap the model simulation period.
    obs_dates <- as.POSIXct(
      obs_data_loaded$datetime,
      tz = "UTC",
      tryFormats = c(
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d"
      )
    )
    obs_dates <- obs_dates[!is.na(obs_dates)]
    ler_cfg <- yaml::read_yaml(cfg_for_dict$LER_config_file)
    sim_start <- as.POSIXct(ler_cfg$time$start, tz = "UTC")
    sim_stop  <- as.POSIXct(ler_cfg$time$stop,  tz = "UTC")
    if (length(obs_dates) > 0 && !is.na(sim_start) && !is.na(sim_stop)) {
      obs_start <- min(obs_dates)
      obs_stop  <- max(obs_dates)
      has_overlap <- obs_start <= sim_stop && obs_stop >= sim_start
      if (!has_overlap) {
        warning(
          "Observed data dates do not overlap the model simulation period. ",
          "Observed range: ", format(obs_start, "%Y-%m-%d"), " to ", format(obs_stop, "%Y-%m-%d"), "; ",
          "simulation range: ", format(sim_start, "%Y-%m-%d"), " to ", format(sim_stop, "%Y-%m-%d"), ". ",
          "Exact datetime matching will produce no statistics unless the model period is changed or the observations are aligned."
        )
      }
    }
  }

  # LHS matrix in [0, 1]
  n_params <- length(param_names)
  lhs_matrix <- lhs::randomLHS(n_samples, n_params)

  # Pre-collect bounds for each parameter
  bounds <- lapply(param_names, function(p) {
    row <- calib_setup[calib_setup$pars == p, ]
    c(lb = row$lb[1], ub = row$ub[1])
  })
  names(bounds) <- param_names

  # Model runner by coupling
  .run_model <- function() {
    switch(model_upper,
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
        WETr::run_wet(sim_folder = model_dir, yaml_file = yaml_file_model,
                      verbose = verbose)
      },
      "GOTM-SELMAPROTBAS" = {
        if (!requireNamespace("SelmaprotbasR", quietly = TRUE)) {
          stop("Package 'SelmaprotbasR' is required to run GOTM-Selmaprotbas.")
        }
        SelmaprotbasR::run_gotm_sp(sim_folder = model_dir,
                                   yaml_file = yaml_file_model,
                                   verbose = verbose)
      },
      "SIMSTRAT-AED2" = {
        if (!requireNamespace("SimstratR", quietly = TRUE)) {
          stop("Package 'SimstratR' is required to run Simstrat-AED2.")
        }
        SimstratR::run_simstrat(sim_folder = model_dir, par_file = par_file,
                                verbose = verbose)
      }
    )
  }

  # Helper to update one parameter in .nml/.csv/.yaml file
  .update_param <- function(p, value) {
    rows <- calib_setup[calib_setup$pars == p, ]
    param_path <- file.path(model_dir, rows$file[1])

    if (grepl("\\.nml$", rows$file[1], ignore.case = TRUE)) {
      nml <- glmtools::read_nml(param_path)
      nml <- glmtools::set_nml(nml, p, value)
      glmtools::write_nml(nml, param_path)

    } else if (grepl("\\.csv$", rows$file[1], ignore.case = TRUE)) {
      df <- readr::read_csv(param_path, show_col_types = FALSE)
      names(df) <- gsub("^['\"]|['\"]$", "", names(df))
      pname_col <- intersect(c("p_name", "pname"), names(df))
      if (length(pname_col) == 0) {
        stop("Could not identify parameter name column ('p_name' or 'pname') in: ",
             param_path)
      }
      df[[pname_col]] <- gsub("^['\"]|['\"]$", "", df[[pname_col]])

      for (k in seq_len(nrow(rows))) {
        idx <- which(df[[pname_col]] == p)
        if (length(idx) == 0) {
          stop("Parameter '", p, "' not found in ", param_path)
        }

        group_col <- rows$group_name[k]
        if (!is.na(group_col) && group_col %in% names(df)) {
          df[idx, group_col] <- value
        } else {
          df[idx, 2:ncol(df)] <- value
        }
      }
      readr::write_csv(df, param_path)

    } else if (grepl("\\.yaml$|\\.yml$", rows$file[1], ignore.case = TRUE)) {
      gotmtools::set_yaml_value(param_path, label = p, value = value)

    } else {
      stop("Unsupported file type for parameter '", p, "': ", rows$file[1])
    }
  }

  # Main LHC loop
  results <- vector("list", n_samples)

  for (i in seq_len(n_samples)) {
    if (verbose) {
      cat("\n[LHC] Iteration", i, "/", n_samples, "\n")
    }

    # Scale LHS values to [lb, ub]
    param_values <- setNames(
      vapply(seq_along(param_names), function(j) {
        p <- param_names[j]
        lb <- bounds[[p]]["lb"]
        ub <- bounds[[p]]["ub"]
        lhs_matrix[i, j] * (ub - lb) + lb
      }, numeric(1)),
      param_names
    )

    if (verbose) {
      for (p in param_names) {
        cat("  ", p, "=", round(param_values[p], 6), "\n")
      }
    }

    # Write sampled parameters
    for (p in param_names) {
      .update_param(p, param_values[p])
    }

    # Run model
    model_ok <- tryCatch({
      .run_model()
      TRUE
    }, error = function(e) {
      warning("[LHC] Model run failed at iteration ", i, ": ", conditionMessage(e))
      FALSE
    })

    # Extract metrics or observed statistics for successful runs
    metrics   <- NULL
    obs_stats <- NULL
    if (model_ok) {
      if (is.null(obs_data_loaded)) {
        # Legacy behaviour: compute derived cal_metrics
        metrics <- tryCatch(
          cal_metrics(yaml_file,
                      model_filter   = model_filter,
                      wq_config_file = wq_config_file),
          error = function(e) {
            warning("[LHC] cal_metrics failed at iteration ", i, ": ",
                    conditionMessage(e))
            NULL
          }
        )
      } else {
        # New behaviour: compare with observed data and collect statistics
        obs_stats <- tryCatch(
          .cal_lhc_obs_stats(obs_data_loaded, dict_loaded, model_short, yaml_file,
                             wq_config_file = wq_config_file),
          error = function(e) {
            warning("[LHC] obs stats failed at iteration ", i, ": ",
                    conditionMessage(e))
            NULL
          }
        )
      }
    }

    results[[i]] <- list(
      params    = as.list(param_values),
      metrics   = metrics,
      obs_stats = obs_stats,
      model_ok  = model_ok
    )
  }

  # When obs_file is provided, flatten results into a long data.frame:
  # one row per (LHC iteration x variable), with stat columns NSE/RMSE/NRMSE/PBIAS/KGE
  if (!is.null(obs_data_loaded)) {
    rows <- lapply(results, function(r) {
      param_row <- as.data.frame(r$params, stringsAsFactors = FALSE)
      param_row$model_ok <- r$model_ok
      param_row$n_stats  <- if (is.null(r$obs_stats)) 0L else length(r$obs_stats)
      if (!is.null(r$obs_stats) && length(r$obs_stats) > 0) {
        var_rows <- lapply(names(r$obs_stats), function(key) {
          st <- r$obs_stats[[key]]
          out <- param_row
          out$variable <- key
          out$NSE      <- st$NSE
          out$RMSE     <- st$RMSE
          out$NRMSE    <- st$NRMSE
          out$PBIAS    <- st$PBIAS
          out$KGE      <- st$KGE
          out
        })
        do.call(rbind, var_rows)
      } else {
        param_row$variable <- NA_character_
        param_row$NSE      <- NA_real_
        param_row$RMSE     <- NA_real_
        param_row$NRMSE    <- NA_real_
        param_row$PBIAS    <- NA_real_
        param_row$KGE      <- NA_real_
        param_row
      }
    })
    results <- if (length(rows) > 0) dplyr::bind_rows(rows) else data.frame()
  }

  if (save_results) {
    out_path <- file.path(model_dir, output_file)
    saveRDS(results, out_path)
    if (verbose) {
      message("[LHC] Results saved to: ", out_path)
    }
  }

  return(results)
}
