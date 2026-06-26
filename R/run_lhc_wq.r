# ---------------------------------------------------------------------------
# Internal helper: compare simulated vs. observed and return statistics.
# Simulated output from get_output_wq() is already in global units
# (conversion_factor has been applied: model_units * CF = global_units).
# Observed data is also in global units, so the two series are directly
# comparable without any additional unit conversion.
# ---------------------------------------------------------------------------
.cal_lhc_obs_stats <- function(obs_data, dict, model_short, yaml_file,
                               wq_config_file = NULL,
                               verbose = FALSE,
                               obs_to_model_units = TRUE,
                               spin_up_days = NULL,
                               stats_by_depth = FALSE) {

                               

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  .parse_dt <- function(x) {
    parsed <- as.POSIXct(
      x,
      tz = "UTC",
      tryFormats = c(
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%dT%H:%M:%OS",
        "%Y-%m-%dT%H:%M:%SZ",
        "%Y-%m-%dT%H:%M:%OSZ"
      )
    )

    if (all(is.na(parsed)) && length(x) > 0) {
      parsed <- suppressWarnings(lubridate::parse_date_time(
        x,
        orders = c(
          "mdY HMS", "mdY HM", "mdY",
          "Ymd HMS", "Ymd HM", "Ymd",
          "YmdTHMS", "YmdTHMSz", "YmdTHM", "YmdTHMz"
        ),
        tz = "UTC"
      ))
      parsed <- as.POSIXct(parsed, tz = "UTC")
    }

    parsed
  }

  .to_num <- function(x) {
    x_chr <- trimws(as.character(x))
    x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
    suppressWarnings(as.numeric(x_chr))
  }

  # conversion_factor is defined as: model_units * conversion_factor =
  # harmonized_units. For obs values already in harmonized units, converting
  # to model units is: harmonized / conversion_factor.
  .harmonized_to_model_units <- function(x, conversion_factor) {
    cf <- suppressWarnings(as.numeric(as.character(conversion_factor)))
    if (is.na(cf) || cf == 0) {
      return(x)
    }
    x / cf
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
      return(list(data = matched[, c("depth", "value", "sim_value")], mode = "datetime_depth"))
    }

    obs_df$date <- as.Date(obs_df$datetime, tz = "UTC")
    sim_df$date <- as.Date(sim_df$datetime, tz = "UTC")
    obs_df <- obs_df[!is.na(obs_df$date), c("date", "depth", "value")]
    sim_df <- sim_df[!is.na(sim_df$date), c("date", "depth", "sim_value")]

    obs_daily <- stats::aggregate(value ~ date + depth, data = obs_df, FUN = mean)
    sim_daily <- stats::aggregate(sim_value ~ date + depth, data = sim_df, FUN = mean)
    matched_daily <- merge(obs_daily, sim_daily, by = c("date", "depth"))

    if (nrow(matched_daily) >= 2L) {
      return(list(data = matched_daily[, c("depth", "value", "sim_value")], mode = "date_depth"))
    }

    list(data = NULL, mode = "none")
  }

  .extract_depth_map <- function(df) {
    depth_cols <- grep("^Depth_", names(df), value = TRUE)
    if (length(depth_cols) == 0L) {
      return(data.frame(col = character(0), depth = numeric(0), stringsAsFactors = FALSE))
    }
    depth_vals <- suppressWarnings(as.numeric(sub("^Depth_", "", depth_cols)))
    data.frame(col = depth_cols, depth = depth_vals, stringsAsFactors = FALSE)
  }

  .resolve_nc_var_name <- function(var_name, available_vars) {
    if (is.null(available_vars) || length(available_vars) == 0L) {
      return(var_name)
    }
    if (var_name %in% available_vars) {
      return(var_name)
    }
    ci <- which(tolower(available_vars) == tolower(var_name))
    if (length(ci) >= 1L) {
      return(available_vars[ci[1]])
    }
    NA_character_
  }

  model_dict <- dict[toupper(dict$model) == toupper(model_short), , drop = FALSE]
  if (!is.null(wq_config_file) && nrow(model_dict) > 0) {
    model_dict <- expand_templates(model_dict, wq_config_file)
  }

  cfg <- load_config(yaml_file)

  model_dict$variable_global_name <- trimws(as.character(model_dict$variable_global_name))
  obs_data$variable_global_name <- trimws(as.character(obs_data$variable_global_name))

  # Preprocess observed fields once (instead of inside each variable loop).
  obs_data$depth <- .to_num(obs_data$depth)
  obs_data$value <- .to_num(obs_data$value)
  obs_data$datetime <- .parse_dt(obs_data$datetime)
  obs_data <- obs_data[is.finite(obs_data$value) & !is.na(obs_data$datetime), , drop = FALSE]

  if (!is.null(spin_up_days)) {
    spin_num <- suppressWarnings(as.numeric(spin_up_days[1]))
    if (is.na(spin_num) || spin_num < 0) {
      stop("'spin_up_days' must be NULL or a non-negative number.")
    }
    ler_cfg <- yaml::read_yaml(cfg$LER_config_file)
    sim_start <- as.POSIXct(ler_cfg$time$start, tz = "UTC")
    if (!is.na(sim_start) && spin_num > 0) {
      spin_cutoff <- sim_start + spin_num * 24 * 60 * 60
      obs_data <- obs_data[obs_data$datetime >= spin_cutoff, , drop = FALSE]
    }
  }

  if (nrow(model_dict) == 0) {
    return(list())
  }

  unresolved_template <- grepl("\\{group\\}|\\{idx:02d\\}",
                               model_dict$variable_model_name)
  model_dict <- model_dict[!unresolved_template, , drop = FALSE]

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
  obs_vars   <- obs_vars[!is.na(obs_vars) & nzchar(as.character(obs_vars))]
  stats_out  <- list()
  skip_counts <- list()
  .mark_skip <- function(reason) {
    current <- skip_counts[[reason]]
    if (is.null(current)) {
      current <- 0L
    }
    skip_counts[[reason]] <<- current + 1L
  }

  for (gvar in obs_vars) {
    dict_sub <- model_dict[model_dict$variable_global_name == gvar, , drop = FALSE]
    if (nrow(dict_sub) == 0) {
      dict_sub <- model_dict[tolower(model_dict$variable_global_name) == tolower(gvar), , drop = FALSE]
    }
    if (nrow(dict_sub) == 0 && "metric_name" %in% names(model_dict)) {
      dict_sub <- model_dict[trimws(as.character(model_dict$metric_name)) == gvar, , drop = FALSE]
    }
    if (nrow(dict_sub) == 0 && "metric_name" %in% names(model_dict)) {
      dict_sub <- model_dict[tolower(trimws(as.character(model_dict$metric_name))) == tolower(gvar), , drop = FALSE]
    }
    if (nrow(dict_sub) == 0) {
      .mark_skip("no_dict_match")
      next
    }

    # Prefer rows where metric_name matches the observed variable. This keeps
    # calibration against the source metric (e.g. Temp_degreeCelcius) and
    # avoids mixing in dictionary rows for other derived metrics that happen to
    # share the same variable_global_name.
    if ("metric_name" %in% names(dict_sub)) {
      metric_match_idx <- which(tolower(trimws(as.character(dict_sub$metric_name))) == tolower(gvar))
      if (length(metric_match_idx) > 0L) {
        dict_sub <- dict_sub[metric_match_idx, , drop = FALSE]
      }
    }

    obs_sub <- obs_data[obs_data$variable_global_name == gvar, ]
    if (nrow(obs_sub) == 0L) {
      .mark_skip("no_finite_obs_values")
      next
    }
    obs_depths <- sort(unique(obs_sub$depth[!is.na(obs_sub$depth)]))

    depth_01_raw <- dict_sub$depth_01[1]
    depth_01_flag <- suppressWarnings(as.integer(as.numeric(as.character(depth_01_raw))))
    if (is.na(depth_01_flag)) {
      depth_01_flag <- if (tolower(as.character(depth_01_raw)) %in% c("false", "f", "no", "n")) 0L else 1L
    }

    if (depth_01_flag == 1L) {
      dup_idx <- duplicated(obs_sub[, c("datetime", "depth")])
      if (any(dup_idx)) {
        if (isTRUE(verbose)) {
          message("[LHC][obs-stats] Non-unique observations for ", gvar,
                  " (datetime + depth). Averaging duplicates.")
        }
        obs_sub <- stats::aggregate(value ~ datetime + depth, data = obs_sub, FUN = mean)
      }
    } else {
      dup_idx <- duplicated(obs_sub[, c("datetime")])
      if (any(dup_idx)) {
        if (isTRUE(verbose)) {
          message("[LHC][obs-stats] Non-unique observations for ", gvar,
                  " (datetime). Averaging duplicates.")
        }
        obs_sub <- stats::aggregate(value ~ datetime, data = obs_sub, FUN = mean)
        obs_sub$depth <- NA_real_
      }
    }

    sim_pieces <- list()

    # Deduplicate by variable_model_name: the same model variable (e.g. "temp")
    # can appear in multiple dict rows because several derived metrics all share
    # one underlying variable. Extracting and summing duplicates would multiply
    # values by the count of duplicate rows (e.g. 5x for temperature), causing
    # completely wrong statistics. Only unique model variable names should be
    # extracted; summing is correct only when genuinely different model variables
    # (e.g. separate phytoplankton groups) contribute to one observed variable.
    dict_sub_extract <- dict_sub[!duplicated(dict_sub$variable_model_name), , drop = FALSE]

    cf_extract <- suppressWarnings(as.numeric(as.character(dict_sub_extract$conversion_factor)))
    cf_extract[is.na(cf_extract) | cf_extract == 0] <- 1
    if (isTRUE(obs_to_model_units)) {
      cf_unique <- unique(cf_extract)
      if (length(cf_unique) > 1L) {
        .mark_skip("mixed_conversion_factors_model_units")
        if (isTRUE(verbose)) {
          message("[LHC][obs-stats] Skipping ", gvar,
                  " in obs_to_model_units mode because multiple conversion factors are used: ",
                  paste(cf_unique, collapse = ", "))
        }
        next
      }
      obs_sub$value <- .harmonized_to_model_units(obs_sub$value, cf_unique[1])
    }

    for (row_idx in seq_len(nrow(dict_sub_extract))) {
      var_model_name <- dict_sub_extract$variable_model_name[row_idx]
      var_model_name <- .resolve_nc_var_name(var_model_name, available_nc_vars)
      if (is.na(var_model_name) || !nzchar(var_model_name)) {
        .mark_skip("var_missing_in_output_nc")
        if (isTRUE(verbose)) {
          message("[LHC][obs-stats] Variable not found in output.nc for ", gvar,
                  ": ", dict_sub_extract$variable_model_name[row_idx])
        }
        next
      }
      cf <- suppressWarnings(as.numeric(as.character(dict_sub_extract$conversion_factor[row_idx])))
      if (is.na(cf) || cf == 0) cf <- 1
      out_cf <- if (isTRUE(obs_to_model_units)) 1 else cf

      sim_data <- tryCatch(
        get_output_wq(
          config_file       = yaml_file,
          model             = model_short,
          vars              = var_model_name,
          obs_depths        = if (depth_01_flag == 1L) obs_depths else NULL,
          depth_01          = depth_01_flag,
          conversion_factor = out_cf
        ),
        error = function(e) NULL
      )

      if (is.null(sim_data) || length(sim_data) == 0) {
        .mark_skip("empty_sim_data")
        next
      }
      sim_piece <- sim_data[[1]]
      if (is.null(sim_piece) || nrow(sim_piece) == 0) {
        .mark_skip("empty_sim_piece")
        next
      }
      sim_pieces[[length(sim_pieces) + 1]] <- sim_piece
    }

    if (length(sim_pieces) == 0) {
      .mark_skip("no_sim_pieces")
      next
    }

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

    # Normalise the datetime column name in sim_df.  gotmtools::get_vari()
    # returns "date"; glmtools and Simstrat paths return "datetime".
    sim_dt_col <- names(sim_df)[tolower(names(sim_df)) %in% c("datetime", "date")][1]
    if (is.na(sim_dt_col)) {
      .mark_skip("sim_no_datetime_column")
      next
    }
    if (sim_dt_col != "datetime") {
      names(sim_df)[names(sim_df) == sim_dt_col] <- "datetime"
    }

    sim_df$datetime  <- .parse_dt(sim_df$datetime)
    sim_df <- sim_df[!is.na(sim_df$datetime), , drop = FALSE]

     
    if (nrow(sim_df) > 0) {
  message("[DEBUG] Sim time range: ",
          format(min(sim_df$datetime)), " -> ",
          format(max(sim_df$datetime)))
}
    if (nrow(sim_df) == 0L) {
      .mark_skip("sim_datetime_unparsed")
      next
    }

    if (depth_01_flag == 1L) {
      if (length(obs_depths) == 0L) {
        .mark_skip("no_obs_depths")
        next
      }

      depth_map <- .extract_depth_map(sim_df)
      if (nrow(depth_map) == 0L) {
        .mark_skip("no_depth_columns_in_sim")
        next
      }

      sim_long <- do.call(
        rbind,
        lapply(obs_depths, function(dep) {
          exact_idx <- which(!is.na(depth_map$depth) & abs(depth_map$depth - dep) <= 1e-6)
          if (length(exact_idx) == 0L) {
            candidate_idx <- which(!is.na(depth_map$depth))
            if (length(candidate_idx) == 0L) {
              return(NULL)
            }
            nearest <- candidate_idx[which.min(abs(depth_map$depth[candidate_idx] - dep))]
            dep_col <- depth_map$col[nearest]
          } else {
            dep_col <- depth_map$col[exact_idx[1]]
          }

          out <- sim_df[, c("datetime", dep_col), drop = FALSE]
          names(out)[2] <- "sim_value"
          out$depth <- round(dep, 4)
          out
        })
      )

      if (is.null(sim_long) || nrow(sim_long) == 0) {
        .mark_skip("empty_sim_long")
        next
      }

      obs_long <- obs_sub[, c("datetime", "depth", "value")]
      obs_long$depth <- round(obs_long$depth, 4)
      sim_long$depth <- round(sim_long$depth, 4)
      matched <- .match_obs_sim_depth(obs_long, sim_long)
      if (is.null(matched$data)) {
        .mark_skip("no_obs_sim_overlap_depth")
        next
      }

      if (isTRUE(stats_by_depth)) {
        depth_levels <- sort(unique(matched$data$depth))
        depth_levels <- depth_levels[!is.na(depth_levels)]
        for (dep in depth_levels) {
          dep_sub <- matched$data[matched$data$depth == dep, , drop = FALSE]
          if (nrow(dep_sub) < 2L) {
            next
          }
          st <- cal_stats(dep_sub$value, dep_sub$sim_value)
          dep_chr <- gsub("[^0-9A-Za-z_.-]", "_", as.character(signif(dep, 8)))
          key <- paste0(gvar, "__depth_", dep_chr)
          stats_out[[key]] <- list(
            variable = gvar,
            depth = as.numeric(dep),
            n_pairs = nrow(dep_sub),
            NSE   = st$NSE,
            RMSE  = st$RMSE,
            NRMSE = st$NRMSE,
            PBIAS = st$PBIAS,
            KGE   = st$KGE
          )
        }
      } else {
        st <- cal_stats(matched$data$value, matched$data$sim_value)
        stats_out[[gvar]] <- list(
          variable = gvar,
          depth = NA_real_,
          n_pairs = nrow(matched$data),
          NSE   = st$NSE,
          RMSE  = st$RMSE,
          NRMSE = st$NRMSE,
          PBIAS = st$PBIAS,
          KGE   = st$KGE
        )
      }
    } else {
      # No depth dimension – direct time-series comparison
      obs_ts           <- obs_sub[, c("datetime", "value")]

      # Identify the datetime column (case-insensitive) and the value column(s).
      dt_col <- names(sim_df)[tolower(names(sim_df)) == "datetime"][1]
      if (is.na(dt_col)) dt_col <- names(sim_df)[1]
      val_cols <- setdiff(names(sim_df), dt_col)
      if (length(val_cols) == 0L) {
        .mark_skip("sim_no_value_column")
        next
      }
      # When multiple value columns exist (e.g. depth layers returned for a
      # scalar variable), average across them.
      if (length(val_cols) == 1L) {
        sim_ts <- sim_df[, c(dt_col, val_cols[1]), drop = FALSE]
      } else {
        sim_ts <- sim_df[, dt_col, drop = FALSE]
        sim_ts$sim_value <- rowMeans(sim_df[, val_cols, drop = FALSE], na.rm = TRUE)
        val_cols <- "sim_value"
      }
      names(sim_ts) <- c("datetime", "sim_value")

      matched <- .match_obs_sim(obs_ts, sim_ts)
      if (is.null(matched$data)) {
        .mark_skip("no_obs_sim_overlap_time")
        next
      }

      st  <- cal_stats(matched$data$value, matched$data$sim_value)
      stats_out[[gvar]] <- list(
        variable = gvar,
        depth = NA_real_,
        n_pairs = nrow(matched$data),
        NSE   = st$NSE,
        RMSE  = st$RMSE,
        NRMSE = st$NRMSE,
        PBIAS = st$PBIAS,
        KGE   = st$KGE
      )
    }
  }

  vars_with_stats <- unique(vapply(stats_out, function(st) as.character(st$variable %||% ""), character(1)))
  vars_with_stats <- vars_with_stats[nzchar(vars_with_stats)]
  attr(stats_out, "n_obs_vars_total") <- length(obs_vars)
  attr(stats_out, "n_obs_vars_with_stats") <- length(vars_with_stats)

  if (length(stats_out) == 0L) {
    dict_gnames <- if ("variable_global_name" %in% names(model_dict))
      unique(model_dict$variable_global_name) else character(0)
    dict_mnames <- if ("metric_name" %in% names(model_dict))
      unique(model_dict$metric_name) else character(0)
    skip_msg <- if (length(skip_counts) > 0L)
      paste(names(skip_counts), unlist(skip_counts), sep = "=", collapse = ", ")
    else
      "(no skip counts — obs_vars may be empty)"
    message(
      "[LHC][obs-stats] No statistics produced.",
      "\n  obs variable_global_name values tried: ", paste(obs_vars, collapse = ", "),
      "\n  dict variable_global_name available : ", paste(head(dict_gnames, 20), collapse = ", "),
      "\n  dict metric_name available          : ", paste(head(dict_mnames, 20), collapse = ", "),
      "\n  skip summary: ", skip_msg
    )
  } else if (isTRUE(verbose) && length(skip_counts) > 0L) {
    skip_msg <- paste(names(skip_counts), unlist(skip_counts), sep = "=", collapse = ", ")
    message("[LHC][obs-stats] Some variables skipped. Skip summary: ", skip_msg)
  }

  stats_out
}

#' Run Latin Hypercube Calibration for Water Quality Models
#'
#' Performs Latin Hypercube Sampling (LHS) based parameter-space exploration
#' for coupled water-quality model setups and evaluates each sampled run using
#' either \code{cal_metrics()} outputs or observed-data statistics.
#'
#' @param model Character. One of \code{"GLM-AED2"}, \code{"GOTM-WET"},
#'   \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"}.
#' @param param_names Character vector. Parameter names to vary.
#' @param calib_setup Data frame with at least columns \code{pars}, \code{lb},
#'   \code{ub}, and \code{file}; optional \code{group_name} for group-specific
#'   CSV updates.
#' @param yaml_file Character. Path to output metrics YAML file.
#' @param model_dir Character. Path to model simulation directory.
#' @param n_samples Integer. Number of LHS samples (default = 50).
#' @param model_filter Character or \code{NULL}. Optional model key for
#'   \code{cal_metrics()}. If \code{NULL}, it is auto-derived from
#'   \code{model}.
#' @param wq_config_file Character or \code{NULL}. Path to WQ config file.
#' @param yaml_file_model Character. GOTM yaml filename (default
#'   \code{"gotm.yaml"}).
#' @param par_file Character. Simstrat par filename (default
#'   \code{"simstrat.par"}).
#' @param verbose Logical. Print progress messages.
#' @param save_results Logical. If \code{TRUE}, save results to
#'   \code{output_file} in \code{model_dir}.
#' @param output_file Character. RDS output filename when
#'   \code{save_results = TRUE}.
#' @param obs_file Character or \code{NULL}. Optional observed-data CSV for
#'   per-run statistics.
#' @param obs_to_model_units Logical. When \code{TRUE} and \code{obs_file} is
#'   provided, observed values (assumed harmonized/global units) are converted
#'   back to model-specific units using dictionary \code{conversion_factor}
#'   before computing statistics. Default is \code{TRUE} (model outputs are
#'   kept in model-specific units for comparison).
#' @param spin_up_days Numeric or \code{NULL}. Optional number of days after
#'   simulation start to exclude from observed-data comparison in
#'   \code{obs_file} mode. This is useful to ignore model spin-up transients.
#'   Default is \code{NULL} (no spin-up exclusion).
#' @param stats_by_depth Logical. Only used when \code{obs_file} is provided.
#'   If \code{TRUE}, depth-resolved variables return one set of statistics per
#'   depth instead of one aggregated set per variable. Default is \code{FALSE}.
#' @param return_best Logical. Only used when \code{obs_file} is provided.
#'   If \code{TRUE}, identify the best parameter set across iterations using
#'   \code{best_metric}. Default is \code{TRUE}.
#' @param best_metric Character. Objective metric used to rank parameter sets
#'   when \code{return_best = TRUE}. One of \code{"KGE"}, \code{"NSE"},
#'   \code{"RMSE"}, \code{"NRMSE"}, or \code{"PBIAS"}. Default is
#'   \code{"KGE"}.
#' @param parallel Logical. If \code{TRUE}, run in parallel by delegating to
#'   \code{run_lhc_wq_parallel()}. Default is \code{FALSE}.
#' @param n_workers Integer or \code{NULL}. Number of workers used when
#'   \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()}.
#' @param parallel_dir Character. Parent directory for worker copies when
#'   \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()}.
#' @param keep_worker_dirs Logical. Keep worker directories after completion
#'   when \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()}.
#' @param use_de Logical. If \code{TRUE}, run differential evolution after LHC
#'   initialization using LHC results as generation 1. Default is \code{FALSE}
#'   (LHC-only mode maintains backward compatibility).
#' @param de_iterations Integer. Number of DE generations to run (default = 50).
#'   Ignored when \code{use_de = FALSE}.
#' @param de_popsize Integer or \code{NULL}. DE population size. If \code{NULL}
#'   (default), uses \code{n_samples}. Must be >= 4 for DEoptim. Ignored when
#'   \code{use_de = FALSE}.
#' @param de_f Numeric. Mutation scaling factor in range [0.2, 2.0]. Higher
#'   values increase mutation magnitude. Default is 0.8. Ignored when
#'   \code{use_de = FALSE}.
#' @param de_cr Numeric. Crossover probability in range [0, 1]. Higher values
#'   increase parameter exchange probability. Default is 0.9. Ignored when
#'   \code{use_de = FALSE}.
#' @param de_seed_from_lhc Logical. Initialize DE population from best LHC
#'   results (default = \code{TRUE}). If \code{FALSE}, DE starts from random
#'   population. Ignored when \code{use_de = FALSE}.
#'
#' @return If \code{obs_file = NULL}, a list of length \code{n_samples} with
#'   sampled parameters and metrics per run. When \code{use_de = TRUE}, includes
#'   additional \code{de_phase} element with DEoptim results. If \code{obs_file}
#'   is supplied, returns a flattened data frame with sampled parameters and
#'   summary statistics. In \code{obs_file} mode and when \code{return_best = TRUE},
#'   the returned data.frame includes column \code{is_best}, and attributes
#'   \code{best_parameter_set} and \code{best_metric}.
#' @export

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
                       obs_file        = NULL,
                       obs_to_model_units = TRUE,
                       spin_up_days    = NULL,
                       stats_by_depth  = FALSE,
                       return_best     = TRUE,
                       best_metric     = "KGE",
                       parallel        = FALSE,
                       n_workers       = NULL,
                       parallel_dir    = tempdir(),
                       keep_worker_dirs = FALSE,
                       lhs_matrix      = NULL,
                       sample_indices  = NULL,
                       use_de          = FALSE,
                       de_iterations   = 50,
                       de_popsize      = NULL,
                       de_f            = 0.8,
                       de_cr           = 0.9,
                       de_seed_from_lhc = TRUE) {

  # Helper: Initialize DE population from best LHC results
  .init_de_population_from_lhc <- function(lhc_results, lhs_matrix_ref,
                                         param_names_ref, bounds_ref,
                                         de_popsize_val) {

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  # ------------------------------------------------------------
  # ✅ CASE 1: obs_file mode → data.frame
  # ------------------------------------------------------------
  if (is.data.frame(lhc_results)) {

    metric_col <- best_metric   #
    df <- lhc_results

    df <- df[df$model_ok %in% TRUE, ]
    df <- df[!is.na(df[[metric_col]]), ]

    if (nrow(df) == 0) {
      stop("[DE] No valid LHC results to seed DE")
    }

    # Aggregate per sample_index
    agg <- aggregate(df[[metric_col]],
                     by = list(sample_index = df$sample_index),
                     FUN = mean, na.rm = TRUE)

    names(agg)[2] <- "fitness"

    # sort best first
    agg <- agg[order(agg$fitness, decreasing = TRUE), ]

    top_indices <- agg$sample_index[seq_len(min(de_popsize_val, nrow(agg)))]

    # convert to row indices
    lhs_indices <- match(top_indices, seq_len(nrow(lhs_matrix_ref)))

    init_pop <- lhs_matrix_ref[lhs_indices, , drop = FALSE]

    return(init_pop)
  }

  # ------------------------------------------------------------
  # ✅ CASE 2: list mode (original behavior)
  # ------------------------------------------------------------
  lhc_fitness <- lapply(lhc_results, function(r) {
    if (!is.null(r$obs_stats) && length(r$obs_stats) > 0) {
      vals <- vapply(r$obs_stats, function(st) st$KGE %||% NA_real_, numeric(1))
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) return(mean(vals))
    }
    return(NA_real_)
  })

  lhc_fitness <- unlist(lhc_fitness)
  lhc_fitness[is.na(lhc_fitness)] <- -Inf

  top_indices <- order(lhc_fitness, decreasing = TRUE)[
    seq_len(min(de_popsize_val, length(lhc_fitness)))
  ]

  if (length(top_indices) < de_popsize_val) {
    top_indices <- c(top_indices,
                     rep(top_indices,
                         length.out = de_popsize_val - length(top_indices)))
  }

  lhs_matrix_ref[top_indices, , drop = FALSE]
}

  # Helper: Create objective function for DE optimization
  .make_de_objective <- function() {

    iter_counter <- 0 
    # ------------------------------------------------------------------
# Reset model files to clean baseline before applying parameters
# ------------------------------------------------------------------

    # Closure over all calibration context
    function(x_scaled) {
      
iter_counter <<- iter_counter + 1   # ✅ ADD
  if (isTRUE(verbose)) {
    message("[DE] Eval ", iter_counter)
  }

      file.copy(
  from = baseline_dir, 
  to   = model_dir,
  recursive = TRUE,
  overwrite = TRUE
)
      # x_scaled is in [0, 1] from DEoptim; scale to actual bounds
param_values <- setNames(
  x_scaled,
  param_names
)

      
      # Update parameters in model config files
      for (p in param_names) {
        tryCatch(
          .update_param(p, param_values[p]),
          error = function(e) {
            if (isTRUE(verbose)) {
              message("[DE-obj] Warning updating param ", p, ": ", conditionMessage(e))
            }
          }
        )
      }
      
      # Run model
      model_ok <- tryCatch({
        .run_model()
        TRUE
      }, error = function(e) {
        if (isTRUE(verbose)) {
          message("[DE-obj] Model run failed: ", conditionMessage(e))
        }
        FALSE
      })
      
      if (!model_ok) {
        return(1e6)  # Large penalty for failed runs
      }
      
      # Compute objective from obs_file stats or metrics
      if (!is.null(obs_data_loaded)) {
        obs_stats <- tryCatch(
          .cal_lhc_obs_stats(obs_data_loaded, dict_loaded, model_short, yaml_file,
                             wq_config_file = wq_config_file,
                             verbose = FALSE,
                             obs_to_model_units = obs_to_model_units,
                             spin_up_days = spin_up_days,
                             stats_by_depth = stats_by_depth),
          error = function(e) NULL
        )
        
        if (is.null(obs_stats) || length(obs_stats) == 0) {
          return(1e6)
        }
        
        # Extract fitness values (KGE or other metric)
        metric_col <- best_metric
        vals <- vapply(obs_stats, function(st) st[[metric_col]] %||% NA_real_, numeric(1))
        vals <- vals[is.finite(vals)]
        
        if (length(vals) == 0) {
          return(1e6)
        }
        
        # For KGE/NSE, higher is better; for RMSE/NRMSE/PBIAS, lower is better
        # DEoptim minimizes, so we negate KGE/NSE and keep RMSE/NRMSE/PBIAS
        mean_val <- mean(vals, na.rm = TRUE)
        if (metric_col %in% c("KGE", "NSE")) {
          return(-mean_val)  # Negate for minimization
        } else if (metric_col == "PBIAS") {
          return(abs(mean_val))  # Minimize absolute bias
        } else {
          return(mean_val)  # RMSE, NRMSE already minimized
        }
      } else {
        # Metrics-based mode (legacy)
        metrics <- tryCatch(
          cal_metrics(yaml_file,
                      model_filter   = model_filter,
                      wq_config_file = wq_config_file),
          error = function(e) NULL
        )
        
        if (is.null(metrics)) {
          return(1e6)
        }
        
        # Simple aggregation: count of non-null metrics (higher is better)
        # This is a placeholder; users may want to customize this
        n_metrics <- length(metrics)
        return(-n_metrics)  # Negate to maximize metric count
      }
    }
  }

  if (isTRUE(parallel)) {
    return(run_lhc_wq_parallel(
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
      verbose = verbose,
      save_results = save_results,
      output_file = output_file,
      obs_file = obs_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days = spin_up_days,
      stats_by_depth = stats_by_depth,
      return_best = return_best,
      best_metric = best_metric,
      n_workers = n_workers,
      parallel_dir = parallel_dir,
      keep_worker_dirs = keep_worker_dirs,
      use_de = use_de
    ))
  }

  original_tz <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = original_tz), add = TRUE)

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
        model_filter <- "WET"
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
    obs_dates <- as.POSIXct(obs_data_loaded$datetime,
                            tz = "UTC",
                            tryFormats = c(
                              "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
                              "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
                              "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"
                            ))
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

  n_params <- length(param_names)
  if (is.null(lhs_matrix)) {
    lhs_matrix <- lhs::randomLHS(n_samples, n_params)
  } else {
    if (!is.matrix(lhs_matrix)) {
      stop("'lhs_matrix' must be a matrix when provided.")
    }
    if (ncol(lhs_matrix) != n_params) {
      stop("'lhs_matrix' must have one column per parameter in 'param_names'.")
    }
  }

  total_samples <- nrow(lhs_matrix)
  if (is.null(sample_indices)) {
    sample_indices <- seq_len(total_samples)
  }

  if (length(sample_indices) == 0L) {
    return(if (is.null(obs_data_loaded)) list() else data.frame())
  }

  if (any(sample_indices < 1L | sample_indices > total_samples)) {
    stop("'sample_indices' must fall within the rows of 'lhs_matrix'.")
  }

  best_metric <- toupper(as.character(best_metric[1]))
  valid_best_metrics <- c("KGE", "NSE", "RMSE", "NRMSE", "PBIAS")
  if (!best_metric %in% valid_best_metrics) {
    stop("'best_metric' must be one of: ", paste(valid_best_metrics, collapse = ", "),
         "\nProvided: ", best_metric)
  }


    # ------------------------------------------------------------------
# Create baseline copy of the model directory (for DE stability)
# ------------------------------------------------------------------


baseline_dir <- file.path(tempdir(), "model_baseline_copy")

if (!dir.exists(baseline_dir)) {
  dir.create(baseline_dir, recursive = TRUE)
  file.copy(model_dir, baseline_dir,
            recursive = TRUE,
            overwrite = TRUE)
}
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
    for (k in seq_len(nrow(rows))) {
      file_or_path <- as.character(rows$file[k])
      param_path <- file.path(model_dir, file_or_path)

      if (grepl("\\.nml$", file_or_path, ignore.case = TRUE)) {
        nml <- glmtools::read_nml(param_path)
        nml <- glmtools::set_nml(nml, p, value)
        glmtools::write_nml(nml, param_path)

      } else if (grepl("\\.csv$", file_or_path, ignore.case = TRUE)) {
        df <- readr::read_csv(param_path, show_col_types = FALSE)
        names(df) <- gsub("^['\"]|['\"]$", "", names(df))
        pname_col <- intersect(c("p_name", "pname"), names(df))
        if (length(pname_col) == 0) {
          stop("Could not identify parameter name column ('p_name' or 'pname') in: ",
               param_path)
        }
        df[[pname_col]] <- gsub("^['\"]|['\"]$", "", df[[pname_col]])

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
        readr::write_csv(df, param_path)

      } else if (grepl("\\.yaml$|\\.yml$", file_or_path, ignore.case = TRUE)) {
        gotmtools::set_yaml_value(param_path, label = p, value = value)

      } else if (model_upper %in% c("GLM-AED2", "SIMSTRAT-AED2")) {
        # Dictionary-style AED2 key path (e.g. aed2_oxygen/theta_sed_oxy)
        if (!requireNamespace("configr", quietly = TRUE)) {
          stop("Package 'configr' is required to resolve model config files.")
        }

        if (is.null(wq_config_file) || !nzchar(wq_config_file)) {
          stop("'wq_config_file' must be provided when calib_setup$file contains ",
               "dictionary-style AED2 paths (e.g. section/parameter).")
        }

        lst_cfg <- configr::read.config(wq_config_file)
        cfg_files <- lst_cfg[["config_files"]]
        model_cfg <- cfg_files[[model]]
        if (is.null(model_cfg) || !nzchar(model_cfg)) {
          cfg_names <- names(cfg_files)
          cfg_idx <- which(toupper(cfg_names) == toupper(model))[1]
          if (!is.na(cfg_idx)) {
            model_cfg <- cfg_files[[cfg_idx]]
          }
        }
        if (is.null(model_cfg) || !nzchar(model_cfg)) {
          stop("Could not resolve config file for model '", model,
               "' from wq_config_file: ", wq_config_file,
               ". Available keys: ", paste(names(cfg_files), collapse = ", "))
        }

        # For Simstrat, config_files often points to simstrat.par (physics),
        # while WQ parameters from AED2 dictionary paths belong in AED2 nml files.
        if (model_upper == "SIMSTRAT-AED2" &&
            grepl("\\.par$", model_cfg, ignore.case = TRUE)) {
          model_cfg <- file.path(dirname(model_cfg), "aed2.nml")
        }

        module_k <- if ("module" %in% names(rows)) as.character(rows$module[k]) else NA_character_
        if (module_k %in% c("phytoplankton", "zooplankton")) {
          base_dir <- dirname(model_cfg)
          model_cfg <- if (module_k == "phytoplankton") {
            file.path(base_dir, "aed2_phyto_pars.nml")
          } else {
            file.path(base_dir, "aed2_zoop_pars.nml")
          }
        }

        nml_candidates <- c(
          file.path(model_dir, model_cfg),
          file.path(dirname(model_dir), model_cfg),
          model_cfg
        )
        nml_candidates <- unique(normalizePath(nml_candidates,
                                               winslash = "/",
                                               mustWork = FALSE))
        nml_path <- nml_candidates[file.exists(nml_candidates)][1]
        if (is.na(nml_path) || !nzchar(nml_path)) {
          stop("Could not find resolved nml file for parameter '", p,
               "'. Tried: ", paste(nml_candidates, collapse = ", "))
        }

        path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
        if (length(path_parts) != 2L) {
          stop("Unsupported AED2 dictionary path for parameter '", p,
               "': ", file_or_path, " (expected 'section/parameter').")
        }

        nml <- glmtools::read_nml(nml_path)
        section <- path_parts[1]
        par_name <- path_parts[2]

        if (is.null(nml[[section]]) || is.null(nml[[section]][[par_name]])) {
          nml <- glmtools::set_nml(nml, par_name, value)
        } else {
          nml[[section]][[par_name]][1] <- value
        }
        glmtools::write_nml(nml, nml_path)

      } else if (model_upper %in% c("GOTM-WET", "GOTM-SELMAPROTBAS")) {
        # Dictionary-style YAML key path (e.g. selmaprotbas/initialization/o2)
        if (!requireNamespace("LakeEnsemblR", quietly = TRUE)) {
          stop("Package 'LakeEnsemblR' is required to update GOTM YAML key paths.")
        }

        # Prefer fabm.yaml for biogeochemistry parameters; fallback to yaml_file_model.
        yaml_target <- file.path(model_dir, "fabm.yaml")
        if (!file.exists(yaml_target)) {
          yaml_target <- file.path(model_dir, yaml_file_model)
        }
        if (!file.exists(yaml_target)) {
          stop("Could not find a YAML file to update for parameter '", p,
               "'. Tried: ", file.path(model_dir, "fabm.yaml"), " and ",
               file.path(model_dir, yaml_file_model))
        }

        path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
        group_col <- rows$group_name[k]
        if (!is.na(group_col)) {
          path_parts[path_parts == "{group_name}"] <- group_col
        }

        arglist <- as.list(path_parts)
        names(arglist) <- paste0("key", seq_along(path_parts))
        arglist$value <- value
        arglist$file <- yaml_target
        arglist$verbose <- FALSE
        do.call(LakeEnsemblR::input_yaml_multiple, args = arglist)

      } else {
        stop("Unsupported file type for parameter '", p, "': ", file_or_path)
      }
    }
  }

  # Main LHC loop. Iterations update files in-place under model_dir, so this
  # remains sequential unless each run is executed in an isolated copy.
  results <- vector("list", length(sample_indices))

  for (result_idx in seq_along(sample_indices)) {
    i <- sample_indices[result_idx]
    if (verbose) {
      cat("\n[LHC] Iteration", i, "/", total_samples, "\n")
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
                             wq_config_file = wq_config_file,
                             verbose = verbose,
                             obs_to_model_units = obs_to_model_units,
                             spin_up_days = spin_up_days,
                             stats_by_depth = stats_by_depth),
          error = function(e) {
            warning("[LHC] obs stats failed at iteration ", i, ": ",
                    conditionMessage(e))
            NULL
          }
        )
      }
    }

    results[[result_idx]] <- list(
      params    = as.list(param_values),
      metrics   = metrics,
      obs_stats = obs_stats,
      model_ok  = model_ok,
      sample_index = i
    )
  }

  # When obs_file is provided, flatten results into a long data.frame:
  # one row per (LHC iteration x variable), with stat columns NSE/RMSE/NRMSE/PBIAS/KGE
  if (!is.null(obs_data_loaded)) {
    `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

    rows <- lapply(results, function(r) {
      param_row <- as.data.frame(r$params, stringsAsFactors = FALSE)
      param_row$sample_index <- as.integer(r$sample_index %||% NA_integer_)
      param_row$model_ok <- r$model_ok
      param_row$n_stats  <- if (is.null(r$obs_stats)) 0L else length(r$obs_stats)
      param_row$n_obs_vars <- if (is.null(r$obs_stats)) NA_integer_ else as.integer(attr(r$obs_stats, "n_obs_vars_total") %||% NA_integer_)
      param_row$n_obs_vars_with_stats <- if (is.null(r$obs_stats)) NA_integer_ else as.integer(attr(r$obs_stats, "n_obs_vars_with_stats") %||% NA_integer_)

      if (!is.null(r$obs_stats) && length(r$obs_stats) > 0) {
        var_rows <- lapply(names(r$obs_stats), function(key) {
          st <- r$obs_stats[[key]]
          out <- param_row
          out$variable <- as.character(st$variable %||% key)
          out$depth    <- suppressWarnings(as.numeric(st$depth %||% NA_real_))
          out$n_pairs  <- as.integer(st$n_pairs %||% NA_integer_)
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
        param_row$depth    <- NA_real_
        param_row$n_pairs  <- NA_integer_
        param_row$NSE      <- NA_real_
        param_row$RMSE     <- NA_real_
        param_row$NRMSE    <- NA_real_
        param_row$PBIAS    <- NA_real_
        param_row$KGE      <- NA_real_
        param_row
      }
    })
    results <- if (length(rows) > 0) dplyr::bind_rows(rows) else data.frame()

    # Identify best parameter set across LHC iterations from observed-data stats.
    if (isTRUE(return_best) && nrow(results) > 0) {
      metric_for_scoring <- best_metric
      score_sign <- if (metric_for_scoring %in% c("RMSE", "NRMSE")) -1 else 1
      score_transform <- function(x) {
        if (metric_for_scoring == "PBIAS") {
          return(-abs(x))
        }
        score_sign * x
      }

      score_rows <- results[results$model_ok %in% TRUE, , drop = FALSE]
      score_rows <- score_rows[!is.na(score_rows[[metric_for_scoring]]), , drop = FALSE]
      if (nrow(score_rows) == 0 && best_metric %in% c("KGE", "NSE")) {
        fallback_metric <- "RMSE"
        fallback_rows <- results[results$model_ok %in% TRUE, , drop = FALSE]
        fallback_rows <- fallback_rows[!is.na(fallback_rows[[fallback_metric]]), , drop = FALSE]
        if (nrow(fallback_rows) > 0) {
          metric_for_scoring <- fallback_metric
          score_sign <- -1
          score_transform <- function(x) -x
          score_rows <- fallback_rows
          if (isTRUE(verbose)) {
            message("[LHC] No finite ", best_metric,
                    " values were available; using RMSE to rank parameter sets.")
          }
        }
      }

      best_summary <- NULL
      if (nrow(score_rows) > 0) {
        iter_ids <- sort(unique(score_rows$sample_index))
        iter_scores <- lapply(iter_ids, function(iter_id) {
          sub <- score_rows[score_rows$sample_index == iter_id, , drop = FALSE]
          if (nrow(sub) == 0) {
            return(NULL)
          }

          vals <- score_transform(sub[[metric_for_scoring]])
          w <- suppressWarnings(as.numeric(sub$n_pairs))
          w[is.na(w) | w <= 0] <- 1

          ok <- is.finite(vals) & is.finite(w)
          if (!any(ok)) {
            return(NULL)
          }

          objective <- stats::weighted.mean(vals[ok], w = w[ok], na.rm = TRUE)
          first_row <- sub[1, , drop = FALSE]

          out <- first_row[, c("sample_index", "n_stats", "n_obs_vars", "n_obs_vars_with_stats"), drop = FALSE]
          out$best_metric <- metric_for_scoring
          out$objective_score <- objective
          out$objective_value <- if (metric_for_scoring == "PBIAS") -objective else objective * score_sign
          for (p in param_names) {
            out[[p]] <- first_row[[p]]
          }
          out
        })

        iter_scores <- iter_scores[!vapply(iter_scores, is.null, logical(1))]
        if (length(iter_scores) > 0) {
          score_table <- do.call(rbind, iter_scores)
          best_idx <- which.max(score_table$objective_score)[1]
          best_summary <- score_table[best_idx, , drop = FALSE]

          best_iter <- as.integer(best_summary$sample_index[1])
          results$is_best <- results$sample_index == best_iter
          attr(results, "best_parameter_set") <- best_summary
          attr(results, "best_metric") <- metric_for_scoring

          if (isTRUE(verbose)) {
            message("[LHC] Best parameter set identified at sample_index=", best_iter,
                    " using ", best_metric, ".")
          }
        }
      }

      if (is.null(best_summary)) {
        results$is_best <- FALSE
        attr(results, "best_parameter_set") <- NULL
        attr(results, "best_metric") <- metric_for_scoring
        if (isTRUE(verbose)) {
          message("[LHC] Could not identify a best parameter set: no valid observed-data statistics were available.")
        }
      }
    }
  }

  # =========================================================================
  # DIFFERENTIAL EVOLUTION PHASE
  # =========================================================================
  # Use LHC results as generation 1 and apply DE refinement if requested
  if (isTRUE(use_de)) {
    if (isTRUE(verbose)) {
      message("\n[DE] Starting differential evolution phase after LHC...")
    }
    
    # Validate DE parameters
    de_popsize <- de_popsize %||% n_samples
    if (de_popsize < 4) {
      stop("de_popsize must be >= 4 for DEoptim; provided: ", de_popsize)
    }
    
    if (de_f < 0.2 || de_f > 2.0) {
      warning("de_f should be in [0.2, 2.0]; provided: ", de_f, ". Proceeding anyway.")
    }
    
    if (de_cr < 0 || de_cr > 1) {
      stop("de_cr must be in [0, 1]; provided: ", de_cr)
    }
    
    # Initialize DE population from LHC results
    initial_pop <- NULL
    if (isTRUE(de_seed_from_lhc)) {
      initial_pop <- .init_de_population_from_lhc(
        results, lhs_matrix, param_names, bounds, de_popsize
      )
    }
    
    # Create objective function for DE
    obj_fn <- .make_de_objective()
    
    # Build lower and upper bounds vectors for DEoptim
    bounds_lower <- vapply(param_names, function(p) bounds[[p]]["lb"], numeric(1))
    bounds_upper <- vapply(param_names, function(p) bounds[[p]]["ub"], numeric(1))
    
    # Run DE
    if (isTRUE(verbose)) {
      message("[DE] Population size: ", de_popsize, 
              ", Generations: ", de_iterations,
              ", F: ", de_f, ", CR: ", de_cr)
    }
    

    de_result <- DEoptim::DEoptim(
  fn = obj_fn,
  lower = bounds_lower,
  upper = bounds_upper,
  control = DEoptim::DEoptim.control(
    itermax = de_iterations,
    NP = de_popsize,
    F = de_f,
    CR = de_cr,
   # initial.pop = initial_pop,
    trace = TRUE,
    parallelType = 0
  )
)
    # de_result <- tryCatch(
    #   DEoptim::DEoptim(
    #     fn = obj_fn,
    #     lower = bounds_lower,
    #     upper = bounds_upper,
    #     control = DEoptim::DEoptim.control(
    #       itermax = de_iterations,
    #       NP = de_popsize,
    #       F = de_f,
    #       CR = de_cr,
    #       initial.pop = initial_pop,
    #       trace = if (isTRUE(verbose)) 10 else FALSE,
    #       parallelType = 0  # Sequential (parallel would need different setup)
    #     )
    #   ),
    #   error = function(e) {
    #     warning("[DE] DEoptim failed: ", conditionMessage(e))
    #     NULL
    #   }
    # )
    
    if (!is.null(de_result)) {
      if (isTRUE(verbose)) {
        message("[DE] Optimization complete. Best fitness: ", 
                round(de_result$optim$bestval, 6))
      }
      
      # Store DE phase results
      if (is.null(obs_data_loaded)) {
        # For list-based LHC results, append de_phase
        results$de_phase <- de_result
        results$de_best_params <- setNames(
          as.list(de_result$optim$bestmem),
          param_names
        )
      } else {
        # For data.frame results from obs_file, store as attributes
        attr(results, "de_phase") <- de_result
        attr(results, "de_best_params") <- setNames(
          as.list(de_result$optim$bestmem),
          param_names
        )
        
        if (isTRUE(verbose)) {
          message("[DE] Best parameters from DE:")
          for (i in seq_along(param_names)) {
            message("  ", param_names[i], " = ", 
                    round(de_result$optim$bestmem[i], 6))
          }
        }
      }
    } else {
      if (isTRUE(verbose)) {
        message("[DE] DE phase skipped due to error.")
      }
    }
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
