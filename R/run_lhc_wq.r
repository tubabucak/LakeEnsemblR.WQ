# ---------------------------------------------------------------------------
# Internal helper: turn a completed DEoptim result into a one-row
# best_parameter_set data.frame in the same shape run_lhc_wq() produces for
# the LHC phase (sample_index, n_stats, n_obs_vars, n_obs_vars_with_stats,
# best_metric, objective_score, objective_value, <one column per param>).
#
# Pulled out of run_lhc_wq()'s DE block as a pure function -- both so it's
# unit-testable without running an actual model, and because getting the
# metric sign convention right here matters: DEoptim always minimizes, so
# .make_de_objective() flips the sign for KGE/NSE (maximize) vs RMSE/NRMSE
# (already minimize) vs PBIAS (minimizes mean(abs(.)), no flip). This
# un-flips that back to the metric's natural value for reporting, matching
# the LHC phase's objective_value convention.
# ---------------------------------------------------------------------------
.de_best_parameter_set <- function(de_result, param_key, best_metric) {
  best_metric_upper <- toupper(as.character(best_metric[1]))

  if (best_metric_upper == "PBIAS") {
    objective_value <- de_result$optim$bestval
  } else {
    score_sign <- if (best_metric_upper %in% c("RMSE", "NRMSE")) 1 else -1
    objective_value <- de_result$optim$bestval * score_sign
  }

  # Named with param_key (unique per position, matching the LHC phase's
  # result columns) rather than raw param_names -- duplicate `pars` names
  # across groups would otherwise collide here too.
  best_params_df <- as.data.frame(
    as.list(stats::setNames(de_result$optim$bestmem, param_key)),
    stringsAsFactors = FALSE
  )

  cbind(
    data.frame(
      sample_index = NA_integer_,
      n_stats = NA_integer_,
      n_obs_vars = NA_integer_,
      n_obs_vars_with_stats = NA_integer_,
      best_metric = best_metric_upper,
      objective_score = de_result$optim$bestval,
      objective_value = objective_value,
      stringsAsFactors = FALSE
    ),
    best_params_df
  )
}

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
                               stats_by_depth = FALSE,
                               target_variables = NULL) {

  # Unconditional (not gated behind verbose) entry-point diagnostic -- writes
  # to its own per-process file rather than message()/the shared cluster
  # outfile, since concurrent workers writing to one shared log file can
  # corrupt/interleave each other's lines (confirmed: garbled warnings like
  # "4: 5: In system2(...)...In system2(...):" in earlier logs are exactly
  # this happening), which could make this line silently vanish without
  # meaning the call itself didn't happen.
  try({
    diag_file <- file.path(tempdir(), paste0("cal_lhc_obs_stats_debug_", Sys.getpid(), ".txt"))
    cat(format(Sys.time(), "%H:%M:%OS3"), " yaml_file=", yaml_file,
        " model_short=", model_short, "\n", sep = "", file = diag_file, append = TRUE)
  }, silent = TRUE)

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

  # Only require this run's model to have an existing output folder -- LHC
  # calibration always targets one model at a time, so an unrelated model
  # listed in the same shared Output.yaml but never actually run here
  # shouldn't fail this call.
  cfg <- load_config(yaml_file, required_models = model_short)

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
    # Force it to check relative to where the current configuration file resides
    nc_file <- file.path(dirname(yaml_file), "output.nc")
    if (!file.exists(nc_file)) {
      nc_file <- file.path(cfg$model_folders[[model_key]], "output.nc") # fallback
    }
    if (file.exists(nc_file)) {
      # Open only long enough to read the variable names, then close
      # immediately -- get_output_wq() below opens its own handle on this
      # same file per variable via glmtools::get_var(), and holding this one
      # open concurrently (previously via on.exit for the whole function)
      # risks corrupted/garbage NetCDF reads, especially on Windows.
      nc <- ncdf4::nc_open(nc_file)
      available_nc_vars <- names(nc$var)
      ncdf4::nc_close(nc)
    }
  }

  obs_vars   <- unique(obs_data$variable_global_name)
  obs_vars   <- obs_vars[!is.na(obs_vars) & nzchar(as.character(obs_vars))]
  
  # Filter to target variables

  if (!is.null(target_variables)) {
    obs_vars <- obs_vars[obs_vars %in% target_variables]
  }
  

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
      if (isTRUE(verbose)) {
        num_cols <- vapply(sim_piece, is.numeric, logical(1))
        sim_means <- if (any(num_cols)) {
          round(colMeans(sim_piece[, num_cols, drop = FALSE], na.rm = TRUE), 4)
        } else {
          NA
        }
        message("[LHC][diag] yaml_file=", yaml_file, " var=", var_model_name,
                " sim_piece dims=", nrow(sim_piece), "x", ncol(sim_piece),
                " colmeans=", paste(names(sim_means), sim_means, sep = "=", collapse = ", "))
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

     
 #   if (nrow(sim_df) > 0) {
 # message("[DEBUG] Sim time range: ",
 #         format(min(sim_df$datetime)), " -> ",
 #         format(max(sim_df$datetime)))
#}
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
      # No depth dimension -- direct time-series comparison
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
      "(no skip counts -- obs_vars may be empty)"
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
#' @param yaml_file_model Character or \code{NULL}. GOTM yaml filename. If
#'   \code{NULL} (default), derived from \code{ler_config_file}'s
#'   \code{config_files$GOTM} entry when available, else falls back to
#'   \code{"gotm.yaml"}.
#' @param par_file Character or \code{NULL}. Simstrat par filename. If
#'   \code{NULL} (default), derived from \code{ler_config_file}'s
#'   \code{config_files$Simstrat} entry when available, else falls back to
#'   \code{"simstrat.par"}.
#' @param ler_config_file Character or \code{NULL}. Path to the LakeEnsemblR
#'   (physical-model) config file, e.g. \code{"LakeEnsemblR.yaml"}. When
#'   supplied and \code{yaml_file_model}/\code{par_file} are not explicitly
#'   set, they are auto-derived from its \code{config_files} section instead
#'   of requiring the user to duplicate those filenames here. If relative,
#'   resolved against \code{dirname(model_dir)}.
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
#' @param target_variables Character. Vector of variables to be included in the objective function
#' @param parallel Logical. If \code{TRUE}, run in parallel by delegating to
#'   \code{run_lhc_wq_parallel()}. Default is \code{FALSE}.
#' @param n_workers Integer or \code{NULL}. Number of workers used when
#'   \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()}.
#' @param parallel_dir Character or \code{NULL}. Parent directory for worker
#'   copies when \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()},
#'   which defaults it (when \code{NULL}) to a sibling of \code{model_dir}
#'   under the project root rather than \code{tempdir()} -- see its docs.
#' @param keep_worker_dirs Logical. Keep worker directories after completion
#'   when \code{parallel = TRUE}. Passed to \code{run_lhc_wq_parallel()}.
#' @param lhs_matrix Numeric matrix or \code{NULL}. Internal use -- a
#'   precomputed Latin Hypercube sample matrix (in [0, 1] scale, one row per
#'   sample, one column per parameter) to reuse instead of drawing a fresh
#'   one. Used by \code{run_lhc_wq_parallel()} to share one sample matrix
#'   across its worker processes.
#' @param sample_indices Integer vector or \code{NULL}. Internal use -- which
#'   rows of \code{lhs_matrix} (or of a freshly-drawn sample matrix) this call
#'   should actually run, when only a subset is wanted. Used by
#'   \code{run_lhc_wq_parallel()} to assign each worker its own slice of
#'   samples.
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
#' @param de_parallel Logical. If \code{TRUE}, evaluate each DE generation's
#'   population across a parallel cluster instead of sequentially. Default is
#'   \code{FALSE}. Ignored when \code{use_de = FALSE}.
#' @param de_n_workers Integer or \code{NULL}. Number of workers used when
#'   \code{de_parallel = TRUE}. \code{NULL} (default) uses
#'   \code{detectCores(logical = FALSE) - 1}. Ignored when
#'   \code{de_parallel = FALSE}.
#' @param precomputed_lhc_results Data frame or \code{NULL}. Internal use --
#'   when supplied (already in the shape \code{run_lhc_wq()}'s own LHC phase
#'   would produce, including the \code{attr(., "best_parameter_set")}
#'   attribute when applicable), the LHC sampling loop is skipped entirely and
#'   these results are used directly to seed the DE phase. Used by
#'   \code{run_lhc_wq_parallel()} to seed DE from its already-computed
#'   parallel LHC results instead of re-running LHC sequentially.
#'
#' @return If \code{obs_file = NULL}, a list of length \code{n_samples} with
#'   sampled parameters and metrics per run. When \code{use_de = TRUE}, includes
#'   additional \code{de_phase} element with DEoptim results. If \code{obs_file}
#'   is supplied, returns a flattened data frame with sampled parameters and
#'   summary statistics. In \code{obs_file} mode and when \code{return_best = TRUE},
#'   the returned data.frame includes column \code{is_best}, and attributes
#'   \code{best_parameter_set} and \code{best_metric} (identifying the best LHC
#'   iteration). When \code{use_de = TRUE} is also set, \code{best_parameter_set}
#'   and \code{best_metric} are overwritten with DE's refined optimum (DE runs
#'   after and improves on the LHC seed) -- this is the attribute read by
#'   \code{write_best_calib_to_par_files()} and by
#'   \code{cali_ensemble_wq()$best_parameter_sets}, so both automatically use
#'   the DE result when DE was run. The original LHC-phase best is preserved
#'   under attribute \code{lhc_best_parameter_set} for comparison. The DEoptim
#'   object itself and its raw best member are always available via attributes
#'   \code{de_phase} and \code{de_best_params}.
#' @export

run_lhc_wq <- function(model,
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
                       parallel        = FALSE,
                       n_workers       = NULL,
                       parallel_dir    = NULL,
                       keep_worker_dirs = FALSE,
                       lhs_matrix      = NULL,
                       sample_indices  = NULL,
                       use_de          = FALSE,
                       de_iterations   = 50,
                       de_popsize      = NULL,
                       de_f            = 0.8,
                       de_cr           = 0.9,
                       de_seed_from_lhc = TRUE,
                       de_parallel     = FALSE,
                       de_n_workers    = NULL,
                       precomputed_lhc_results = NULL) {

  # Helper: Initialize DE population from best LHC results
  #
  # Returns a (de_popsize_val x length(param_names_ref)) matrix in actual
  # parameter units, suitable to pass straight through as
  # DEoptim.control(initial.pop = ...).
  .init_de_population_from_lhc <- function(lhc_results, lhs_matrix_ref,
                                         param_names_ref, bounds_ref,
                                         de_popsize_val) {

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  # lhs_matrix_ref holds the LHS design in [0,1] per column -- the same
  # values the main LHC loop scales via `lhs_matrix[i, j] * (ub - lb) + lb`
  # before running the model. DEoptim's initial.pop must be in actual
  # parameter units (matching `lower`/`upper`), not raw [0,1] design units,
  # so any rows selected below must go through this same scaling.
  .scale_lhs_rows <- function(rows) {
    scaled <- rows
    for (j in seq_along(param_names_ref)) {
      # Indexed positionally (bounds_ref[[j]]), matching the same
      # disambiguation used to build bounds_ref -- see row_for_param in the
      # main function body. A name lookup here would collapse duplicate
      # `pars` names (different groups sharing one parameter) again.
      lb <- bounds_ref[[j]]["lb"]
      ub <- bounds_ref[[j]]["ub"]
      scaled[, j] <- rows[, j] * (ub - lb) + lb
    }
    colnames(scaled) <- names(bounds_ref)
    scaled
  }

  # Rank in the same "higher transformed value = better" direction the real
  # best-parameter-set selector uses after the LHC loop (see score_transform
  # there): raw value for KGE/NSE, sign-flipped for RMSE/NRMSE (lower raw is
  # better), and -abs() for PBIAS (closer to zero is better). Without this,
  # seeding with best_metric = RMSE/NRMSE/PBIAS would rank the *worst*
  # fitting LHC samples as "best".
  metric_col <- best_metric
  score_sign <- if (metric_col %in% c("RMSE", "NRMSE")) -1 else 1
  score_transform <- function(x) {
    if (metric_col == "PBIAS") return(-abs(x))
    score_sign * x
  }

  # ------------------------------------------------------------
  # CASE 1: obs_file mode -> data.frame
  # ------------------------------------------------------------
  if (is.data.frame(lhc_results)) {

    df <- lhc_results
    df <- df[df$model_ok %in% TRUE, ]
    df <- df[!is.na(df[[metric_col]]), ]

    if (nrow(df) == 0) {
      stop("[DE] No valid LHC results to seed DE")
    }

    # Weighted mean per sample_index (weighted by n_pairs, matching the main
    # best-set selector), in the transformed "higher is better" direction.
    iter_ids <- sort(unique(df$sample_index))
    iter_fitness <- vapply(iter_ids, function(iter_id) {
      sub  <- df[df$sample_index == iter_id, , drop = FALSE]
      vals <- score_transform(sub[[metric_col]])
      w    <- suppressWarnings(as.numeric(sub$n_pairs))
      w[is.na(w) | w <= 0] <- 1
      ok <- is.finite(vals) & is.finite(w)
      if (!any(ok)) return(NA_real_)
      stats::weighted.mean(vals[ok], w = w[ok], na.rm = TRUE)
    }, numeric(1))

    agg <- data.frame(sample_index = iter_ids, fitness = iter_fitness)
    agg <- agg[!is.na(agg$fitness), , drop = FALSE]
    if (nrow(agg) == 0) {
      stop("[DE] No valid LHC results to seed DE")
    }

    # sort best first
    agg <- agg[order(agg$fitness, decreasing = TRUE), ]

    # sample_index is already the row number in lhs_matrix_ref (it's assigned
    # from that same row during the LHC loop), so it can be used directly.
    top_indices <- agg$sample_index[seq_len(min(de_popsize_val, nrow(agg)))]

    if (length(top_indices) < de_popsize_val) {
      top_indices <- c(top_indices,
                       rep(top_indices,
                           length.out = de_popsize_val - length(top_indices)))
    }

    return(.scale_lhs_rows(lhs_matrix_ref[top_indices, , drop = FALSE]))
  }

  # ------------------------------------------------------------
  # CASE 2: list mode (no obs_file)
  # ------------------------------------------------------------
  # cal_metrics() output (stored in r$metrics) is raw/derived model output,
  # not a goodness-of-fit score, so there is nothing to rank by in this mode.
  # r$obs_stats is only ever populated when obs_data_loaded is non-NULL, and
  # in that case `results` is always flattened into a data.frame before this
  # function is called (see above) -- so this branch is reached only when
  # obs_stats is NULL for every result, and the lookup below intentionally
  # falls back to taking the first de_popsize_val LHC samples in their
  # original order.
  lhc_fitness <- lapply(lhc_results, function(r) {
    if (!is.null(r$obs_stats) && length(r$obs_stats) > 0) {
      vals <- vapply(r$obs_stats, function(st) st[[metric_col]] %||% NA_real_, numeric(1))
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) return(mean(score_transform(vals)))
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

  .scale_lhs_rows(lhs_matrix_ref[top_indices, , drop = FALSE])
}

.make_de_objective <- function(obs_data_loaded, dict_loaded, model_short, yaml_file,
                               wq_config_file, obs_to_model_units, spin_up_days,
                               stats_by_depth, target_variables, best_metric,
                               model_dir, param_names, calib_setup, row_for_param,
                               model = NULL, verbose = FALSE,
                               yaml_file_model = "gotm.yaml", par_file = "simstrat.par") {
  
  thread_eval_counter <- 0 
  best_metric_upper <- toupper(as.character(best_metric[1]))
  score_sign <- if (best_metric_upper %in% c("RMSE", "NRMSE")) 1 else -1
  stats_fn <- .cal_lhc_obs_stats
  
  function(x_scaled) {
    thread_eval_counter <<- thread_eval_counter + 1

    cat(sprintf(
      "[DE eval %d] pid=%d time=%s x=%s\n",
      thread_eval_counter,
      Sys.getpid(),
      format(Sys.time(), "%H:%M:%OS3"),
      paste(round(x_scaled, 4), collapse = ", ")
    ), file = stdout(), fill = TRUE)
    flush.console()

    unique_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%OS3")
    unique_run_id <- paste0("de_worker_pid", Sys.getpid(), "_", unique_timestamp, "_", sample(100000:999999, 1))
    project_root <- dirname(model_dir)
    eval_dir <- file.path(project_root, unique_run_id)
    debug_dir <- file.path(model_dir, "de_debug_logs")
    if (!dir.exists(debug_dir)) dir.create(debug_dir, recursive = TRUE, showWarnings = FALSE)
    debug_log <- file.path(debug_dir, sprintf("de_worker_%s_%s.log", Sys.getpid(), thread_eval_counter))

    if (dir.exists(eval_dir)) unlink(eval_dir, recursive = TRUE, force = TRUE)
    dir.create(eval_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(eval_dir, recursive = TRUE, force = TRUE), add = TRUE)

    cat(sprintf("[%s] DE objective started on worker pid=%s eval=%s uid=%s dir=%s\n",
                format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), Sys.getpid(), thread_eval_counter,
                unique_run_id, eval_dir),
        file = debug_log, append = TRUE)

    all_files <- list.files(model_dir, full.names = TRUE)
    files_to_copy <- all_files[!grepl("de_worker_", basename(all_files))]
    file.copy(from = files_to_copy, to = eval_dir, recursive = TRUE, overwrite = TRUE)

    # Copy project-root yaml siblings (LakeEnsemblR_WQ.yaml, LakeEnsemblR.yaml,
    # etc.) into eval_dir -- EXCEPT anything named "output.yaml"/"Output.yaml"
    # (any case). GOTM reserves that exact filename for its OWN native
    # output-stream config, already copied in from model_dir just above.
    # yaml_file (this function's metric-config argument) is almost always
    # this package's own "Output.yaml" -- copying it here under its literal
    # name would silently clobber GOTM's file on Windows (case-insensitive
    # filesystem), corrupting it and crashing the model engine with a yaml
    # parse error ("single- and double-quoted strings are not supported")
    # while looking like an ordinary model failure. Confirmed happening in
    # practice: GOTM-WET/GOTM-Selmaprotbas DE phases silently failed every
    # single evaluation this way, each one just returning the 1e6 penalty,
    # producing a perfectly flat "convergence" trace with nothing behind it.
    # Sandbox yaml_file itself under a collision-safe name below instead --
    # the same fix already applied to run_lhc_wq_parallel.R's worker
    # sandboxing, just missing here since this is a separate code path.
    root_yamls_all <- list.files(project_root, pattern = "\\.yaml$", full.names = TRUE)
    yaml_file_src <- root_yamls_all[tolower(basename(root_yamls_all)) == tolower(basename(yaml_file))][1]
    root_yamls <- root_yamls_all[tolower(basename(root_yamls_all)) != "output.yaml"]
    if (length(root_yamls) > 0) {
      file.copy(from = root_yamls, to = eval_dir, overwrite = TRUE)
    }

    sandbox_yaml_path <- file.path(eval_dir, "__de_worker_output_config.yaml")
    if (!is.na(yaml_file_src) && file.exists(yaml_file_src)) {
      file.copy(yaml_file_src, sandbox_yaml_path, overwrite = TRUE)
    }
    if (file.exists(sandbox_yaml_path)) {
      yaml_content <- yaml::read_yaml(sandbox_yaml_path)
      # If 'folder' is blank, load_config() would otherwise default to this
      # sandbox copy's own directory (eval_dir). But eval_dir only holds a
      # copy of model_dir's contents plus the root *.yaml files -- sibling
      # data files (bathymetry, metrics dict, etc.) live in project_root and
      # were never copied in, so pin 'folder' to project_root explicitly.
      if (is.null(yaml_content$folder) || !nzchar(yaml_content$folder)) {
        yaml_content$folder <- project_root
      }
      model_key <- names(yaml_content$model_folders)[toupper(names(yaml_content$model_folders)) == toupper(model_short)][1]
      if (!is.na(model_key)) {
        # Preserve the original output subfolder's basename/case; it was
        # copied as-is from model_dir into eval_dir above, so it may not be
        # "Output" (e.g. "output" on a case-sensitive Linux filesystem).
        orig_output_subdir <- basename(yaml_content$model_folders[[model_key]])
        yaml_content$model_folders[[model_key]] <- file.path(eval_dir, orig_output_subdir)
      }
      yaml::write_yaml(yaml_content, sandbox_yaml_path)
      cat("\n", file = sandbox_yaml_path, append = TRUE)
    }

    for (i in seq_along(param_names)) {
      # calib_setup. sliced to this position's own row (row_for_param[i]) so
      # duplicate `pars` names across groups don't all get overwritten with
      # the same x_scaled[i] -- see row_for_param in the main function body.
      .update_param(
        p = param_names[i], value = x_scaled[i], current_dir = eval_dir,
        calib_setup. = calib_setup[row_for_param[i], , drop = FALSE], model. = model, wq_config_file. = wq_config_file
      )
    }

    run_model_in_eval_dir <- function() {
      out <- switch(toupper(model),
        "GLM-AED2" = {
          if (!requireNamespace("GLM3r", quietly = TRUE)) {
            stop("Package 'GLM3r' is required to run GLM-AED2.")
          }
          GLM3r::run_glm(sim_folder = eval_dir, verbose = verbose)
        },
        "GOTM-WET" = {
          if (!requireNamespace("WETr", quietly = TRUE)) {
            stop("Package 'WETr' is required to run GOTM-WET.")
          }
          WETr::run_wet(sim_folder = eval_dir, yaml_file = yaml_file_model, verbose = verbose)
        },
        "GOTM-SELMAPROTBAS" = {
          if (!requireNamespace("SelmaprotbasR", quietly = TRUE)) {
            stop("Package 'SelmaprotbasR' is required to run GOTM-Selmaprotbas.")
          }
          SelmaprotbasR::run_gotm_sp(sim_folder = eval_dir, yaml_file = yaml_file_model, verbose = verbose)
        },
        "SIMSTRAT-AED2" = {
          if (!requireNamespace("SimstratR", quietly = TRUE)) {
            stop("Package 'SimstratR' is required to run Simstrat-AED2.")
          }
          SimstratR::run_simstrat(sim_folder = eval_dir, par_file = par_file, verbose = verbose)
        },
        stop("Unsupported model: ", model)
      )
      # See the matching check in the non-DE .run_model() above: system2()'s
      # non-zero exit status is only ever a warning, never an R error, so a
      # crashed model engine would otherwise be silently treated as success.
      status <- attr(out, "status")
      if (!is.null(status) && !isTRUE(status == 0)) {
        out_text <- if (is.character(out) && length(out) > 0) paste(out, collapse = "\n") else "(no stdout captured)"
        stop("Model engine exited with non-zero status (", status, "). ",
             "The simulation likely failed to run -- check for missing input ",
             "files or a config error. Engine output:\n", out_text)
      }
      invisible(out)
    }

    model_ok <- tryCatch({
      old_wd <- getwd()
      setwd(eval_dir)
      on.exit(setwd(old_wd), add = TRUE)
      run_model_in_eval_dir()
      cat(sprintf("[%s] worker pid=%s eval=%s uid=%s model run OK dir=%s\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), Sys.getpid(), thread_eval_counter,
                  unique_run_id, eval_dir),
          file = debug_log, append = TRUE)
      TRUE
    }, error = function(e) {
      cat(sprintf("[%s] worker pid=%s eval=%s model run ERROR: %s\n",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), Sys.getpid(), thread_eval_counter, e$message),
          file = debug_log, append = TRUE)
      message("--- MODEL ENGINE RUN ERROR: ", e$message)
      FALSE
    })

    if (!model_ok) return(1e6)

    if (!is.null(obs_data_loaded)) {
      obs_stats <- stats_fn(
        obs_data           = obs_data_loaded,
        dict               = dict_loaded,
        model_short        = model_short,
        yaml_file          = sandbox_yaml_path,
        wq_config_file     = file.path(eval_dir, wq_config_file),
        verbose            = TRUE,
        obs_to_model_units = obs_to_model_units,
        spin_up_days       = spin_up_days,
        stats_by_depth     = stats_by_depth,
        target_variables   = target_variables
      )

      if (is.null(obs_stats) || length(obs_stats) == 0L) return(Inf)

      if (is.data.frame(obs_stats)) {
        vals <- obs_stats[[best_metric_upper]]
        w    <- suppressWarnings(as.numeric(obs_stats$n_pairs))
      } else {
        vals <- vapply(obs_stats, function(st) {
          if (is.null(st) || !is.list(st)) return(NA_real_)
          st[[best_metric_upper]]
        }, numeric(1))
        w <- vapply(obs_stats, function(st) {
          if (is.null(st) || !is.list(st)) return(NA_real_)
          suppressWarnings(as.numeric(st$n_pairs))
        }, numeric(1))
      }

      w[is.na(w) | w <= 0] <- 1
      ok <- is.finite(vals) & is.finite(w)
      if (!any(ok)) return(Inf)

      mean_metric <- stats::weighted.mean(vals[ok], w = w[ok], na.rm = TRUE)
      unlink(eval_dir, recursive = TRUE, force = TRUE)

      if (best_metric_upper == "PBIAS") {
        return(mean(abs(vals[ok]), na.rm = TRUE))
      } else {
        return(mean_metric * score_sign)
      }
    } else {
      unlink(eval_dir, recursive = TRUE, force = TRUE)
      return(Inf)
    }
  }
}

  # Resolve yaml_file_model/par_file before any delegation below, so both the
  # parallel and DE/LHC worker paths receive concrete filenames. Explicit
  # user-supplied values always win; otherwise derive from ler_config_file's
  # config_files section, falling back to the historical hardcoded defaults.
  # Treat both NULL and zero-length (e.g. character(0), which as.character(NULL[1])
  # produces -- as cali_ensemble_wq()'s per-model argument resolution does when
  # yaml_file_model/par_file are left at their NULL defaults) as "not supplied".
  if (is.null(yaml_file_model) || length(yaml_file_model) == 0L) {
    yaml_file_model <- .derive_ler_config_filename(ler_config_file, "GOTM",
                                                    base_dir = dirname(model_dir))
    if (is.null(yaml_file_model) || length(yaml_file_model) == 0L) yaml_file_model <- "gotm.yaml"
  }
  if (is.null(par_file) || length(par_file) == 0L) {
    par_file <- .derive_ler_config_filename(ler_config_file, "Simstrat",
                                            base_dir = dirname(model_dir))
    if (is.null(par_file) || length(par_file) == 0L) par_file <- "simstrat.par"
  }

  # Resolve obs_file to an absolute path now, in the main session, before any
  # delegation to parallel/DE workers below. Workers are separate processes
  # that may not share the main session's current working directory by the
  # time they actually run (e.g. after a setwd() elsewhere in the session
  # left the main process's cwd somewhere unexpected) -- a relative obs_file
  # would then silently fail to resolve inside the worker even though it was
  # valid at the top-level call site.
  if (!is.null(obs_file) && length(obs_file) > 0L && nzchar(obs_file)) {
    if (!file.exists(obs_file)) {
      stop("obs_file does not exist: ", obs_file)
    }
    obs_file <- normalizePath(obs_file, winslash = "/", mustWork = TRUE)
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
      target_variables = target_variables,
      ler_config_file = ler_config_file,
      n_workers = n_workers,
      parallel_dir = parallel_dir,
      keep_worker_dirs = keep_worker_dirs,
      use_de = use_de,
      de_parallel = de_parallel,
      de_n_workers = de_n_workers,
      de_iterations = de_iterations,
      de_popsize = de_popsize,
      de_f = de_f,
      de_cr = de_cr,
      de_seed_from_lhc = de_seed_from_lhc
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
  model_dir <- normalizePath(model_dir, winslash = "/", mustWork = TRUE)

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

    # Load metrics dictionary from configured file or bundled defaults. Only
    # require this run's model folder to exist (see .cal_lhc_obs_stats()'s
    # load_config() call above for the same rationale).
    cfg_for_dict <- load_config(yaml_file, required_models = model_short)
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
  # Snapshot model_dir before the LHC loop starts mutating it in place, and
  # restore those files via on.exit once this call returns -- including on
  # error/interrupt, since on.exit(add = TRUE) handlers still fire then.
  # tempfile() gives each call its own baseline dir; the previous version
  # used one fixed path (tempdir()/model_baseline_copy) reused across the
  # whole R session, so after the first call it silently stopped snapshotting
  # anything for subsequent calls/models and was never read back regardless.
  # ------------------------------------------------------------------
  baseline_dir <- tempfile("lhc_baseline_")
  dir.create(baseline_dir, recursive = TRUE)
  file.copy(
    from = list.files(model_dir, full.names = TRUE),
    to   = baseline_dir,
    recursive = TRUE,
    overwrite = TRUE
  )
  on.exit({
    file.copy(
      from = list.files(baseline_dir, full.names = TRUE),
      to   = model_dir,
      recursive = TRUE,
      overwrite = TRUE
    )
    unlink(baseline_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  # Disambiguate parameter positions that share the same `pars` string across
  # different calib_setup rows -- e.g. one physical parameter (like
  # "R_growth") calibrated independently for two phytoplankton groups, which
  # both keep the same `pars` value and are told apart only by `group_name`.
  # Everything below this point used to look values up by NAME alone
  # (`bounds[[p]]`, `param_values[p]`, `calib_setup[calib_setup$pars == p, ]`)
  # -- with duplicate names that silently collapses onto whichever row/value
  # is found *first*, wasting one LHS dimension and forcing both groups onto
  # one shared value. row_for_param[j] pins LHS/param_names position j to the
  # correct, distinct calib_setup row (its j-th occurrence of that name, in
  # calib_setup's own row order), and param_key gives each position a unique
  # label to store/report results under, so nothing downstream needs to
  # match by name again.
  .occurrence_index <- function(x) stats::ave(seq_along(x), x, FUN = seq_along)
  .pname_occurrence  <- .occurrence_index(param_names)
  .setup_occurrence  <- .occurrence_index(calib_setup$pars)
  row_for_param <- vapply(seq_along(param_names), function(j) {
    p   <- param_names[j]
    occ <- .pname_occurrence[j]
    cand <- which(calib_setup$pars == p & .setup_occurrence == occ)
    if (length(cand) == 0L) cand <- which(calib_setup$pars == p)
    if (length(cand) == 0L) {
      stop("Parameter '", p, "' not found in calib_setup$pars.")
    }
    cand[1]
  }, integer(1))
  param_key <- make.unique(param_names)

  # Pre-collect bounds for each parameter (indexed positionally by
  # row_for_param, not by name -- see above).
  bounds <- lapply(seq_along(param_names), function(j) {
    row <- calib_setup[row_for_param[j], , drop = FALSE]
    c(lb = row$lb[1], ub = row$ub[1])
  })
  names(bounds) <- param_key

  # Model runner by coupling
  .run_model <- function() {
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
    # These runners wrap system2(), whose non-zero exit status is only ever
    # surfaced as an R *warning* ("... had status N"), never an error -- so a
    # crashed model engine was previously indistinguishable from a successful
    # run here, and model_ok was silently set to TRUE regardless. Escalate a
    # non-zero exit status to a real error so the caller's tryCatch(model_ok)
    # correctly detects the failure instead of re-scoring stale output.nc.
    status <- attr(out, "status")
    if (!is.null(status) && !isTRUE(status == 0)) {
      # out itself holds the model engine's captured stdout (system2(...,
      # stdout = TRUE) captures rather than prints it) -- this is usually
      # where the actual reason for the crash is, not just "status 1".
      out_text <- if (is.character(out) && length(out) > 0) paste(out, collapse = "\n") else "(no stdout captured)"
      stop("Model engine exited with non-zero status (", status, "). ",
           "The simulation likely failed to run -- check for missing input ",
           "files or a config error. Engine output:\n", out_text)
    }
    invisible(out)
  }

.update_param <- function(p, value, current_dir = model_dir, calib_setup. = calib_setup,
                          model. = model, wq_config_file. = wq_config_file) {
  if (missing(current_dir)) current_dir <- get("model_dir", envir = parent.frame())
  if (missing(calib_setup.)) calib_setup. <- get("calib_setup", envir = parent.frame())
  if (missing(model.)) model. <- get("model", envir = parent.frame())
  if (missing(wq_config_file.)) wq_config_file. <- get("wq_config_file", envir = parent.frame())

  model_upper <- toupper(model.)
  rows <- calib_setup.[calib_setup.$pars == p, ]

  for (k in seq_len(nrow(rows))) {
    file_or_path <- as.character(rows$file[k])
    param_path <- file.path(current_dir, file_or_path)

    if (grepl("\\.nml$", file_or_path, ignore.case = TRUE)) {
      
      # SAFE INTERCEPT: If it's an AED2 file, do NOT use glmtools!
      if (model_upper %in% c("GLM-AED2", "SIMSTRAT-AED2") || grepl("aed2", basename(param_path), ignore.case = TRUE)) {
        
        path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
        nml_lines <- readLines(param_path, warn = FALSE)
        
        # If it's a standard dictionary path format (e.g., "aed2_oxygen/Fsed_oxy")
        if (length(path_parts) == 2L) {
          target_sec <- paste0("&", trimws(path_parts[1]))
          target_var <- trimws(path_parts[2])
          
          sec_idx <- which(grepl(paste0("^\\s*", target_sec, "\\b"), nml_lines, ignore.case = TRUE))
          if (length(sec_idx) > 0) {
            sec_start <- sec_idx[1]
            # Find the end of this namelist block (marked by a forward slash)
            slashes <- Jack <- which(grepl("^\\s*/\\s*$", nml_lines))
            sec_end <- slashes[slashes > sec_start][1]
            if (is.na(sec_end)) sec_end <- length(nml_lines)

            group_col_k <- if ("group_name" %in% names(rows)) rows$group_name[k] else NA_character_
            upd <- .update_nml_group_value(nml_lines, sec_start, sec_end, target_var,
                                           value, group_name = group_col_k)
            if (isTRUE(upd$found)) {
              nml_lines <- upd$lines
              writeLines(nml_lines, param_path)
              next # Move to next parameter safely!
            }
          }
        }
        
        # Fallback text substitution if the dictionary path layout varies
        var_pattern <- paste0("^(\\s*", p, "\\s*=\\s*)[^\\s,!;#]*")
        var_idx <- which(grepl(var_pattern, nml_lines, ignore.case = TRUE))
        if (length(var_idx) > 0) {
          nml_lines[var_idx[1]] <- sub(var_pattern, paste0("\\1", value), nml_lines[var_idx[1]], ignore.case = TRUE)
          writeLines(nml_lines, param_path)
          next
        }
      }
      
      # Standard glmtools processing ONLY for native glm3.nml files
      nml <- glmtools::read_nml(param_path)
      nml <- glmtools::set_nml(nml, p, value)
      glmtools::write_nml(nml, param_path)

    } else if (grepl("\\.csv$", file_or_path, ignore.case = TRUE)) {
      df <- readr::read_csv(param_path, show_col_types = FALSE)
      names(df) <- gsub("^['\"]|['\"]$", "", names(df))
      pname_col <- intersect(c("p_name", "pname"), names(df))
      if (length(pname_col) == 0) stop("Could not identify parameter name column.")
      df[[pname_col]] <- gsub("^['\"]|['\"]$", "", df[[pname_col]])
      idx <- which(df[[pname_col]] == p)
      if (length(idx) == 0) stop("Parameter not found.")
      
      group_col <- if ("group_name" %in% names(rows)) rows$group_name[k] else NA_character_
      if (!is.na(group_col) && group_col %in% names(df)) {
        df[idx, group_col] <- value
      } else {
        df[idx, 2:ncol(df)] <- value
      }
      readr::write_csv(df, param_path)

    } else if (grepl("\\.yaml$|\\.yml$", file_or_path, ignore.case = TRUE)) {
      LakeEnsemblR::input_yaml_multiple(file = param_path, value = value,
                                        key1 = p, verbose = FALSE)

    } else if (model_upper %in% c("GLM-AED2", "SIMSTRAT-AED2")) {
      # Custom path logic fallback for dictionary pointers
      if (!requireNamespace("configr", quietly = TRUE)) stop("configr package required.")
      if (is.null(wq_config_file.) || !nzchar(wq_config_file.)) stop("wq_config_file missing.")

      # wq_config_file. may live directly in current_dir (DE-worker sandbox,
      # where root *.yaml files are copied alongside the model folder) or one
      # level up in the real project folder (plain run_lhc_wq() calls, where
      # current_dir is model_dir itself). Try both, plus the path as given.
      wq_yaml_candidates <- c(
        file.path(current_dir, basename(wq_config_file.)),
        wq_config_file.,
        file.path(dirname(current_dir), basename(wq_config_file.))
      )
      wq_yaml_candidates <- unique(normalizePath(wq_yaml_candidates, winslash = "/", mustWork = FALSE))
      isolated_wq_yaml <- wq_yaml_candidates[file.exists(wq_yaml_candidates)][1]
      if (is.na(isolated_wq_yaml) || !nzchar(isolated_wq_yaml)) {
        stop("Could not find wq_config_file '", wq_config_file., "' (looked in ",
             current_dir, " and ", dirname(current_dir), ")")
      }
      lst_cfg <- configr::read.config(isolated_wq_yaml)
      cfg_files <- lst_cfg[["config_files"]]
      model_cfg <- cfg_files[[model.]]
      
      if (is.null(model_cfg) || !nzchar(model_cfg)) {
        cfg_names <- names(cfg_files)
        cfg_idx <- which(toupper(cfg_names) == toupper(model.))[1]
        if (!is.na(cfg_idx)) model_cfg <- cfg_files[[cfg_idx]]
      }
      
      if (model_upper == "SIMSTRAT-AED2" && grepl("\\.par$", model_cfg, ignore.case = TRUE)) {
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

      nml_candidates <- c(file.path(current_dir, model_cfg), file.path(current_dir, basename(model_cfg)))
      nml_candidates <- unique(normalizePath(nml_candidates, winslash = "/", mustWork = FALSE))
      nml_path <- nml_candidates[file.exists(nml_candidates)][1]

      
      
      if (is.na(nml_path) || !nzchar(nml_path)) stop("NML file not found.")

      # Apply text-based search and edit directly to the resolved deep configurations
      path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
      nml_lines <- readLines(nml_path, warn = FALSE)
      target_sec <- paste0("&", trimws(path_parts[1]))
      target_var <- if(length(path_parts) == 2L) trimws(path_parts[2]) else p
      
      sec_idx <- which(grepl(paste0("^\\s*", target_sec, "\\b"), nml_lines, ignore.case = TRUE))
      if (length(sec_idx) > 0) {
        sec_start <- sec_idx[1]
        slashes <- jack <- which(grepl("^\\s*/\\s*$", nml_lines))
        sec_end <- slashes[slashes > sec_start][1]
        if (is.na(sec_end)) sec_end <- length(nml_lines)

        group_col_k <- if ("group_name" %in% names(rows)) rows$group_name[k] else NA_character_
        upd <- .update_nml_group_value(nml_lines, sec_start, sec_end, target_var,
                                       value, group_name = group_col_k)
        if (isTRUE(upd$found)) {
          nml_lines <- upd$lines
          writeLines(nml_lines, nml_path)
        }
      }

    } else if (model_upper %in% c("GOTM-WET", "GOTM-SELMAPROTBAS")) {
      yaml_target <- file.path(current_dir, "fabm.yaml")
      if (!file.exists(yaml_target)) yaml_target <- file.path(current_dir, "lake_ensemblr.yaml")
      path_parts <- strsplit(file_or_path, "/", fixed = TRUE)[[1]]
      group_col <- if ("group_name" %in% names(rows)) rows$group_name[k] else NA_character_
      if (!is.na(group_col)) path_parts[path_parts == "{group_name}"] <- group_col

      arglist <- as.list(path_parts)
      names(arglist) <- paste0("key", seq_along(path_parts))
      arglist$value <- value
      arglist$file <- yaml_target
      arglist$verbose <- FALSE
      do.call(LakeEnsemblR::input_yaml_multiple, args = arglist)
    }
  }
}

  if (!is.null(precomputed_lhc_results)) {
    # Skip LHC sampling entirely -- reuse already-computed results (e.g. from
    # run_lhc_wq_parallel()'s parallel LHC phase) instead of re-running it
    # sequentially just to seed DE.
    results <- precomputed_lhc_results
  } else {

  # Main LHC loop. Iterations update files in-place under model_dir, so this
  # remains sequential unless each run is executed in an isolated copy.
  results <- vector("list", length(sample_indices))

  for (result_idx in seq_along(sample_indices)) {
    i <- sample_indices[result_idx]
    if (verbose) {
      cat("\n[LHC] Iteration", i, "/", total_samples, "\n")
    }

    # Scale LHS values to [lb, ub]. Indexed positionally (bounds[[j]], not
    # bounds[[p]]) so duplicate `pars` names (different groups sharing one
    # physical parameter) each get their own distinct value instead of
    # collapsing onto whichever one a name lookup happens to find first.
    param_values <- setNames(
      vapply(seq_along(param_names), function(j) {
        lb <- bounds[[j]]["lb"]
        ub <- bounds[[j]]["ub"]
        lhs_matrix[i, j] * (ub - lb) + lb
      }, numeric(1)),
      param_key
    )

    if (verbose) {
      for (j in seq_along(param_names)) {
        cat("  ", param_key[j], "=", round(param_values[j], 6), "\n")
      }
    }

    # Write sampled parameters. calib_setup is sliced down to the single row
    # this position corresponds to (row_for_param[j]) -- .update_param()'s
    # own `calib_setup.$pars == p` lookup then has only that one row to
    # match, so each group/duplicate-named parameter is written with its own
    # sampled value instead of every row sharing that `pars` name getting
    # overwritten with the same one.
    for (j in seq_along(param_names)) {
      .update_param(
    p              = param_names[j],
    value          = param_values[j],
    current_dir    = model_dir,        # During LHC, update the master workspace
    calib_setup    = calib_setup[row_for_param[j], , drop = FALSE],
    model          = model,            # Pass the top-level model variable
    wq_config_file = wq_config_file
  )
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
                             target_variables = target_variables,
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
          # first_row's parameter columns are named by param_key (see the
          # results[[result_idx]]$params construction above), not raw
          # param_names -- duplicate `pars` names would otherwise collide.
          for (p in param_key) {
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

  } # end of else (precomputed_lhc_results was NULL) from above

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
    local_obj_fun <- .make_de_objective(
      obs_data_loaded    = obs_data_loaded,
      dict_loaded        = dict_loaded,
      model_short        = model_filter,
      yaml_file          = yaml_file,
      wq_config_file     = wq_config_file,
      obs_to_model_units = obs_to_model_units,
      spin_up_days       = spin_up_days,
      stats_by_depth     = stats_by_depth,
      target_variables   = target_variables,
      best_metric        = best_metric,
      model_dir          = model_dir,
      param_names        = param_names,
      calib_setup        = calib_setup,
      row_for_param      = row_for_param,
      model              = model,
      verbose            = verbose,
      yaml_file_model    = yaml_file_model,
      par_file           = par_file
    )

    obj_fun <- local_obj_fun

    # Build lower and upper bounds vectors for DEoptim. Indexed positionally
    # (bounds[[j]]) -- see row_for_param -- so duplicate `pars` names don't
    # collapse both groups onto the first group's range.
    bounds_lower <- vapply(seq_along(param_names), function(j) bounds[[j]]["lb"], numeric(1))
    bounds_upper <- vapply(seq_along(param_names), function(j) bounds[[j]]["ub"], numeric(1))
    names(bounds_lower) <- param_key
    names(bounds_upper) <- param_key
    
    # Run DE
    if (isTRUE(verbose)) {
      message("[DE] Population size: ", de_popsize, 
              ", Generations: ", de_iterations,
              ", F: ", de_f, ", CR: ", de_cr)
    }
    

    de_parallel_enabled <- isTRUE(de_parallel)
    if (de_parallel_enabled && is.null(de_n_workers)) {
      de_n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
    }
    if (de_parallel_enabled && de_n_workers < 1L) {
      de_n_workers <- 1L
    }

    de_control_args <- list(
      itermax = de_iterations,
      NP = de_popsize,
      F = de_f,
      CR = de_cr,
      trace = if (isTRUE(verbose)) 10 else FALSE,
      packages = c("stats", "utils", "yaml", "readr", "dplyr", "ncdf4", "lubridate", "glmtools", "gotmtools", "configr", "GLM3r")
    )

    # Seed DE's starting population from the best LHC samples (see
    # .init_de_population_from_lhc() above), when requested and available.
    # initial_pop is guaranteed to have exactly de_popsize rows when non-NULL.
    if (isTRUE(de_seed_from_lhc) && !is.null(initial_pop)) {
      de_control_args$initial.pop <- initial_pop
    }

    de_control_formals <- names(formals(DEoptim::DEoptim.control))
    supported_control_args <- intersect(names(de_control_args), de_control_formals)
    de_control_args <- de_control_args[supported_control_args]

    parallel_mode_used <- FALSE
    de_cluster <- NULL
    if (de_parallel_enabled && "cluster" %in% de_control_formals) {
      de_cluster <- parallel::makeCluster(de_n_workers)
      de_control_args$cluster <- de_cluster
      parallel_mode_used <- TRUE
      if (isTRUE(verbose)) {
        message("[DE] Created parallel cluster with ", de_n_workers, " workers")
      }
      worker_pids <- parallel::clusterCall(de_cluster, Sys.getpid)
      if (isTRUE(verbose)) {
        message("[DE] Cluster worker PIDs: ", paste(worker_pids, collapse = ", "))
        message("[DE] Worker debug logs will be written under: ", file.path(model_dir, "de_debug_logs"))
      }
    }

    if (!is.null(de_cluster)) {
      parallel::clusterExport(
        de_cluster,
        varlist = c(
          "obj_fun", "target_variables", "obs_data_loaded",
          ".cal_lhc_obs_stats", "load_config", "expand_templates", "get_output_wq",
          "cal_metrics", "cal_stats", "dict_loaded", "model_short", "yaml_file",
          "wq_config_file", "obs_to_model_units", "spin_up_days", "stats_by_depth",
          "best_metric", "model_dir", "param_names", "calib_setup", "verbose",
          "yaml_file_model", "par_file", "bounds", "model"
        ),
        envir = environment()
      )
    }

    if (isTRUE(verbose)) {
      message(sprintf(
        "[DE] parallelEnabled=%s parallelModeUsed=%s workers=%s",
        if (de_parallel_enabled) "TRUE" else "FALSE",
        if (parallel_mode_used) "TRUE" else "FALSE",
        if (!is.null(de_n_workers)) as.character(de_n_workers) else "NULL"
      ))
    }

    de_control <- do.call(DEoptim::DEoptim.control, de_control_args)

    de_result <- tryCatch(
      DEoptim::DEoptim(
        fn = obj_fun,
        lower = bounds_lower,
        upper = bounds_upper,
        control = de_control
      ),
      error = function(e) {
        if (de_parallel_enabled && !is.null(de_cluster)) {
          message("[DE] Parallel backend failed (", conditionMessage(e), "). Retrying sequentially.")
          de_control$cluster <- NULL
          if ("parallelType" %in% names(de_control)) {
            de_control$parallelType <- 0L
          }
          DEoptim::DEoptim(
            fn = obj_fun,
            lower = bounds_lower,
            upper = bounds_upper,
            control = de_control
          )
        } else {
          stop(conditionMessage(e), call. = FALSE)
        }
      }
    )

    if (isTRUE(verbose)) {
      if (parallel_mode_used) {
        message("[DE] Run completed using parallel cluster with ", de_n_workers, " workers.")
      } else if (de_parallel_enabled) {
        message("[DE] Run completed sequentially after parallel backend could not be used.")
      } else {
        message("[DE] Run completed sequentially (parallel disabled).")
      }
    }

    if (!is.null(de_cluster)) {
      parallel::stopCluster(de_cluster)
      de_cluster <- NULL
    }
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

        # `best_parameter_set` (read by write_best_calib_to_par_files() and by
        # cali_ensemble_wq()$best_parameter_sets) was set above from the LHC
        # phase only -- DE runs *after* that and refines on top of it, so
        # leaving the attribute untouched would silently report the LHC seed
        # as "best" even though DE found something better. Overwrite it with
        # DE's actual optimum here; keep the LHC-phase one under a different
        # name in case it's still wanted for comparison.
        attr(results, "lhc_best_parameter_set") <- attr(results, "best_parameter_set")

        attr(results, "best_parameter_set") <- .de_best_parameter_set(de_result, param_key, best_metric)
        attr(results, "best_metric") <- toupper(as.character(best_metric[1]))

        if (isTRUE(verbose)) {
          message("[DE] Best parameters from DE:")
          for (i in seq_along(param_names)) {
            message("  ", param_key[i], " = ",
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
