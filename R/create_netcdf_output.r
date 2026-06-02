#' Create NetCDF output from model-specific runs
#'
#' Create a NetCDF file from model output lists (e.g., temperature, ice thickness).
#'
#' @param output_lists list; list containing lists of output data.frames.
#' @param folder character; folder that contains model folders.
#' @param model character vector; model names to include (e.g., c("GOTM", "GLM", "Simstrat")).
#' @param out_time data.frame; optional data.frame with datetime column.
#'   If NULL, it is inferred from the first metric data.frame.
#' @param longitude numeric; longitude of lake in decimal degrees. If NULL,
#'   it is inferred from `LakeEnsemblR.yaml` when possible.
#' @param latitude numeric; latitude of lake in decimal degrees. If NULL,
#'   it is inferred from `LakeEnsemblR.yaml` when possible.
#' @param ler_config_file character; optional path to LakeEnsemblR.yaml.
#'   If NULL, the function tries `files$LER_config_file` from `wq_config_file`,
#'   then `folder/LakeEnsemblR.yaml`.
#' @param wq_config_file character; optional path to LakeEnsemblR_WQ.yaml.
#'   Used only to discover `files$LER_config_file` if `ler_config_file` is NULL.
#' @param compression integer; compression level from 1 (least) to 9 (most).
#' @param members integer; number of ensemble members in output NetCDF.
#' @param out_file character; output NetCDF filename.
#'
#' @return Invisibly returns the output NetCDF file path.
#' @export
create_netcdf_output <- function(output_lists,
                                 folder = ".",
                                 model,
                                 out_time = NULL,
                                 longitude = NULL,
                                 latitude = NULL,
                                 ler_config_file = NULL,
                                 wq_config_file = NULL,
                                 compression = 4,
                                 members = 25,
                                 out_file = "ensemble_output.nc") {

  if (!is.list(output_lists) || length(output_lists) == 0) {
    stop("output_lists must be a non-empty list.")
  }
  if (!is.numeric(compression) || compression < 1 || compression > 9) {
    stop("compression must be an integer between 1 and 9.")
  }
  if (!is.numeric(members) || length(members) != 1 || members < 1) {
    stop("members must be a positive integer.")
  }

  .resolve_config_path <- function(path, folder = ".") {
    if (is.null(path) || !nzchar(path)) {
      return(NULL)
    }
    if (grepl("^([A-Za-z]:|/)", path)) {
      return(path)
    }
    file.path(folder, path)
  }

  .as_single_numeric <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(NULL)
    }
    val <- suppressWarnings(as.numeric(x[[1]]))
    if (is.na(val)) {
      return(NULL)
    }
    val
  }

  .infer_coords_from_ler <- function(folder = ".",
                                     ler_config_file = NULL,
                                     wq_config_file = NULL) {
    ler_path <- .resolve_config_path(ler_config_file, folder = folder)

    if (is.null(ler_path)) {
      wq_path <- .resolve_config_path(wq_config_file, folder = folder)
      if (!is.null(wq_path) && file.exists(wq_path)) {
        wq_cfg <- tryCatch(yaml::read_yaml(wq_path), error = function(e) NULL)
        if (!is.null(wq_cfg) && !is.null(wq_cfg$files$LER_config_file)) {
          ler_path <- .resolve_config_path(wq_cfg$files$LER_config_file, folder = folder)
        }
      }
    }

    if (is.null(ler_path)) {
      default_ler <- file.path(folder, "LakeEnsemblR.yaml")
      if (file.exists(default_ler)) {
        ler_path <- default_ler
      }
    }

    if (is.null(ler_path) || !file.exists(ler_path)) {
      return(list(longitude = NULL, latitude = NULL))
    }

    ler_cfg <- tryCatch(yaml::read_yaml(ler_path), error = function(e) NULL)
    if (is.null(ler_cfg) || is.null(ler_cfg$location)) {
      return(list(longitude = NULL, latitude = NULL))
    }

    list(
      longitude = .as_single_numeric(ler_cfg$location$longitude),
      latitude = .as_single_numeric(ler_cfg$location$latitude)
    )
  }

  .normalize_output_lists <- function(x) {
    if (!is.list(x) || length(x) == 0) {
      return(x)
    }

    converted <- list()

    .add_df <- function(var_key, model_name, df) {
      if (!is.data.frame(df)) return()
      if (!var_key %in% names(converted)) converted[[var_key]] <<- list()
      converted[[var_key]][[paste(model_name, var_key, sep = "_")]] <<- df
    }

    .emit_strat <- function(model_name, df) {
      if (!is.data.frame(df) || !all(c("Year", "Strat_Start_Date", "Consecutive_Strat_Days", "Mixing_Start_Date") %in% names(df))) {
        return(FALSE)
      }
      yr <- suppressWarnings(as.integer(df$Year))
      if (all(is.na(yr))) return(FALSE)
      dt <- as.POSIXct(paste0(yr, "-01-01"), tz = "UTC")
      .add_df("Duration_of_Stratification", model_name,
              data.frame(datetime = dt,
                         Duration_of_Stratification = suppressWarnings(as.numeric(df$Consecutive_Strat_Days))))
      .add_df("Stratification_Onset_DOY", model_name,
              data.frame(datetime = dt,
                         Stratification_Onset_DOY = as.integer(format(as.Date(df$Strat_Start_Date), "%j"))))
      .add_df("Mixing_Onset_DOY", model_name,
              data.frame(datetime = dt,
                         Mixing_Onset_DOY = as.integer(format(as.Date(df$Mixing_Start_Date), "%j"))))
      TRUE
    }

    .emit_anoxic <- function(model_name, obj) {
      if (!is.list(obj) || !all(c("AF_yearly", "num_anoxic_days", "anoxic_depths") %in% names(obj))) {
        return(FALSE)
      }
      .add_df("Anoxic_Factor", model_name, obj[["AF_yearly"]])
      .add_df("Number_of_Anoxic_Days", model_name, obj[["num_anoxic_days"]])
      .add_df("Anoxic_Depth", model_name, obj[["anoxic_depths"]])
      TRUE
    }

    for (metric_name in names(x)) {
      metric_block <- x[[metric_name]]
      if (!is.list(metric_block) || length(metric_block) == 0) next

      model_keys <- names(metric_block)
      if (is.null(model_keys)) next

      for (model_name in model_keys) {
        model_obj <- metric_block[[model_name]]
        if (is.null(model_obj)) next

        # Case 1: direct data.frame per model
        if (is.data.frame(model_obj)) {
          if (metric_name == "Duration_of_Stratification" && .emit_strat(model_name, model_obj)) {
            next
          }
          .add_df(metric_name, model_name, model_obj)
          next
        }

        # Case 2: direct anoxic block per model
        if (.emit_anoxic(model_name, model_obj)) {
          next
        }

        # Case 3: model -> instance -> data.frame/list
        if (is.list(model_obj) && length(model_obj) > 0) {
          inst_keys <- names(model_obj)

          if (is.null(inst_keys)) {
            inst_keys <- as.character(seq_along(model_obj))
          }

          for (inst_name in inst_keys) {
            inst_obj <- model_obj[[inst_name]]
            if (is.null(inst_obj)) next

            if (is.data.frame(inst_obj)) {
              if (metric_name == "Duration_of_Stratification" && .emit_strat(model_name, inst_obj)) {
                next
              }
              .add_df(metric_name, model_name, inst_obj)
              next
            }

            if (.emit_anoxic(model_name, inst_obj)) {
              next
            }

            # Case 4: one more nested wrapper: instance -> subinstance -> data.frame
            if (is.list(inst_obj) && length(inst_obj) > 0) {
              sub_keys <- names(inst_obj)
              if (is.null(sub_keys)) {
                sub_keys <- as.character(seq_along(inst_obj))
              }
              for (sub_name in sub_keys) {
                sub_obj <- inst_obj[[sub_name]]
                if (!is.data.frame(sub_obj)) next

                if (metric_name == "Duration_of_Stratification" && .emit_strat(model_name, sub_obj)) {
                  next
                }
                .add_df(metric_name, model_name, sub_obj)
              }
            }
          }
        }
      }
    }

    if (length(converted) == 0) {
      return(x)
    }
    converted
  }

  .find_datetime_col <- function(df) {
    if (!is.data.frame(df) || ncol(df) == 0) {
      return(integer(0))
    }

    nm_low <- tolower(names(df))
    exact_idx <- which(nm_low %in% c("datetime", "date", "time", "timestamp"))
    if (length(exact_idx) > 0) {
      return(exact_idx[1])
    }

    class_idx <- which(vapply(df, function(col) {
      inherits(col, "POSIXt") || inherits(col, "Date")
    }, logical(1)))
    if (length(class_idx) > 0) {
      return(class_idx[1])
    }

    integer(0)
  }

  .sanitize_metric_df <- function(df) {
    if (!is.data.frame(df)) {
      return(NULL)
    }

    dt_col <- .find_datetime_col(df)
    if (length(dt_col) == 0) {
      # Support yearly metrics by constructing datetime from Year/Years.
      nm_low <- tolower(names(df))
      year_col <- which(nm_low %in% c("year", "years"))

      if (length(year_col) == 0) {
        return(NULL)
      }

      year_vals <- suppressWarnings(as.integer(df[[year_col[1]]]))
      if (all(is.na(year_vals))) {
        return(NULL)
      }

      datetime_vals <- as.POSIXct(paste0(year_vals, "-01-01"), tz = "UTC")
      # Move synthetic datetime to first column and keep other value columns.
      keep_value_cols <- setdiff(seq_len(ncol(df)), year_col[1])
      df <- cbind(datetime = datetime_vals, df[, keep_value_cols, drop = FALSE])
      dt_col <- 1L
    }

    # Keep only the first datetime column if duplicates exist.
    dt_name_low <- tolower(names(df))
    dup_dt_cols <- which(dt_name_low %in% c("datetime", "date", "time", "timestamp"))
    keep_cols <- c(dt_col[1], setdiff(seq_len(ncol(df)), unique(c(dt_col, dup_dt_cols))))
    df <- df[, keep_cols, drop = FALSE]
    names(df)[1] <- "datetime"

    # Ensure deterministic unique names for depth/value columns.
    if (ncol(df) > 1) {
      names(df)[-1] <- make.unique(names(df)[-1])
    }

    df
  }

  output_lists <- .normalize_output_lists(output_lists)

  for (i in seq_len(length(output_lists))) {
    if (!is.list(output_lists[[i]]) || length(output_lists[[i]]) == 0) {
      next
    }
    cleaned_var <- list()
    for (j in seq_len(length(output_lists[[i]]))) {
      cleaned_df <- .sanitize_metric_df(output_lists[[i]][[j]])
      if (!is.null(cleaned_df)) {
        cleaned_var[[length(cleaned_var) + 1]] <- cleaned_df
        names(cleaned_var)[length(cleaned_var)] <- names(output_lists[[i]])[j]
      }
    }
    output_lists[[i]] <- cleaned_var
  }

  # Keep only variables that still contain at least one valid time-series data.frame.
  output_lists <- output_lists[vapply(output_lists, function(v) is.list(v) && length(v) > 0, logical(1))]
  if (length(output_lists) == 0) {
    stop("No time-series metrics found to write. Provide metrics with a datetime/date/time column.")
  }

  if (is.null(out_time)) {
    inferred_df <- NULL
    for (i in seq_len(length(output_lists))) {
      var_block <- output_lists[[i]]
      if (!is.list(var_block) || length(var_block) == 0) {
        next
      }
      for (j in seq_len(length(var_block))) {
        candidate <- var_block[[j]]
        if (is.data.frame(candidate) && "datetime" %in% names(candidate)) {
          inferred_df <- candidate
          break
        }
      }
      if (!is.null(inferred_df)) {
        break
      }
    }

    if (is.null(inferred_df)) {
      stop("Could not infer out_time: no data.frame with a datetime column was found in output_lists.")
    }
    out_time <- data.frame(datetime = inferred_df$datetime)
  }

  if (!is.data.frame(out_time) || !"datetime" %in% names(out_time)) {
    stop("out_time must be NULL or a data.frame with a 'datetime' column.")
  }

  if (is.null(longitude) || is.null(latitude)) {
    inferred_coords <- .infer_coords_from_ler(
      folder = folder,
      ler_config_file = ler_config_file,
      wq_config_file = wq_config_file
    )
    if (is.null(longitude)) {
      longitude <- inferred_coords$longitude
    }
    if (is.null(latitude)) {
      latitude <- inferred_coords$latitude
    }
  }

  if (is.null(longitude) || is.na(longitude)) {
    longitude <- 0
  }
  if (is.null(latitude) || is.na(latitude)) {
    latitude <- 0
  }

  if (!dir.exists(file.path(folder, "output"))) {
    message("Creating directory for output: ", file.path(folder, "output"))
    dir.create(file.path(folder, "output"), showWarnings = FALSE, recursive = TRUE)
  }

  message("Writing NetCDF file... [", Sys.time(), "]")

  ref_time <- as.POSIXct("1970-01-01 00:00:00", tz = "GMT")
  out_datetime <- as.POSIXct(out_time$datetime, tz = "UTC")
  nsecs <- as.numeric(difftime(out_datetime, ref_time, units = "secs"))

  xvals <- ifelse(longitude >= 0, longitude, longitude + 360)
  yvals <- latitude

  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.double(xvals))
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.double(yvals))
  time_dim <- ncdf4::ncdim_def(
    "time",
    units = "seconds since 1970-01-01 00:00:00",
    vals = as.double(nsecs),
    calendar = "proleptic_gregorian"
  )

  mod_names <- c(model, "Obs")
  model_dim <- ncdf4::ncdim_def("model", units = "-", vals = as.double(seq_len(length(mod_names))))
  member_dim <- ncdf4::ncdim_def("member", units = "", unlim = TRUE,
                                 vals = as.double(seq_len(as.integer(members))))

  fillvalue <- 1e20

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  .infer_var_name <- function(var_entry, fallback_name = "") {
    fallback <- gsub("_list$", "", fallback_name)
    if (nzchar(fallback)) return(fallback)

    nm <- names(var_entry)
    if (length(nm) > 0 && nzchar(nm[1]) && grepl("_", nm[1], fixed = TRUE)) {
      parts <- strsplit(nm[1], "_", fixed = TRUE)[[1]]
      candidate <- paste(parts[2:length(parts)], collapse = "_")
      if (nzchar(candidate)) return(candidate)
    }

    "variable"
  }

  .lookup_unit <- function(variable_name) {
    unit <- "-"

    if (exists("wq_var_dic", inherits = TRUE)) {
      wq_dic <- get("wq_var_dic", inherits = TRUE)
      idx_std <- which(wq_dic$standard_name == variable_name)
      if (length(idx_std) > 0) {
        return(as.character(wq_dic$unit[idx_std[1]]))
      }
      idx_short <- which(wq_dic$short_name == variable_name)
      if (length(idx_short) > 0) {
        return(as.character(wq_dic$unit[idx_short[1]]))
      }
    }

    if (exists("lake_var_dic", inherits = TRUE)) {
      l_dic <- get("lake_var_dic", inherits = TRUE)
      if ("standard_name" %in% names(l_dic) && "unit" %in% names(l_dic)) {
        idx <- which(l_dic$standard_name == variable_name)
        if (length(idx) > 0) {
          unit <- as.character(l_dic$unit[idx[1]])
        }
      }
    }

    unit
  }

  .get_depths_from_df <- function(df) {
    if (ncol(df) <= 2) {
      return(numeric(0))
    }

    depth_names <- names(df)[-1]
    dep <- suppressWarnings(as.numeric(gsub("^Depth_", "", depth_names)))

    if (all(!is.na(dep))) {
      return(dep)
    }

    # Fallback: extract numeric tokens from generic column names like z-1.5m.
    dep <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", depth_names)))

    if (all(!is.na(dep))) {
      return(dep)
    }

    # Final fallback: preserve column order as synthetic depth axis.
    seq(0, ncol(df) - 2)
  }

  .time_index_map <- function(df_datetime, target_datetime) {
    src <- as.numeric(as.POSIXct(df_datetime, tz = "UTC"))
    tgt <- as.numeric(as.POSIXct(target_datetime, tz = "UTC"))
    match(src, tgt)
  }

  # Build a shared depth axis for all 3D variables.
  all_depths <- numeric(0)
  for (i in seq_len(length(output_lists))) {
    first_df <- output_lists[[i]][[1]]
    if (!is.null(first_df) && is.data.frame(first_df) && ncol(first_df) > 2) {
      all_depths <- c(all_depths, .get_depths_from_df(first_df))
    }
  }
  all_depths <- sort(unique(as.numeric(all_depths)))

  depth_dim <- NULL
  if (length(all_depths) > 0) {
    depth_dim <- ncdf4::ncdim_def("z", units = "meters", vals = as.double(-all_depths),
                                  longname = "Depth from surface")
  }

  nc_vars <- vector("list", length(output_lists))
  var_meta <- vector("list", length(output_lists))

  for (i in seq_len(length(output_lists))) {
    var_name <- .infer_var_name(output_lists[[i]], names(output_lists)[i] %||% "")
    var_unit <- .lookup_unit(var_name)

    first_df <- output_lists[[i]][[1]]
    is_2d <- is.data.frame(first_df) && ncol(first_df) == 2
    is_3d <- is.data.frame(first_df) && ncol(first_df) > 2

    if (!is_2d && !is_3d) {
      stop("Each entry in output_lists must contain data.frames with datetime + value/depth columns.")
    }

    if (is_2d) {
      nc_vars[[i]] <- ncdf4::ncvar_def(
        var_name,
        var_unit,
        list(lon_dim, lat_dim, member_dim, model_dim, time_dim),
        fillvalue,
        var_name,
        prec = "float",
        compression = as.integer(compression),
        shuffle = FALSE
      )
      var_meta[[i]] <- list(is_3d = FALSE, variable_name = var_name)
    }

    if (is_3d) {
      if (is.null(depth_dim)) {
        stop("Could not determine depth dimension for 3D output.")
      }

      nc_vars[[i]] <- ncdf4::ncvar_def(
        var_name,
        var_unit,
        list(lon_dim, lat_dim, member_dim, model_dim, time_dim, depth_dim),
        fillvalue,
        var_name,
        prec = "float",
        compression = as.integer(compression),
        shuffle = FALSE
      )
      var_meta[[i]] <- list(is_3d = TRUE, variable_name = var_name)
    }
  }

  names(nc_vars) <- names(output_lists)

  fname <- file.path(folder, "output", out_file)
  if (file.exists(fname)) {
    unlink(fname, recursive = TRUE)
  }

  ncout <- ncdf4::nc_create(fname, nc_vars, force_v4 = TRUE)

  on.exit({
    try(ncdf4::nc_close(ncout), silent = TRUE)
  }, add = TRUE)

  if (length(all_depths) > 0) {
    ncdf4::ncatt_put(ncout, "z", attname = "coordinates", attval = c("z"))
  }
  ncdf4::ncatt_put(ncout, "model", attname = "Model",
                   attval = paste(seq_len(length(mod_names)), "-", mod_names, collapse = ", "))
  ncdf4::ncatt_put(ncout, "member", attname = "member", attval = c(members))

  for (i in seq_len(length(output_lists))) {
    if (!isTRUE(var_meta[[i]]$is_3d)) {
      arr <- array(NA_real_, dim = c(1, 1, as.integer(members), length(mod_names), length(nsecs)))

      for (j in seq_len(length(output_lists[[i]]))) {
        entry_name <- names(output_lists[[i]])[j] %||% ""
        model_name <- strsplit(entry_name, "_", fixed = TRUE)[[1]][1] %||% ""
        model_idx <- which(mod_names == model_name)

        if (length(model_idx) == 0) {
          next
        }

        entry_df <- output_lists[[i]][[j]]
        vals <- suppressWarnings(as.numeric(entry_df[, -1, drop = TRUE]))
        idx_time <- .time_index_map(entry_df$datetime, out_datetime)
        ok <- which(!is.na(idx_time))
        if (length(ok) > 0) {
          arr[1, 1, 1, model_idx, idx_time[ok]] <- vals[ok]
        }
      }

      ncdf4::ncvar_put(ncout, nc_vars[[i]], arr)
      ncdf4::ncatt_put(ncout, nc_vars[[i]], attname = "coordinates", attval = c("lon lat member model"))
      ncdf4::ncvar_change_missval(ncout, nc_vars[[i]], missval = fillvalue)
    } else {
      arr <- array(NA_real_, dim = c(1, 1, as.integer(members), length(mod_names),
                                     length(nsecs), length(all_depths)))

      for (j in seq_len(length(output_lists[[i]]))) {
        entry_df <- output_lists[[i]][[j]]
        entry_name <- names(output_lists[[i]])[j] %||% ""
        model_name <- strsplit(entry_name, "_", fixed = TRUE)[[1]][1] %||% ""
        model_idx <- which(mod_names == model_name)

        if (length(model_idx) == 0) {
          next
        }

        deps_tmp <- .get_depths_from_df(entry_df)
        mat_df <- entry_df[, -1, drop = FALSE]
        mat <- as.matrix(as.data.frame(lapply(mat_df, function(col) suppressWarnings(as.numeric(col))),
                     check.names = FALSE, stringsAsFactors = FALSE))

        aligned <- matrix(NA_real_, nrow = length(nsecs), ncol = length(all_depths))
        idx_time <- .time_index_map(entry_df$datetime, out_datetime)
        ok_time <- which(!is.na(idx_time))

        for (k in seq_len(ncol(mat))) {
          dep_col <- which(all_depths == deps_tmp[k])
          if (length(dep_col) > 0 && length(ok_time) > 0) {
            aligned[idx_time[ok_time], dep_col] <- mat[ok_time, k]
          }
        }

        arr[1, 1, 1, model_idx, , ] <- aligned
      }

      ncdf4::ncvar_put(nc = ncout, varid = nc_vars[[i]], vals = arr)
      ncdf4::ncatt_put(ncout, nc_vars[[i]], attname = "coordinates", attval = c("lon lat member model z"))
      ncdf4::ncvar_change_missval(ncout, nc_vars[[i]], missval = fillvalue)
    }
  }

  message("Finished writing NetCDF file [", Sys.time(), "]")

  invisible(fname)
}

#' Backward-compatible alias for create_netcdf_output
#'
#' @inheritParams create_netcdf_output
#' @return Invisibly returns the output NetCDF file path.
#' @export
create_netcdf_wq <- function(output_lists,
                             folder = ".",
                             model,
                             out_time = NULL,
                             longitude = NULL,
                             latitude = NULL,
                             ler_config_file = NULL,
                             wq_config_file = NULL,
                             compression = 4,
                             members = 25,
                             out_file = "ensemble_output.nc") {
  create_netcdf_output(
    output_lists = output_lists,
    folder = folder,
    model = model,
    out_time = out_time,
    longitude = longitude,
    latitude = latitude,
    ler_config_file = ler_config_file,
    wq_config_file = wq_config_file,
    compression = compression,
    members = members,
    out_file = out_file
  )
}
