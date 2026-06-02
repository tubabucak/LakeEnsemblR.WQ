#' Compare one metric across models
#'
#' Plot a selected metric across models directly from cal_metrics output.
#'
#' @param metric_out list; output from cal_metrics().
#' @param metric character; metric name to compare (e.g., "Temp_degreeCelcius").
#' @param models character vector; optional subset/order of models to plot.
#'   If NULL, all available models for the metric are used.
#' @param depth numeric or character; optional depth selector for depth-resolved
#'   metrics. If NULL, first non-datetime value column is used.
#' @param metric_instance character; optional metric instance name when multiple
#'   instances exist for the same metric/model.
#'
#' @return A list with:
#' \itemize{
#'   \item plot: ggplot object.
#'   \item data: long-format data used for plotting.
#'   \item used_columns: named character vector of selected value columns per model.
#' }
#' @export
compare_models_metric <- function(metric_out,
                                  metric,
                                  models = NULL,
                                  depth = NULL,
                                  metric_instance = NULL) {

  if (!is.list(metric_out) || length(metric_out) == 0) {
    stop("metric_out must be a non-empty list from cal_metrics().")
  }
  if (!is.character(metric) || length(metric) != 1 || !nzchar(metric)) {
    stop("metric must be a single non-empty character string.")
  }
  if (!metric %in% names(metric_out)) {
    stop("Metric not found in metric_out: ", metric)
  }

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  .pick_df <- function(model_entry, metric_instance = NULL) {
    if (is.data.frame(model_entry)) {
      return(model_entry)
    }

    if (is.list(model_entry) && length(model_entry) > 0) {
      if (!is.null(metric_instance) && metric_instance %in% names(model_entry) &&
          is.data.frame(model_entry[[metric_instance]])) {
        return(model_entry[[metric_instance]])
      }

      for (i in seq_len(length(model_entry))) {
        if (is.data.frame(model_entry[[i]])) {
          return(model_entry[[i]])
        }
        if (is.list(model_entry[[i]])) {
          nested_df <- .pick_df(model_entry[[i]], metric_instance = metric_instance)
          if (!is.null(nested_df)) {
            return(nested_df)
          }
        }
      }
    }

    NULL
  }

  .find_datetime_col <- function(df) {
    nm <- tolower(names(df))
    idx <- which(nm %in% c("datetime", "date", "time", "timestamp"))
    if (length(idx) > 0) return(idx[1])

    cls_idx <- which(vapply(df, function(col) {
      inherits(col, "POSIXt") || inherits(col, "Date")
    }, logical(1)))
    if (length(cls_idx) > 0) return(cls_idx[1])

    # Heuristic fallback: first column parseable as datetime for most rows
    first_col <- suppressWarnings(as.POSIXct(df[[1]], tz = "UTC"))
    if (sum(!is.na(first_col)) >= max(2, floor(0.5 * length(first_col)))) {
      return(1)
    }

    integer(0)
  }

  .select_value_col <- function(df, depth = NULL) {
    cols <- names(df)
    value_cols <- setdiff(cols, "datetime")

    if (length(value_cols) == 0) {
      return(NA_character_)
    }

    if (is.null(depth)) {
      return(value_cols[1])
    }

    target <- suppressWarnings(as.numeric(depth))
    depth_cols <- value_cols[grepl("^Depth_", value_cols)]
    if (length(depth_cols) == 0) {
      return(value_cols[1])
    }

    dep_num <- suppressWarnings(as.numeric(gsub("^Depth_", "", depth_cols)))
    if (all(is.na(dep_num)) || is.na(target)) {
      wanted <- paste0("Depth_", depth)
      if (wanted %in% depth_cols) return(wanted)
      return(depth_cols[1])
    }

    depth_cols[which.min(abs(dep_num - target))]
  }

  metric_block <- metric_out[[metric]]
  if (!is.list(metric_block) || length(metric_block) == 0) {
    stop("Selected metric has no model data: ", metric)
  }

  available_models <- names(metric_block)
  if (is.null(models)) {
    models <- available_models
  } else {
    models <- as.character(models)
    models <- models[models %in% available_models]
    if (length(models) == 0) {
      stop("None of the requested models are available for metric: ", metric)
    }
  }

  long_data <- list()
  used_columns <- character(0)

  for (m in models) {
    df_raw <- .pick_df(metric_block[[m]], metric_instance = metric_instance)
    if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) {
      next
    }

    dt_col <- .find_datetime_col(df_raw)
    if (length(dt_col) == 0) {
      next
    }

    df <- df_raw[, c(dt_col, setdiff(seq_len(ncol(df_raw)), dt_col)), drop = FALSE]
    names(df)[1] <- "datetime"

    val_col <- .select_value_col(df, depth = depth)
    if (is.na(val_col) || !val_col %in% names(df)) {
      next
    }

    plot_df <- data.frame(
      datetime = as.POSIXct(df$datetime, tz = "UTC"),
      value = suppressWarnings(as.numeric(df[[val_col]])),
      model = m,
      stringsAsFactors = FALSE
    )

    long_data[[length(long_data) + 1]] <- plot_df
    used_columns[m] <- val_col
  }

  if (length(long_data) == 0) {
    stop("No plottable model data found for metric: ", metric)
  }

  data_all <- dplyr::bind_rows(long_data)

  p <- ggplot2::ggplot(data_all, ggplot2::aes(x = datetime, y = value, color = model)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(
      title = paste("Model comparison:", metric),
      x = "Date",
      y = if (is.null(depth)) metric else paste0(metric, " @ depth ", depth)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  list(plot = p, data = data_all, used_columns = used_columns)
}

#' Compare one metric across models from NetCDF output
#'
#' Plot a selected metric across models directly from a NetCDF file created by
#' create_netcdf_output().
#'
#' @param nc_file character; path to NetCDF file.
#' @param metric character; variable name in NetCDF to compare.
#' @param models character vector; optional subset/order of models. If NULL, all
#'   models in the NetCDF model attribute are used except Obs.
#' @param depth numeric; optional depth selector for 3D variables. For 2D
#'   variables this is ignored.
#' @param member integer; member index to plot (default 1).
#'
#' @return A list with:
#' \itemize{
#'   \item plot: ggplot object.
#'   \item data: long-format data used for plotting.
#'   \item dimension_info: list with variable dimensions and selected depth.
#' }
#' @export
compare_models_metric_netcdf <- function(nc_file,
                                         metric,
                                         models = NULL,
                                         depth = NULL,
                                         member = 1) {
  if (!file.exists(nc_file)) {
    stop("NetCDF file not found: ", nc_file)
  }
  if (!is.character(metric) || length(metric) != 1 || !nzchar(metric)) {
    stop("metric must be a single non-empty character string.")
  }

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  if (!metric %in% names(nc$var)) {
    stop("Metric variable not found in NetCDF: ", metric)
  }

  `%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

  time_vals <- ncdf4::ncvar_get(nc, "time")
  datetime <- as.POSIXct(time_vals, origin = "1970-01-01", tz = "UTC")

  model_att <- tryCatch(
    ncdf4::ncatt_get(nc, "model", "Model")$value,
    error = function(e) NULL
  )

  model_names <- NULL
  if (!is.null(model_att) && nzchar(model_att)) {
    parts <- strsplit(model_att, ",")[[1]]
    model_names <- trimws(vapply(parts, function(x) {
      sub("^[0-9]+\\s*-\\s*", "", trimws(x))
    }, character(1)))
  }

  if (is.null(model_names) || length(model_names) == 0) {
    model_names <- as.character(seq_len(nc$dim$model$len))
  }

  if (is.null(models)) {
    models <- model_names[model_names != "Obs"]
  } else {
    models <- as.character(models)
    models <- models[models %in% model_names]
    if (length(models) == 0) {
      stop("None of requested models were found in NetCDF model dimension.")
    }
  }

  var_obj <- nc$var[[metric]]
  dim_names <- vapply(var_obj$dim, function(d) d$name, character(1))
  vals <- ncdf4::ncvar_get(nc, metric, collapse_degen = FALSE)

  idx_member <- which(dim_names == "member")
  if (length(idx_member) == 1) {
    if (member < 1 || member > dim(vals)[idx_member]) {
      stop("member is out of bounds for NetCDF variable: ", metric)
    }
  }

  long_data <- list()
  z_vals <- NULL
  selected_depth <- NA_real_

  if ("z" %in% dim_names) {
    z_vals <- ncdf4::ncvar_get(nc, "z")
    if (is.null(depth)) {
      z_idx <- 1
      selected_depth <- z_vals[z_idx]
    } else {
      target <- as.numeric(depth)
      z_idx <- which.min(abs(z_vals - target))
      selected_depth <- z_vals[z_idx]
    }
  }

  for (m in models) {
    model_idx <- which(model_names == m)
    if (length(model_idx) == 0) next

    # Expected dimension order from create_netcdf_output:
    # lon, lat, member, model, time [, z]
    if ("z" %in% dim_names) {
      series <- vals[1, 1, member, model_idx, , z_idx]
    } else {
      series <- vals[1, 1, member, model_idx, ]
    }

    long_data[[length(long_data) + 1]] <- data.frame(
      datetime = datetime,
      value = as.numeric(series),
      model = m,
      stringsAsFactors = FALSE
    )
  }

  if (length(long_data) == 0) {
    stop("No plottable model data found in NetCDF for metric: ", metric)
  }

  data_all <- dplyr::bind_rows(long_data)

  y_label <- metric
  if (!is.na(selected_depth)) {
    y_label <- paste0(metric, " @ z=", round(selected_depth, 3), " m")
  }

  p <- ggplot2::ggplot(data_all, ggplot2::aes(x = datetime, y = value, color = model)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(
      title = paste("Model comparison from NetCDF:", metric),
      x = "Date",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  list(
    plot = p,
    data = data_all,
    dimension_info = list(
      dim_names = dim_names,
      has_depth = "z" %in% dim_names,
      selected_depth = selected_depth
    )
  )
}
