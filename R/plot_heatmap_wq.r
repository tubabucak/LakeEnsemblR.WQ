#' Plot heat map from LakeEnsemblR.WQ NetCDF output
#'
#' Plot a heat map for a metric stored in a NetCDF created by
#' create_netcdf_output(). Unlike LakeEnsemblR::plot_heatmap(), this function
#' uses the metric names as written in the file (e.g., Temp_degreeCelcius).
#'
#' @param ncdf character; path to NetCDF file.
#' @param metric character; metric variable name in the NetCDF.
#' @param models character vector; optional subset/order of model names.
#'   If NULL, all available models except Obs are used.
#' @param member integer; ensemble member index to plot. Defaults to 1.
#' @param spin_up numeric; number of days to omit from start of series.
#' @param tile_width numeric; optional tile width in seconds.
#' @param tile_height numeric; optional tile height in depth units.
#'
#' @return ggplot object.
#' @export
plot_heatmap_wq <- function(ncdf,
                            metric,
                            models = NULL,
                            member = 1,
                            spin_up = 0,
                            tile_width = NULL,
                            tile_height = NULL) {
  if (!file.exists(ncdf)) {
    stop("File does not exist: ", ncdf)
  }
  if (!is.character(metric) || length(metric) != 1 || !nzchar(metric)) {
    stop("metric must be a single non-empty character string.")
  }

  nc <- ncdf4::nc_open(ncdf)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  if (!metric %in% names(nc$var)) {
    stop(
      "Metric variable not found in NetCDF: ", metric,
      ". Available variables: ", paste(names(nc$var), collapse = ", ")
    )
  }

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
    if (!is.null(nc$dim$model) && !is.null(nc$dim$model$len)) {
      model_names <- as.character(seq_len(nc$dim$model$len))
    } else {
      model_names <- "1"
    }
  }

  if (is.null(models)) {
    models <- model_names[model_names != "Obs"]
  } else {
    models <- as.character(models)
    models <- models[models %in% model_names]
    if (length(models) == 0) {
      stop("None of requested models are present in NetCDF model dimension.")
    }
  }

  vals <- ncdf4::ncvar_get(nc, metric, collapse_degen = FALSE)
  dim_names <- vapply(nc$var[[metric]]$dim, function(d) d$name, character(1))
  dim_sizes <- dim(vals)

  if (length(dim_names) != length(dim_sizes)) {
    stop(
      "Could not align NetCDF dimensions for metric: ", metric,
      ". Dimensions found: ", paste(dim_names, collapse = ", ")
    )
  }

  idx_member <- which(dim_names == "member")
  if (length(idx_member) != 1) {
    stop("Expected a 'member' dimension for metric: ", metric)
  }
  if (member < 1 || member > dim_sizes[idx_member]) {
    stop("member index out of range: ", member)
  }

  has_depth <- "z" %in% dim_names
  if (has_depth) {
    z_vals <- ncdf4::ncvar_get(nc, "z")
  } else {
    z_vals <- 0
  }

  idx_model <- which(dim_names == "model")
  idx_time <- which(dim_names == "time")
  idx_lon <- which(dim_names == "lon")
  idx_lat <- which(dim_names == "lat")
  idx_z <- which(dim_names == "z")

  long_data <- list()

  for (m in models) {
    model_index <- which(model_names == m)
    if (length(model_index) == 0) {
      next
    }

    idx_list <- lapply(dim_sizes, function(n) seq_len(n))
    idx_list[[idx_model]] <- model_index
    idx_list[[idx_member]] <- member
    if (length(idx_lon) == 1) idx_list[[idx_lon]] <- 1
    if (length(idx_lat) == 1) idx_list[[idx_lat]] <- 1

    arr <- do.call(`[`, c(list(vals), idx_list, list(drop = TRUE)))

    if (has_depth) {
      if (length(dim(arr)) != 2) {
        arr <- matrix(arr, nrow = length(datetime), ncol = length(z_vals))
      }

      # Ensure orientation is time x depth
      if (nrow(arr) == length(z_vals) && ncol(arr) == length(datetime)) {
        arr <- t(arr)
      } else if (!(nrow(arr) == length(datetime) && ncol(arr) == length(z_vals))) {
        arr <- matrix(as.numeric(arr), nrow = length(datetime), ncol = length(z_vals), byrow = FALSE)
      }

      df <- data.frame(
        datetime = rep(datetime, times = length(z_vals)),
        depth_value = rep(z_vals, each = length(datetime)),
        value = as.numeric(arr),
        Model = m,
        stringsAsFactors = FALSE
      )
    } else {
      vec <- as.numeric(arr)
      if (length(vec) != length(datetime)) {
        vec <- rep(NA_real_, length(datetime))
      }

      df <- data.frame(
        datetime = datetime,
        depth_value = 0,
        value = vec,
        Model = m,
        stringsAsFactors = FALSE
      )
    }

    long_data[[length(long_data) + 1]] <- df
  }

  if (length(long_data) == 0) {
    stop("No plottable model data found for metric: ", metric)
  }

  data <- dplyr::bind_rows(long_data)
  data$Model <- factor(data$Model, levels = models)

  if (spin_up > 0) {
    start_date <- min(data$datetime, na.rm = TRUE) + lubridate::days(spin_up)
    data <- data[data$datetime >= start_date, , drop = FALSE]
  }

  if (is.null(tile_width)) {
    u_times <- sort(unique(data$datetime))
    if (length(u_times) >= 2) {
      tile_width <- as.numeric(difftime(u_times[2], u_times[1], units = "secs"))
    } else {
      tile_width <- 86400
    }
  }

  if (is.null(tile_height)) {
    u_depths <- sort(unique(data$depth_value))
    if (length(u_depths) >= 2) {
      tile_height <- abs(min(diff(u_depths)))
    } else {
      tile_height <- 1
    }
  }

  data <- data[!is.na(data$value), , drop = FALSE]
  if (nrow(data) == 0) {
    stop("All values are NA for selected metric/models.")
  }

  ggplot2::ggplot(data, ggplot2::aes(datetime, depth_value, fill = value)) +
    ggplot2::geom_tile(width = tile_width, height = tile_height) +
    ggplot2::scale_fill_gradientn(colours = rev(grDevices::hcl.colors(11, "Spectral"))) +
    ggplot2::facet_wrap(~Model, ncol = 2) +
    ggplot2::labs(
      title = paste("Heatmap:", metric),
      x = "Date",
      y = if (has_depth) "Depth (m)" else "Layer"
    ) +
    ggplot2::theme_minimal()
}
