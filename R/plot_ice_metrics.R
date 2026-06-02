#' Plot and compare ice metrics across models
#'
#' Supports two inputs:
#' 1) nested cal_metrics output containing a cal_ice_duration result
#'    (ice_duration_period and ice_thickness), or
#' 2) a NetCDF output file path.
#'
#' @param metrics_list list or character; full cal_metrics output, a metric
#'   sub-list, or NetCDF file path (.nc).
#' @param metric_name character; metric name to extract from list input.
#'   If not found, the function tries to auto-detect a block containing
#'   ice_duration_period and ice_thickness.
#' @param member integer; ensemble member index used in NetCDF mode.
#' @param years integer vector or NULL; optional year subset.
#' @param colors named character vector or NULL; optional model colors.
#' @param free_y logical; if TRUE, yearly and daily plots use free y by model.
#'
#' @return Named list with duration_plot, thickness_plot, duration_data,
#'   thickness_data.
#' @export
plot_ice_metrics <- function(metrics_list,
                             metric_name = "Ice_Duration_Days",
                             member = 1,
                             years = NULL,
                             colors = NULL,
                             free_y = FALSE) {

  .parse_model_names_from_nc <- function(nc) {
    model_att <- tryCatch(
      ncdf4::ncatt_get(nc, "model", "Model")$value,
      error = function(e) NULL
    )

    if (!is.null(model_att) && nzchar(model_att)) {
      parts <- strsplit(model_att, ",")[[1]]
      return(trimws(vapply(parts, function(x) {
        sub("^[0-9]+\\s*-\\s*", "", trimws(x))
      }, character(1))))
    }

    if (!is.null(nc$dim$model) && !is.null(nc$dim$model$len)) {
      return(as.character(seq_len(nc$dim$model$len)))
    }

    "1"
  }

  .find_nc_var <- function(var_names, candidates, fallback = NULL) {
    if (!is.null(fallback) && nzchar(fallback) && fallback %in% var_names) {
      return(fallback)
    }

    low <- tolower(var_names)
    for (cand in candidates) {
      hit <- which(low == tolower(cand))
      if (length(hit) > 0) return(var_names[hit[1]])
    }

    for (cand in candidates) {
      hit <- grep(cand, var_names, ignore.case = TRUE)
      if (length(hit) > 0) return(var_names[hit[1]])
    }

    NULL
  }

  .extract_nc_model_time <- function(nc, var_name, datetime, model_names, member = 1L) {
    if (is.null(var_name) || !var_name %in% names(nc$var)) {
      return(data.frame())
    }

    vals <- ncdf4::ncvar_get(nc, var_name, collapse_degen = FALSE)
    dim_names <- vapply(nc$var[[var_name]]$dim, function(d) d$name, character(1))
    dim_sizes <- dim(vals)

    idx_model <- which(dim_names == "model")
    idx_time <- which(dim_names == "time")
    idx_member <- which(dim_names == "member")
    idx_lon <- which(dim_names == "lon")
    idx_lat <- which(dim_names == "lat")

    if (length(idx_time) != 1) {
      return(data.frame())
    }

    if (length(idx_model) == 0) {
      model_names <- "Model"
      idx_model <- NA_integer_
    }

    out <- list()
    for (mi in seq_along(model_names)) {
      idx_list <- lapply(dim_sizes, function(n) seq_len(n))
      if (!is.na(idx_model)) idx_list[[idx_model]] <- mi
      if (length(idx_member) == 1) {
        if (member < 1 || member > dim_sizes[idx_member]) {
          stop("member index out of range for variable: ", var_name)
        }
        idx_list[[idx_member]] <- member
      }
      if (length(idx_lon) == 1) idx_list[[idx_lon]] <- 1
      if (length(idx_lat) == 1) idx_list[[idx_lat]] <- 1

      arr <- do.call("[", c(list(vals), idx_list, list(drop = TRUE)))
      vec <- as.numeric(arr)

      if (length(vec) != length(datetime)) {
        if (is.matrix(arr) && nrow(arr) == length(datetime)) {
          vec <- as.numeric(arr[, 1])
        } else if (is.matrix(arr) && ncol(arr) == length(datetime)) {
          vec <- as.numeric(arr[1, ])
        } else {
          next
        }
      }

      out[[length(out) + 1]] <- data.frame(
        datetime = datetime,
        ice_thickness = vec,
        Model = model_names[mi],
        stringsAsFactors = FALSE
      )
    }

    dplyr::bind_rows(out)
  }

  .find_ice_struct <- function(x, max_depth = 6L) {
    if (max_depth < 0L || is.null(x)) return(NULL)

    if (is.list(x) && all(c("ice_duration_period", "ice_thickness") %in% names(x))) {
      return(x)
    }

    if (!is.list(x) || length(x) == 0) return(NULL)

    for (i in seq_along(x)) {
      found <- .find_ice_struct(x[[i]], max_depth = max_depth - 1L)
      if (!is.null(found)) return(found)
    }

    NULL
  }

  .standardize_duration <- function(df) {
    if (!is.data.frame(df) || ncol(df) < 2) return(NULL)
    data.frame(
      Year = as.integer(df[[1]]),
      ice_duration_days = as.numeric(df[[2]]),
      stringsAsFactors = FALSE
    )
  }

  .standardize_thickness <- function(df) {
    if (!is.data.frame(df) || ncol(df) < 2) return(NULL)
    out <- data.frame(
      datetime = as.POSIXct(df[[1]], tz = "UTC"),
      ice_thickness = as.numeric(df[[2]]),
      stringsAsFactors = FALSE
    )
    out$Year <- as.integer(format(as.Date(out$datetime), "%Y"))
    out
  }

  duration_data <- data.frame()
  thickness_data <- data.frame()

  if (is.character(metrics_list) && length(metrics_list) == 1L &&
      file.exists(metrics_list) && grepl("\\.nc$", metrics_list, ignore.case = TRUE)) {

    nc <- ncdf4::nc_open(metrics_list)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    time_vals <- ncdf4::ncvar_get(nc, "time")
    datetime <- as.POSIXct(time_vals, origin = "1970-01-01", tz = "UTC")
    model_names <- .parse_model_names_from_nc(nc)
    var_names <- names(nc$var)

    thickness_var <- .find_nc_var(
      var_names,
      candidates = c("Ice_Thickness_meter", "ice_thickness", "ice.*thickness")
    )

    if (is.null(thickness_var)) {
      stop("No ice thickness variable found in NetCDF. Available variables: ", paste(var_names, collapse = ", "))
    }

    thickness_data <- .extract_nc_model_time(nc, thickness_var, datetime, model_names, member = as.integer(member))
    if (nrow(thickness_data) == 0) {
      stop("Could not extract ice thickness from NetCDF variable: ", thickness_var)
    }

    thickness_data$Year <- as.integer(format(as.Date(thickness_data$datetime), "%Y"))

    duration_data <- thickness_data %>%
      dplyr::group_by(Model, Year) %>%
      dplyr::summarise(
        ice_duration_days = sum(ice_thickness > 0, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    ice_block <- NULL

    if (!is.null(names(metrics_list)) && metric_name %in% names(metrics_list)) {
      ice_block <- metrics_list[[metric_name]]
    } else {
      ice_block <- metrics_list
    }

    if (!is.list(ice_block) || length(ice_block) == 0) {
      stop("Could not locate an ice metric block in supplied metrics_list.")
    }

    model_names <- names(ice_block)
    if (is.null(model_names) || length(model_names) == 0) {
      stop("No model entries found in ice metric block.")
    }

    duration_data <- dplyr::bind_rows(lapply(model_names, function(m) {
      s <- .find_ice_struct(ice_block[[m]])
      if (is.null(s)) return(NULL)
      d <- .standardize_duration(s$ice_duration_period)
      if (is.null(d)) return(NULL)
      d$Model <- m
      d
    }))

    thickness_data <- dplyr::bind_rows(lapply(model_names, function(m) {
      s <- .find_ice_struct(ice_block[[m]])
      if (is.null(s)) return(NULL)
      d <- .standardize_thickness(s$ice_thickness)
      if (is.null(d)) return(NULL)
      d$Model <- m
      d
    }))

    if (nrow(duration_data) == 0 && nrow(thickness_data) > 0) {
      duration_data <- thickness_data %>%
        dplyr::group_by(Model, Year) %>%
        dplyr::summarise(
          ice_duration_days = sum(ice_thickness > 0, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }

  if (nrow(duration_data) == 0 && nrow(thickness_data) == 0) {
    stop("No valid ice data could be extracted for plotting.")
  }

  if (!is.null(years)) {
    if (nrow(duration_data) > 0) {
      duration_data <- duration_data[duration_data$Year %in% years, , drop = FALSE]
    }
    if (nrow(thickness_data) > 0) {
      thickness_data <- thickness_data[thickness_data$Year %in% years, , drop = FALSE]
    }
  }

  present_models <- unique(c(
    if (nrow(duration_data) > 0) duration_data$Model else character(0),
    if (nrow(thickness_data) > 0) thickness_data$Model else character(0)
  ))

  default_palette <- c(
    GLM = "#E41A1C",
    WET = "#377EB8",
    SELMAPROTBAS = "#4DAF4A",
    SIMSTRAT = "#984EA3"
  )

  if (is.null(colors)) {
    fallback <- grDevices::rainbow(length(present_models))
    names(fallback) <- present_models
    colors <- ifelse(present_models %in% names(default_palette),
                     default_palette[present_models],
                     fallback[present_models])
    names(colors) <- present_models
  }

  .year_breaks <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    seq.int(from = floor(min(x)), to = ceiling(max(x)), by = 1)
  }
  .year_labels <- function(x) as.character(as.integer(round(x)))

  ybreaks <- .year_breaks(if (nrow(duration_data) > 0) duration_data$Year else numeric(0))

  p_duration <- ggplot2::ggplot(
    duration_data,
    ggplot2::aes(x = Year, y = ice_duration_days, colour = Model, group = Model)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = ybreaks, labels = .year_labels) +
    ggplot2::labs(
      title = "Ice Duration Period",
      x = "Year",
      y = "Ice Days",
      colour = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_thickness <- ggplot2::ggplot(
    thickness_data,
    ggplot2::aes(x = datetime, y = ice_thickness, colour = Model, group = Model)
  ) +
    ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = "Ice Thickness",
      x = "Date",
      y = "Ice Thickness",
      colour = "Model"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (isTRUE(free_y)) {
    p_duration <- p_duration + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
    p_thickness <- p_thickness + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
  }

  list(
    duration_plot = p_duration,
    thickness_plot = p_thickness,
    duration_data = duration_data,
    thickness_data = thickness_data
  )
}
