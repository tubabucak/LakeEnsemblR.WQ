#' Plot and compare stratification metrics across models
#'
#' Takes the \code{Duration_of_Stratification} (or any metric with columns
#' \code{Year}, \code{Strat_Start_Date}, \code{Consecutive_Strat_Days}, and
#' \code{Mixing_Start_Date}) from the \code{cal_metrics()} output list and
#' produces a multi-panel comparison across all available models.
#'
#' @param metrics_list list or character; the full output of \code{cal_metrics()},
#'   the sub-list for a single metric, or a path to a NetCDF file.
#'   When a NetCDF path is provided, stratification variables are read directly
#'   from file.
#' @param member integer; ensemble member index to read in NetCDF mode.
#'   Defaults to \code{1}.
#' @param metric_name character; name of the stratification metric to extract
#'   from \code{metrics_list} when list input is used.
#'   In NetCDF mode this is used as preferred duration variable name.
#'   Defaults to \code{"Duration_of_Stratification"}.
#' @param obs data.frame or \code{NULL}; optional observed reference data in the
#'   same format (columns \code{Year} and \code{Consecutive_Strat_Days}). When
#'   supplied it is overlaid as black points.
#' @param colors named character vector or \code{NULL}; custom colours for
#'   models. Names must match the model names in the metric list (e.g.
#'   \code{c(GLM = "#E41A1C", WET = "#377EB8")}). When \code{NULL}, a default
#'   palette is used.
#' @param years integer vector or \code{NULL}; subset of years to include in
#'   the plots (e.g. \code{1990:2000} or \code{c(1992, 1995, 2000)}). When
#'   \code{NULL} (default) all available years are shown.
#' @param free_y logical; if \code{TRUE} each panel uses a free y-axis scale.
#'   Defaults to \code{FALSE} (shared axis for direct comparison).
#'
#' @return A named list with elements:
#' \describe{
#'   \item{\code{duration_plot}}{ggplot — yearly stratification duration (days) per model.}
#'   \item{\code{onset_plot}}{ggplot — day-of-year of stratification onset per model.}
#'   \item{\code{mixing_plot}}{ggplot — day-of-year of mixing onset per model.}
#'   \item{\code{combined_data}}{data.frame — the tidy data used for plotting.}
#' }
#'
#' @examples
#' \dontrun{
#' metrics <- cal_metrics(metric_yaml_file = "Output.yaml",
#'                        wq_config_file   = "LakeEnsemblR_WQ.yaml")
#'
#' plots <- plot_strat_metrics(metrics)
#' plots$duration_plot
#' plots$onset_plot
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon
#' @importFrom ggplot2  scale_color_manual labs theme_bw theme element_text facet_wrap
#' @importFrom ggplot2  scale_x_continuous scale_y_continuous
#' @importFrom dplyr bind_rows mutate
#' @export

plot_strat_metrics <- function(metrics_list,
                               member      = 1,
                               metric_name = "Duration_of_Stratification",
                               obs         = NULL,
                               years       = NULL,
                               colors      = NULL,
                               free_y      = FALSE) {

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

      arr <- do.call(`[`, c(list(vals), idx_list, list(drop = TRUE)))
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
        Value = vec,
        Model = model_names[mi],
        stringsAsFactors = FALSE
      )
    }

    dplyr::bind_rows(out)
  }

  if (is.character(metrics_list) && length(metrics_list) == 1L &&
      file.exists(metrics_list) && grepl("\\.nc$", metrics_list, ignore.case = TRUE)) {
    nc <- ncdf4::nc_open(metrics_list)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    if (!"time" %in% names(nc$dim) && !"time" %in% names(nc$var)) {
      stop("NetCDF does not contain a 'time' axis.")
    }

    time_vals <- ncdf4::ncvar_get(nc, "time")
    datetime <- as.POSIXct(time_vals, origin = "1970-01-01", tz = "UTC")
    model_names <- .parse_model_names_from_nc(nc)
    var_names <- names(nc$var)

    duration_var <- .find_nc_var(
      var_names,
      candidates = c("Duration_of_Stratification", "Consecutive_Strat_Days", "strat.*duration"),
      fallback = metric_name
    )
    onset_var <- .find_nc_var(
      var_names,
      candidates = c("Stratification_Onset_DOY", "Strat_DOY", "strat.*onset|strat.*start")
    )
    mixing_var <- .find_nc_var(
      var_names,
      candidates = c("Mixing_Onset_DOY", "mixing.*onset|mixing.*start")
    )

    dur_data <- .extract_nc_model_time(nc, duration_var, datetime, model_names, member = as.integer(member))
    onset_data <- .extract_nc_model_time(nc, onset_var, datetime, model_names, member = as.integer(member))
    mix_data <- .extract_nc_model_time(nc, mixing_var, datetime, model_names, member = as.integer(member))

    if (nrow(dur_data) == 0 && nrow(onset_data) == 0 && nrow(mix_data) == 0) {
      stop("No stratification variables found in NetCDF. Available variables: ",
           paste(var_names, collapse = ", "))
    }

    if (nrow(dur_data) > 0) {
      names(dur_data)[names(dur_data) == "Value"] <- "Consecutive_Strat_Days"
    }
    if (nrow(onset_data) > 0) {
      names(onset_data)[names(onset_data) == "Value"] <- "Strat_DOY"
    }
    if (nrow(mix_data) > 0) {
      names(mix_data)[names(mix_data) == "Value"] <- "Mixing_DOY"
    }

    all_data <- Reduce(function(a, b) {
      if (nrow(a) == 0) return(b)
      if (nrow(b) == 0) return(a)
      merge(a, b, by = c("datetime", "Model"), all = TRUE)
    }, list(dur_data, onset_data, mix_data), init = data.frame())

    if (nrow(all_data) == 0) {
      stop("No plottable stratification data found in NetCDF.")
    }

    all_data$Year <- as.integer(format(as.Date(all_data$datetime), "%Y"))

    all_data <- all_data %>%
      dplyr::group_by(Model, Year) %>%
      dplyr::summarise(
        Consecutive_Strat_Days = mean(Consecutive_Strat_Days, na.rm = TRUE),
        Strat_DOY = mean(Strat_DOY, na.rm = TRUE),
        Mixing_DOY = mean(Mixing_DOY, na.rm = TRUE),
        .groups = "drop"
      )

    if (!is.null(years)) {
      all_data <- all_data[all_data$Year %in% years, , drop = FALSE]
      if (!is.null(obs) && "Year" %in% names(obs)) {
        obs <- obs[obs$Year %in% years, , drop = FALSE]
      }
    }

    if (nrow(all_data) == 0) {
      stop("No data remaining after filtering requested years.")
    }

    all_data$Strat_Start_Date <- as.Date(NA)
    all_data$Mixing_Start_Date <- as.Date(NA)

    default_palette <- c(
      GLM           = "#E41A1C",
      WET           = "#377EB8",
      SELMAPROTBAS  = "#4DAF4A",
      SIMSTRAT      = "#984EA3"
    )

    if (is.null(colors)) {
      present <- unique(all_data$Model)
      fallback <- grDevices::rainbow(length(present))
      names(fallback) <- present
      colors <- ifelse(present %in% names(default_palette),
                       default_palette[present],
                       fallback[present])
      names(colors) <- present
    }

    p_dur <- ggplot2::ggplot(
      all_data,
      ggplot2::aes(x = Year, y = Consecutive_Strat_Days,
                   colour = Model, group = Model)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_x_continuous(breaks = seq.int(min(all_data$Year), max(all_data$Year), by = 1)) +
      ggplot2::labs(
        title  = "Duration of Stratification",
        x      = "Year",
        y      = "Consecutive Stratified Days",
        colour = "Model"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    p_onset <- ggplot2::ggplot(
      all_data,
      ggplot2::aes(x = Year, y = Strat_DOY,
                   colour = Model, group = Model)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_x_continuous(breaks = seq.int(min(all_data$Year), max(all_data$Year), by = 1)) +
      ggplot2::labs(
        title  = "Stratification Onset (Day of Year)",
        x      = "Year",
        y      = "Onset (DOY)",
        colour = "Model"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    p_mixing <- ggplot2::ggplot(
      all_data,
      ggplot2::aes(x = Year, y = Mixing_DOY,
                   colour = Model, group = Model)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_x_continuous(breaks = seq.int(min(all_data$Year), max(all_data$Year), by = 1)) +
      ggplot2::labs(
        title  = "Mixing Onset (Day of Year)",
        x      = "Year",
        y      = "Mixing Onset (DOY)",
        colour = "Model"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (isTRUE(free_y)) {
      p_dur <- p_dur + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
      p_onset <- p_onset + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
      p_mixing <- p_mixing + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
    }

    return(list(
      duration_plot  = p_dur,
      onset_plot     = p_onset,
      mixing_plot    = p_mixing,
      combined_data  = all_data
    ))
  }

  # ---- resolve the per-model sub-list --------------------------------------
  # Accept either the full cal_metrics() output or the sub-list for one metric
  if (metric_name %in% names(metrics_list)) {
    strat_list <- metrics_list[[metric_name]]
  } else {
    # Assume the user passed the sub-list directly
    strat_list <- metrics_list
  }

  # ---- unwrap metric-instance nesting --------------------------------------
  # Each model entry is stored as list(metric_instance = data.frame).
  # Flatten to a single data.frame per model.
  .unwrap <- function(x) {
    if (is.data.frame(x)) return(x)
    if (is.list(x) && length(x) > 0 && is.data.frame(x[[1]])) return(x[[1]])
    NULL
  }

  model_names <- names(strat_list)
  if (length(model_names) == 0) {
    stop("No model entries found in the supplied metric list.")
  }

  all_data <- dplyr::bind_rows(
    lapply(model_names, function(m) {
      df <- .unwrap(strat_list[[m]])
      if (is.null(df)) {
        warning("Could not extract data for model: ", m)
        return(NULL)
      }
      df$Model <- m
      df
    })
  )

  required_cols <- c("Year", "Consecutive_Strat_Days",
                     "Strat_Start_Date", "Mixing_Start_Date")
  missing_cols <- setdiff(required_cols, names(all_data))
  if (length(missing_cols) > 0) {
    stop("Missing expected columns in stratification data: ",
         paste(missing_cols, collapse = ", "))
  }

  # ---- filter years --------------------------------------------------------
  if (!is.null(years)) {
    all_data <- all_data[all_data$Year %in% years, , drop = FALSE]
    if (!is.null(obs) && "Year" %in% names(obs)) {
      obs <- obs[obs$Year %in% years, , drop = FALSE]
    }
    if (nrow(all_data) == 0) {
      stop("No data remaining after filtering for years: ",
           paste(years, collapse = ", "))
    }
  }

  # ---- add day-of-year columns ---------------------------------------------
  all_data <- all_data %>%
    dplyr::mutate(
      Strat_DOY  = as.integer(format(as.Date(Strat_Start_Date),  "%j")),
      Mixing_DOY = as.integer(format(as.Date(Mixing_Start_Date), "%j"))
    )

  # ---- colours -------------------------------------------------------------
  default_palette <- c(
    GLM           = "#E41A1C",
    WET           = "#377EB8",
    SELMAPROTBAS  = "#4DAF4A",
    SIMSTRAT      = "#984EA3"
  )

  if (is.null(colors)) {
    present <- unique(all_data$Model)
    # Use defaults where available, otherwise recycle RColorBrewer-style
    fallback <- grDevices::rainbow(length(present))
    names(fallback) <- present
    colors <- ifelse(present %in% names(default_palette),
                     default_palette[present],
                     fallback[present])
    names(colors) <- present
  }

  scale_y <- if (free_y) "free_y" else "fixed"

  # ---- helper for obs overlay ----------------------------------------------
  .add_obs_duration <- function(p) {
    if (!is.null(obs) && "Year" %in% names(obs) &&
        "Consecutive_Strat_Days" %in% names(obs)) {
      p <- p + ggplot2::geom_point(
        data   = obs,
        ggplot2::aes(x = Year, y = Consecutive_Strat_Days),
        colour = "black", shape = 17, size = 2.5,
        inherit.aes = FALSE
      )
    }
    p
  }

  # ---- plot 1: stratification duration -------------------------------------
  p_dur <- ggplot2::ggplot(
    all_data,
    ggplot2::aes(x = Year, y = Consecutive_Strat_Days,
                 colour = Model, group = Model)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::labs(
      title  = "Duration of Stratification",
      x      = "Year",
      y      = "Consecutive Stratified Days",
      colour = "Model"
    ) +
  #  ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_dur <- .add_obs_duration(p_dur)

  # ---- plot 2: stratification onset (DOY) ----------------------------------
  .add_obs_onset <- function(p) {
    if (!is.null(obs) && "Year" %in% names(obs) &&
        "Strat_DOY" %in% names(obs)) {
      p <- p + ggplot2::geom_point(
        data   = obs,
        ggplot2::aes(x = Year, y = Strat_DOY),
        colour = "black", shape = 17, size = 2.5,
        inherit.aes = FALSE
      )
    }
    p
  }

  p_onset <- ggplot2::ggplot(
    all_data,
    ggplot2::aes(x = Year, y = Strat_DOY,
                 colour = Model, group = Model)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::labs(
      title  = "Stratification Onset (Day of Year)",
      x      = "Year",
      y      = "Onset (DOY)",
      colour = "Model"
    ) +
   # ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_onset <- .add_obs_onset(p_onset)

  # ---- plot 3: mixing onset (DOY) ------------------------------------------
  .add_obs_mixing <- function(p) {
    if (!is.null(obs) && "Year" %in% names(obs) &&
        "Mixing_DOY" %in% names(obs)) {
      p <- p + ggplot2::geom_point(
        data   = obs,
        ggplot2::aes(x = Year, y = Mixing_DOY),
        colour = "black", shape = 17, size = 2.5,
        inherit.aes = FALSE
      )
    }
    p
  }

  p_mixing <- ggplot2::ggplot(
    all_data,
    ggplot2::aes(x = Year, y = Mixing_DOY,
                 colour = Model, group = Model)
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
    ggplot2::labs(
      title  = "Mixing Onset (Day of Year)",
      x      = "Year",
      y      = "Mixing Onset (DOY)",
      colour = "Model"
    ) +
   # ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_mixing <- .add_obs_mixing(p_mixing)

  list(
    duration_plot  = p_dur,
    onset_plot     = p_onset,
    mixing_plot    = p_mixing,
    combined_data  = all_data
  )
}
