#' Plot and compare anoxia metrics across models
#'
#' Takes the output of \code{cal_anoxic_date()} (either directly or as part of
#' the nested \code{cal_metrics()} output) and builds comparison plots for:
#' \itemize{
#'   \item yearly number of anoxic days,
#'   \item yearly anoxic factor (AF),
#'   \item daily anoxic depth time series.
#' }
#'
#' @param metrics_list list or character; either:
#' \itemize{
#'   \item the full \code{cal_metrics()} output,
#'   \item one metric sub-list (model -> metric instance), or
#'   \item a named model list of \code{cal_anoxic_date()} outputs,
#'   \item a path to an ensemble NetCDF file (\code{.nc}).
#' }
#' @param metric_name character; metric name to extract when
#'   \code{metrics_list} is the full \code{cal_metrics()} output. If not found,
#'   the function assumes \code{metrics_list} is already the per-model list.
#'   In NetCDF mode, this is used as preferred variable name for AF.
#' @param member integer; ensemble member index to read in NetCDF mode.
#'   Defaults to \code{1}.
#' @param years integer vector or \code{NULL}; optional subset of years to
#'   include in yearly plots (and used to filter the depth time series).
#' @param colors named character vector or \code{NULL}; optional model colours.
#' @param free_y logical; if \code{TRUE} use free y-axis scaling in yearly
#'   comparisons. Defaults to \code{FALSE}.
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{anoxic_days_plot}}{ggplot of yearly number of anoxic days per model.}
#'   \item{\code{af_plot}}{ggplot of yearly AF per model.}
#'   \item{\code{anoxic_depth_plot}}{ggplot of daily anoxic depth per model.}
#'   \item{\code{anoxic_days_data}}{tidy data used for anoxic days plot.}
#'   \item{\code{af_data}}{tidy data used for AF plot.}
#'   \item{\code{anoxic_depth_data}}{tidy data used for depth plot.}
#' }
#'
#' @examples
#' \dontrun{
#' plots <- plot_anoxic_metrics(mendota_metrics)
#' plots$anoxic_days_plot
#' plots$af_plot
#' plots$anoxic_depth_plot
#'
#' plots_subset <- plot_anoxic_metrics(mendota_metrics, years = 1995:2005)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_color_manual
#' @importFrom ggplot2  scale_x_continuous labs theme_bw theme element_text
#' @export

plot_anoxic_metrics <- function(metrics_list,
                                metric_name = "Anoxic_Factor",
                                member      = 1,
                                years       = NULL,
                                colors      = NULL,
                                free_y      = FALSE) {

  .has_anoxic_fields <- function(x) {
    is.list(x) && all(c("AF_yearly", "num_anoxic_days", "anoxic_depths") %in% names(x))
  }

  .extract_anoxic_struct <- function(x, max_depth = 4L) {
    # Direct cal_anoxic_date output
    if (.has_anoxic_fields(x)) {
      return(x)
    }

    if (!is.list(x) || max_depth <= 0L || length(x) == 0L) {
      return(NULL)
    }

    # Search recursively to support extra wrapper levels.
    for (i in seq_along(x)) {
      xi <- x[[i]]
      if (.has_anoxic_fields(xi)) {
        return(xi)
      }
      found <- .extract_anoxic_struct(xi, max_depth = max_depth - 1L)
      if (!is.null(found)) {
        return(found)
      }
    }

    NULL
  }

  .looks_like_model_block <- function(x) {
    if (!is.list(x) || length(x) == 0) {
      return(FALSE)
    }
    any(vapply(x, function(el) !is.null(.extract_anoxic_struct(el)), logical(1)))
  }

  .collect_anoxic_nodes <- function(x, path = character(0), max_depth = 8L) {
    if (max_depth < 0L) {
      return(list())
    }
    if (.has_anoxic_fields(x)) {
      return(list(list(path = path, data = x)))
    }
    if (!is.list(x) || length(x) == 0) {
      return(list())
    }

    out <- list()
    nm <- names(x)
    for (i in seq_along(x)) {
      key <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else as.character(i)
      child <- .collect_anoxic_nodes(x[[i]], c(path, key), max_depth = max_depth - 1L)
      if (length(child) > 0) {
        out <- c(out, child)
      }
    }
    out
  }

  .infer_model_from_path <- function(path_vec) {
    if (length(path_vec) == 0) {
      return("Model")
    }

    known_models <- c(
      "GLM", "WET", "SELMAPROTBAS", "SIMSTRAT",
      "SIMSTRAT-AED2", "GOTM-WET", "GOTM-SELMAPROTBAS", "OBS", "OBSERVATIONS"
    )

    p <- toupper(path_vec)
    for (m in known_models) {
      hit <- which(grepl(m, p, fixed = TRUE))
      if (length(hit) > 0) {
        model <- path_vec[hit[1]]
        model <- gsub("^.*(GLM|WET|SELMAPROTBAS|SIMSTRAT|OBS.*)$", "\\1", toupper(model))
        model <- sub("^OBSERVATIONS$", "Obs", model)
        model <- sub("^OBS$", "Obs", model)
        return(model)
      }
    }

    path_vec[1]
  }

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
    # 1) exact fallback first
    if (!is.null(fallback) && nzchar(fallback) && fallback %in% var_names) {
      return(fallback)
    }

    # 2) case-insensitive exact candidate match
    low <- tolower(var_names)
    for (cand in candidates) {
      hit <- which(low == tolower(cand))
      if (length(hit) > 0) return(var_names[hit[1]])
    }

    # 3) regex pattern fallback
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

  af_data <- data.frame()
  days_data <- data.frame()
  depth_data <- data.frame()

  # NetCDF mode: metrics_list is a path to ensemble output.
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

    af_var <- .find_nc_var(
      var_names,
      candidates = c("Anoxic_Factor", "AF", "anoxic.*factor", "^AF(_|$)"),
      fallback = metric_name
    )
    days_var <- .find_nc_var(
      var_names,
      candidates = c("Number_of_Anoxic_Days", "num_anoxic_days", "anoxic.*days")
    )
    depth_var <- .find_nc_var(
      var_names,
      candidates = c("Anoxic_Depth", "anoxic_depth", "anoxic.*depth")
    )

    if (!is.null(af_var)) {
      af_data <- .extract_nc_model_time(nc, af_var, datetime, model_names, member = as.integer(member))
      if (nrow(af_data) > 0) {
        names(af_data)[names(af_data) == "Value"] <- "AF_total"
        af_data$Year <- as.integer(format(as.Date(af_data$datetime), "%Y"))
      }
    }

    if (!is.null(days_var)) {
      days_data <- .extract_nc_model_time(nc, days_var, datetime, model_names, member = as.integer(member))
      if (nrow(days_data) > 0) {
        names(days_data)[names(days_data) == "Value"] <- "num_anoxic_days"
        days_data$Year <- as.integer(format(as.Date(days_data$datetime), "%Y"))
      }
    }

    if (!is.null(depth_var)) {
      depth_data <- .extract_nc_model_time(nc, depth_var, datetime, model_names, member = as.integer(member))
      if (nrow(depth_data) > 0) {
        names(depth_data)[names(depth_data) == "Value"] <- "anoxic_depth"
        depth_data$Year <- as.integer(format(as.Date(depth_data$datetime), "%Y"))
      }
    }

    if (nrow(af_data) == 0 && nrow(days_data) == 0 && nrow(depth_data) == 0) {
      stop(
        "No anoxia variables were found in NetCDF. Available variables: ",
        paste(var_names, collapse = ", ")
      )
    }
  }

  if (nrow(af_data) == 0 && nrow(days_data) == 0 && nrow(depth_data) == 0) {
    # Accept either full cal_metrics output or already selected metric list.
    if (!is.null(names(metrics_list)) && metric_name %in% names(metrics_list)) {
      anoxic_list <- metrics_list[[metric_name]]
    } else if (.looks_like_model_block(metrics_list)) {
      anoxic_list <- metrics_list
    } else {
      # Try case-insensitive and partial matching against metric names.
      metric_names <- names(metrics_list)
      idx <- integer(0)
      if (!is.null(metric_names) && length(metric_names) > 0) {
        idx <- grep(metric_name, metric_names, ignore.case = TRUE, fixed = TRUE)
      }

      if (length(idx) == 1L) {
        anoxic_list <- metrics_list[[idx]]
      } else {
        # Last fallback: auto-detect first metric block that contains anoxia output.
        detected <- NULL
        detected_name <- NULL
        if (!is.null(metric_names) && length(metric_names) > 0) {
          for (nm in metric_names) {
            block <- metrics_list[[nm]]
            if (.looks_like_model_block(block)) {
              detected <- block
              detected_name <- nm
              break
            }
          }
        }

        if (is.null(detected)) {
          stop(
            "Could not locate anoxia metric block. Requested metric_name: '", metric_name,
            "'. Available top-level metrics: ",
            if (is.null(metric_names)) "<none>" else paste(metric_names, collapse = ", ")
          )
        }

        message("Using detected anoxia metric block: ", detected_name)
        anoxic_list <- detected
      }
    }
  }

  .standardize_yearly <- function(df, value_col_name) {
    if (!is.data.frame(df) || ncol(df) < 2) return(NULL)
    out <- data.frame(
      Year = as.integer(df[[1]]),
      Value = as.numeric(df[[2]]),
      stringsAsFactors = FALSE
    )
    names(out)[2] <- value_col_name
    out
  }

  .standardize_depth <- function(df) {
    if (!is.data.frame(df) || ncol(df) < 2) return(NULL)
    out <- data.frame(
      datetime = as.POSIXct(df[[1]], tz = "UTC"),
      anoxic_depth = as.numeric(df[[2]]),
      stringsAsFactors = FALSE
    )
    out$Year <- as.integer(format(as.Date(out$datetime), "%Y"))
    out
  }

  if (nrow(af_data) == 0 && nrow(days_data) == 0 && nrow(depth_data) == 0) {
    nodes <- .collect_anoxic_nodes(anoxic_list)
    if (length(nodes) == 0) {
      stop("No model entries found in supplied anoxia metric list.")
    }

    node_models <- vapply(nodes, function(n) .infer_model_from_path(n$path), character(1))
    unique_models <- unique(node_models)

    af_data <- bind_rows(lapply(unique_models, function(m) {
      idx <- which(node_models == m)
      bind_rows(lapply(idx, function(i) {
        d <- .standardize_yearly(nodes[[i]]$data$AF_yearly, "AF_total")
        if (is.null(d)) return(NULL)
        d$Model <- m
        d
      }))
    }))

    days_data <- bind_rows(lapply(unique_models, function(m) {
      idx <- which(node_models == m)
      bind_rows(lapply(idx, function(i) {
        d <- .standardize_yearly(nodes[[i]]$data$num_anoxic_days, "num_anoxic_days")
        if (is.null(d)) return(NULL)
        d$Model <- m
        d
      }))
    }))

    depth_data <- bind_rows(lapply(unique_models, function(m) {
      idx <- which(node_models == m)
      bind_rows(lapply(idx, function(i) {
        d <- .standardize_depth(nodes[[i]]$data$anoxic_depths)
        if (is.null(d)) return(NULL)
        d$Model <- m
        d
      }))
    }))
  }

  if (nrow(af_data) == 0 && nrow(days_data) == 0 && nrow(depth_data) == 0) {
    stop("No valid anoxia data could be extracted for plotting.")
  }

  if (!is.null(years)) {
    if (nrow(af_data) > 0) {
      af_data <- af_data[af_data$Year %in% years, , drop = FALSE]
    }
    if (nrow(days_data) > 0) {
      days_data <- days_data[days_data$Year %in% years, , drop = FALSE]
    }
    if (nrow(depth_data) > 0) {
      depth_data <- depth_data[depth_data$Year %in% years, , drop = FALSE]
    }
  }

  # Ensure yearly plots are truly yearly summaries (one value per model-year).
  if (nrow(af_data) > 0) {
    af_data <- af_data %>%
      dplyr::group_by(Model, Year) %>%
      dplyr::summarise(AF_total = mean(AF_total, na.rm = TRUE), .groups = "drop")
  }

  if (nrow(days_data) > 0) {
    days_data <- days_data %>%
      dplyr::group_by(Model, Year) %>%
      dplyr::summarise(num_anoxic_days = mean(num_anoxic_days, na.rm = TRUE), .groups = "drop")
  }

  present_models <- unique(c(
    if (nrow(af_data) > 0) af_data$Model else character(0),
    if (nrow(days_data) > 0) days_data$Model else character(0),
    if (nrow(depth_data) > 0) depth_data$Model else character(0)
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

  year_vals <- c(
    if (nrow(days_data) > 0) days_data$Year else numeric(0),
    if (nrow(af_data) > 0) af_data$Year else numeric(0)
  )
  year_breaks <- .year_breaks(year_vals)

  p_days <- ggplot2::ggplot(days_data,
                            ggplot2::aes(x = Year, y = num_anoxic_days,
                                         colour = Model, group = Model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = year_breaks, labels = .year_labels) +
    ggplot2::labs(
      title = "Number of Anoxic Days",
      x = "Year",
      y = "Days",
      colour = "Model"
    ) +
   # ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_af <- ggplot2::ggplot(af_data,
                          ggplot2::aes(x = Year, y = AF_total,
                                       colour = Model, group = Model)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(breaks = year_breaks, labels = .year_labels) +
    ggplot2::labs(
      title = "Anoxic Factor (AF)",
      x = "Year",
      y = "AF",
      colour = "Model"
    ) +
   # ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_depth <- ggplot2::ggplot(depth_data,
                             ggplot2::aes(x = datetime, y = anoxic_depth,
                                          colour = Model, group = Model)) +
    ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(
      title = "Daily Anoxic Depth",
      x = "Date",
      y = "Anoxic Depth (m)",
      colour = "Model"
    ) +
  #  ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (isTRUE(free_y)) {
    # Keep API parity with plot_strat_metrics; only yearly plots are affected.
    p_days <- p_days + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
    p_af <- p_af + ggplot2::facet_wrap(~Model, scales = "free_y", ncol = 1)
  }

  list(
    anoxic_days_plot  = p_days,
    af_plot           = p_af,
    anoxic_depth_plot = p_depth,
    anoxic_days_data  = days_data,
    af_data           = af_data,
    anoxic_depth_data = depth_data
  )
}
