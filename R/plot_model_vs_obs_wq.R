#' @title Plot a single model's output against observed data at matching depths
#'
#' @description
#' Extracts a model's simulated output at the depths present in an observed
#' dataset (via \code{get_output_wq(obs_depths = ...)}) and plots it against
#' the observations, one facet per depth, with KGE/RMSE annotated. Reads
#' whatever \code{output.nc} currently exists for \code{model} -- it does not
#' run the model itself, and has no notion of "best"/calibrated parameters.
#' To compare against a calibrated run, first write the winning parameters
#' back (\code{write_best_calib_to_par_files()}) and re-run the model
#' (\code{run_ensemble_wq()}) before calling this. For comparing multiple
#' coupled models at once, see \code{compare_plot()}/\code{scat_plot()}
#' instead.
#'
#' @param config_file character; path to the Output config YAML (as used by
#'   \code{load_config()}/\code{get_output_wq()}).
#' @param model character; model to extract, e.g. \code{"GLM-AED2"},
#'   \code{"GOTM-WET"}, \code{"GOTM-Selmaprotbas"}, or \code{"Simstrat-AED2"}
#'   (also accepts the short forms \code{"GLM"}, \code{"WET"},
#'   \code{"SELMAPROTBAS"}, \code{"SIMSTRAT"}).
#' @param vars character or \code{NULL}. Model-native variable name to
#'   extract (passed to \code{get_output_wq()}'s \code{vars} argument). If
#'   \code{NULL} (default), auto-derived from the metrics dictionary's
#'   \code{variable_model_name} for \code{model}/\code{variable_global_name}
#'   -- the same lookup calibration itself uses. Only single-variable
#'   extraction is supported here.
#' @param obs_data character or data.frame; either a path to a CSV, or an
#'   already-loaded data frame, with columns \code{datetime}, \code{depth},
#'   \code{variable_global_name}, \code{value}.
#' @param variable_global_name character; which \code{variable_global_name}
#'   in \code{obs_data} to compare against.
#' @param y_title character; y-axis label for the plot (e.g.
#'   \code{"DO (g/m3)"}).
#' @param conversion_factor numeric or \code{NULL}. Applied to the model
#'   output so it matches \code{obs_data}'s (harmonized) units, since model
#'   output from \code{get_output_wq()} is in model-native units (e.g. GLM's
#'   DO is mmol O2/m3, not grams/m3 as in a typical observed CSV). If
#'   \code{NULL} (default), auto-derived from the metrics dictionary the same
#'   way \code{run_lhc_wq()}'s calibration scoring does: looked up by
#'   \code{model}/\code{variable_global_name}. Pass a number explicitly to
#'   override the dictionary lookup.
#' @param dict_file character, data.frame, or \code{NULL}. Metrics dictionary
#'   source passed to the internal dictionary loader when auto-deriving
#'   \code{vars}/\code{conversion_factor}. If \code{NULL} (default), uses
#'   \code{load_config(config_file)$metrics_dict_file}, falling back to the
#'   package's bundled default dictionary.
#'
#' @return A list with:
#' \describe{
#'   \item{plot}{A ggplot2 object: one facet per observed depth, modeled line
#'     vs. observed points, with per-depth KGE/RMSE in the facet strip.}
#'   \item{data}{The joined long-format data frame (\code{datetime}, \code{depth},
#'     \code{Predicted}, \code{Observed}) used to build the plot.}
#'   \item{stats}{A data frame with one row per depth: \code{depth}, \code{NSE},
#'     \code{RMSE}, \code{NRMSE}, \code{PBIAS}, \code{KGE}, \code{n}.}
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw facet_wrap
#' @importFrom dplyr filter mutate arrange inner_join
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @export
plot_model_vs_obs_wq <- function(config_file, model, vars = NULL, obs_data,
                                 variable_global_name, y_title = variable_global_name,
                                 conversion_factor = NULL, dict_file = NULL) {

  if (is.character(obs_data)) {
    obs_data <- utils::read.csv(obs_data, stringsAsFactors = FALSE)
  }
  required_obs_cols <- c("datetime", "depth", "variable_global_name", "value")
  missing_obs_cols <- setdiff(required_obs_cols, names(obs_data))
  if (length(missing_obs_cols) > 0) {
    stop("'obs_data' is missing required column(s): ", paste(missing_obs_cols, collapse = ", "))
  }

  obs_sub <- obs_data[obs_data$variable_global_name == variable_global_name, , drop = FALSE]
  if (nrow(obs_sub) == 0) {
    stop("No rows in 'obs_data' match variable_global_name = '", variable_global_name, "'.")
  }
  obs_sub$datetime <- as.POSIXct(obs_sub$datetime, tz = "UTC")
  obs_sub$depth <- as.numeric(obs_sub$depth)
  obs_sub$value <- as.numeric(obs_sub$value)
  obs_sub <- obs_sub[is.finite(obs_sub$value) & !is.na(obs_sub$datetime) & !is.na(obs_sub$depth), , drop = FALSE]

  obs_depths <- sort(unique(obs_sub$depth))
  if (length(obs_depths) == 0) {
    stop("No usable (non-NA) depths found in 'obs_data' for variable_global_name = '",
         variable_global_name, "'.")
  }

  model_upper <- toupper(model)
  model_short <- if (grepl("GLM", model_upper)) {
    "GLM"
  } else if (grepl("SIMSTRAT", model_upper)) {
    "SIMSTRAT"
  } else if (grepl("SELMA", model_upper)) {
    "SELMAPROTBAS"
  } else if (grepl("WET", model_upper)) {
    "WET"
  } else {
    stop("Could not determine model type from 'model' = '", model,
         "'. Expected something containing GLM, WET, SELMA, or SIMSTRAT.")
  }

  if (is.null(vars) || is.null(conversion_factor)) {
    cfg <- load_config(config_file)
    dict_src <- if (!is.null(dict_file)) dict_file else cfg$metrics_dict_file
    dict <- .load_metrics_dictionary_wq(dict_file = dict_src)
    dict_row <- dict[toupper(dict$model) == model_short &
                       trimws(as.character(dict$variable_global_name)) == variable_global_name, , drop = FALSE]
    if (nrow(dict_row) == 0) {
      stop("Could not find a metrics dictionary entry for model = '", model_short,
           "', variable_global_name = '", variable_global_name, "' to auto-derive ",
           "'vars'/'conversion_factor'. Pass both explicitly instead.")
    }
    if (is.null(vars)) {
      vars <- as.character(dict_row$variable_model_name[1])
    }
    if (is.null(conversion_factor)) {
      conversion_factor <- suppressWarnings(as.numeric(as.character(dict_row$conversion_factor[1])))
      if (is.na(conversion_factor)) conversion_factor <- 1
    }
  }

  sim_list <- get_output_wq(
    config_file       = config_file,
    model             = model_short,
    vars              = vars,
    obs_depths        = obs_depths,
    depth_01          = 1,
    conversion_factor = conversion_factor
  )
  sim_df <- sim_list[[1]]
  if (is.null(sim_df) || nrow(sim_df) == 0) {
    stop("get_output_wq() returned no data for model = '", model_short,
         "', vars = '", vars, "'.")
  }

  sim_long <- sim_df %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("Depth_"),
                        names_to = "depth", values_to = "Predicted") %>%
    dplyr::mutate(depth = as.numeric(gsub("Depth_", "", depth)))

  obs_long <- obs_sub %>%
    dplyr::mutate(Observed = value) %>%
    dplyr::select(datetime, depth, Observed)

  # Match observed depths to the nearest extracted simulation depth (guards
  # against floating-point formatting mismatches between the two, since both
  # ultimately derive from the same obs_depths values but round-trip through
  # column-name string formatting on the simulation side).
  obs_long$depth <- vapply(obs_long$depth, function(d) {
    sim_depths <- unique(sim_long$depth)
    sim_depths[which.min(abs(sim_depths - d))]
  }, numeric(1))

  joined <- dplyr::inner_join(sim_long, obs_long, by = c("datetime", "depth"))
  if (nrow(joined) == 0) {
    stop("No overlapping datetime/depth rows between simulated and observed data. ",
         "Check that 'obs_data' datetimes fall within the model's simulation period.")
  }

  stats_by_depth <- lapply(sort(unique(joined$depth)), function(d) {
    sub <- joined[joined$depth == d, , drop = FALSE]
    st <- cal_stats(sub$Observed, sub$Predicted)
    data.frame(depth = d, NSE = st$NSE, RMSE = st$RMSE, NRMSE = st$NRMSE,
               PBIAS = st$PBIAS, KGE = st$KGE, n = nrow(sub))
  })
  stats_df <- do.call(rbind, stats_by_depth)

  strip_labels <- stats::setNames(
    sprintf("Depth %.2g m  (KGE=%.2f, RMSE=%.2f)", stats_df$depth, stats_df$KGE, stats_df$RMSE),
    as.character(stats_df$depth)
  )

  p <- ggplot2::ggplot(joined, ggplot2::aes(x = datetime)) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted, color = "Modeled")) +
    ggplot2::geom_point(ggplot2::aes(y = Observed, color = "Observed"), size = 1.5) +
    ggplot2::facet_wrap(~depth, labeller = ggplot2::as_labeller(strip_labels)) +
    ggplot2::labs(x = "Date", y = y_title, color = NULL,
                 title = paste0(model_short, ": ", variable_global_name)) +
    ggplot2::theme_bw()

  list(plot = p, data = joined, stats = stats_df)
}
