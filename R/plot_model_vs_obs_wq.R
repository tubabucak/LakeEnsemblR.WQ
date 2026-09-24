#' @title Plot one or more models' output against observed data at matching depths
#'
#' @description
#' Extracts each requested model's simulated output at the depths present in
#' an observed dataset (via \code{get_output_wq(obs_depths = ...)}) and plots
#' it against the observations, one facet per depth, one line per model
#' (colored), with observed points overlaid once per facet. Reads whatever
#' \code{output.nc} currently exists for each \code{model} -- it does not run
#' the model itself, and has no notion of "best"/calibrated parameters. To
#' compare against a calibrated run, first write the winning parameters back
#' (\code{write_best_calib_to_par_files()}) and re-run the model
#' (\code{run_ensemble_wq()}) before calling this.
#'
#' @param config_file character; path to the Output config YAML (as used by
#'   \code{load_config()}/\code{get_output_wq()}).
#' @param model character vector; one or more models to extract, e.g.
#'   \code{"GLM-AED2"}, \code{"GOTM-WET"}, \code{"GOTM-Selmaprotbas"}, or
#'   \code{"Simstrat-AED2"} (also accepts the short forms \code{"GLM"},
#'   \code{"WET"}, \code{"SELMAPROTBAS"}, \code{"SIMSTRAT"}). Passing more
#'   than one plots them together against the same observations, one line
#'   per model per depth facet; passing one reproduces the original
#'   single-model behavior (including the per-depth KGE/RMSE facet labels).
#' @param vars character or \code{NULL}. Model-native variable name to
#'   extract (passed to \code{get_output_wq()}'s \code{vars} argument). If
#'   \code{NULL} (default), auto-derived independently for each \code{model}
#'   from the metrics dictionary's \code{variable_model_name} for that
#'   \code{model}/\code{variable_global_name} -- the same lookup calibration
#'   itself uses. Only single-variable extraction is supported here. A
#'   non-\code{NULL} value is used as-is for every requested model, so leave
#'   it \code{NULL} when \code{model} has more than one entry unless you're
#'   sure the same native variable name applies to all of them.
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
#'   \code{NULL} (default), auto-derived independently for each \code{model}
#'   from the metrics dictionary the same way \code{calib_wq()}'s calibration
#'   scoring does: looked up by that \code{model}/\code{variable_global_name}.
#'   Pass a number explicitly to override the dictionary lookup for every
#'   requested model.
#' @param dict_file character, data.frame, or \code{NULL}. Metrics dictionary
#'   source passed to the internal dictionary loader when auto-deriving
#'   \code{vars}/\code{conversion_factor}. If \code{NULL} (default), uses
#'   \code{load_config(config_file)$metrics_dict_file}, falling back to the
#'   package's bundled default dictionary.
#' @param wq_config_file character or \code{NULL}. Path to the
#'   \code{LakeEnsemblR_WQ.yaml} config file. Only needed for
#'   \code{model = "GOTM-Selmaprotbas"}/\code{"GOTM-WET"} when the
#'   auto-derived \code{vars} resolves to the dictionary's generic
#'   \code{"zooplankton_*"} placeholder -- since SELMAPROTBAS/WET always
#'   create one named FABM instance per configured zooplankton group (never
#'   a literal instance called \code{"zooplankton"}), that placeholder is
#'   expanded into each group's own output variable (e.g. \code{"daphnia_c"},
#'   \code{"cyclops_c"}), fetched, and summed into one total zooplankton
#'   series before plotting.
#' @param depth_tol numeric; tolerance (in the same units as \code{depth} in
#'   \code{obs_data}) used to bin observed depths before faceting. Depths are
#'   rounded to the nearest multiple of \code{depth_tol} (default \code{0.5}),
#'   so observations from different casts that land within \code{depth_tol}
#'   of each other (e.g. \code{21.9}, \code{22.0}, \code{22.3}) are treated as
#'   one sampling depth/facet instead of three, and averaged where they share
#'   a binned depth and datetime. Set to a smaller value (or \code{0}) to
#'   disable binning and facet on raw observed depths.
#'
#' @return A list with:
#' \describe{
#'   \item{plot}{A ggplot2 object: one facet per (binned) observed depth that
#'     has at least one matched observation for at least one model, one
#'     colored line per model plus observed points, with a legend
#'     distinguishing models (and "Observed"). Depths with no matched
#'     observation at all (e.g. an observed date that never lines up with
#'     any simulated one) are dropped rather than shown as an empty panel.
#'     When only one \code{model} is requested, the facet strip additionally
#'     shows that model's per-depth KGE/RMSE, matching the single-model
#'     behavior from before this function supported multiple models.}
#'   \item{data}{The joined long-format data frame (\code{Model},
#'     \code{datetime}, \code{depth}, \code{Predicted}, \code{Observed}) used
#'     to build the plot -- covers the full simulated series at each depth
#'     kept in \code{plot} for each model, with \code{Observed} \code{NA}
#'     wherever there's no observation on that particular date (matching is
#'     by calendar date, not exact timestamp, since model output is
#'     daily-or-coarser while observed records can carry an arbitrary
#'     time-of-day).}
#'   \item{stats}{A data frame with one row per \code{Model}/\code{depth}
#'     combination: \code{Model}, \code{depth}, \code{NSE}, \code{RMSE},
#'     \code{NRMSE}, \code{PBIAS}, \code{KGE}, \code{n}.}
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw facet_wrap
#' @importFrom dplyr filter mutate arrange inner_join group_by summarise bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @export
plot_model_vs_obs_wq <- function(config_file, model, vars = NULL, obs_data,
                                 variable_global_name, y_title = variable_global_name,
                                 conversion_factor = NULL, dict_file = NULL,
                                 wq_config_file = NULL, depth_tol = 0.5) {

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

  # Match to the model on calendar date, not exact timestamp. Model output is
  # written on a daily (or coarser) timestep, but observed records can carry
  # an arbitrary time-of-day (e.g. a sonde/buoy reading logged at 14:23),
  # sometimes inconsistently within the same obs_data across instruments/
  # years -- an exact POSIXct join would silently drop those rows (they'd
  # show up as "no matching obs" facets) even though the day is covered.
  obs_sub$datetime <- as.Date(obs_sub$datetime)

  # Bin observed depths to the nearest depth_tol (default 0.5 m) before
  # faceting. Real profile data has near-duplicate depths across casts (e.g.
  # 21.9, 22.0, 22.3 m) that are effectively the same sampling depth --
  # without binning, each becomes its own facet, and the %.2g-rounded strip
  # label used to make them look like literal duplicates of one another.
  # Rows that land on the same (date, binned depth) after binning -- either
  # from the depth binning above, or from multiple same-day readings at one
  # depth -- are averaged so the join below doesn't fan out.
  obs_sub$depth <- if (isTRUE(depth_tol > 0)) round(obs_sub$depth / depth_tol) * depth_tol else obs_sub$depth
  obs_sub <- obs_sub %>%
    dplyr::group_by(datetime, depth) %>%
    dplyr::summarise(value = mean(value), .groups = "drop")

  obs_depths <- sort(unique(obs_sub$depth))
  if (length(obs_depths) == 0) {
    stop("No usable (non-NA) depths found in 'obs_data' for variable_global_name = '",
         variable_global_name, "'.")
  }

  obs_long <- obs_sub %>%
    dplyr::mutate(Observed = value) %>%
    dplyr::select(datetime, depth, Observed)

  # ---- Per-model extraction, joined against the shared obs_long above ----
  # Everything about the observations (date-matching, depth-binning) only
  # needs figuring out once, above, since it doesn't depend on which model
  # is being extracted -- plotting several models just means calling this
  # once per model and stacking the results.
  .extract_one_model <- function(model_i) {
    model_upper <- toupper(model_i)
    model_short <- if (grepl("GLM", model_upper)) {
      "GLM"
    } else if (grepl("SIMSTRAT", model_upper)) {
      "SIMSTRAT"
    } else if (grepl("SELMA", model_upper)) {
      "SELMAPROTBAS"
    } else if (grepl("WET", model_upper)) {
      "WET"
    } else {
      stop("Could not determine model type from 'model' = '", model_i,
           "'. Expected something containing GLM, WET, SELMA, or SIMSTRAT.")
    }

    vars_i <- vars
    conversion_factor_i <- conversion_factor
    if (is.null(vars_i) || is.null(conversion_factor_i)) {
      # Only require this model's output folder to exist.
      cfg <- load_config(config_file, required_models = model_short)
      dict_src <- if (!is.null(dict_file)) dict_file else cfg$metrics_dict_file
      dict <- .load_metrics_dictionary_wq(dict_file = dict_src)
      dict_row <- dict[toupper(dict$model) == model_short &
                         trimws(as.character(dict$variable_global_name)) == variable_global_name, , drop = FALSE]
      if (nrow(dict_row) == 0) {
        stop("Could not find a metrics dictionary entry for model = '", model_short,
             "', variable_global_name = '", variable_global_name, "' to auto-derive ",
             "'vars'/'conversion_factor'. Pass both explicitly instead.")
      }
      if (is.null(vars_i)) {
        vars_i <- as.character(dict_row$variable_model_name[1])
      }
      if (is.null(conversion_factor_i)) {
        conversion_factor_i <- suppressWarnings(as.numeric(as.character(dict_row$conversion_factor[1])))
        if (is.na(conversion_factor_i)) conversion_factor_i <- 1
      }
    }

    # SELMAPROTBAS/WET always name their zooplankton FABM instance(s) after
    # the configured group(s) (e.g. "daphnia", "cyclops") -- never a literal
    # instance called "zooplankton". The dictionary's generic "zooplankton_*"
    # entries are a placeholder for that: resolve it to one real per-group
    # variable name each, fetch them all, and sum into one total series.
    is_generic_zoo_var <- model_short %in% c("SELMAPROTBAS", "WET") &&
      grepl("^zooplankton_", vars_i)

    if (is_generic_zoo_var) {
      if (is.null(wq_config_file)) {
        stop("model = '", model_short, "', variable_global_name = '", variable_global_name,
             "' resolves to the dictionary's generic 'zooplankton_*' variable, which needs ",
             "per-group expansion. Pass 'wq_config_file' (path to LakeEnsemblR_WQ.yaml) so the ",
             "configured zooplankton group names can be resolved.")
      }
      wq_cfg <- yaml::read_yaml(wq_config_file)
      zoo_groups <- names(wq_cfg[["zooplankton"]][["groups"]])
      if (length(zoo_groups) == 0) {
        stop("No zooplankton groups found under 'zooplankton/groups' in '", wq_config_file, "'.")
      }

      suffix <- sub("^zooplankton", "", vars_i)
      group_vars <- paste0(zoo_groups, suffix)

      sim_list <- get_output_wq(
        config_file       = config_file,
        model             = model_short,
        vars              = group_vars,
        obs_depths        = obs_depths,
        depth_01          = 1,
        conversion_factor = conversion_factor_i
      )
      # Sum every group's (already unit-converted) series into one total.
      sim_df <- Reduce(function(a, b) {
        stopifnot(identical(a$datetime, b$datetime), identical(names(a), names(b)))
        a[, -1] <- a[, -1] + b[, -1]
        a
      }, sim_list)
    } else {
      sim_list <- get_output_wq(
        config_file       = config_file,
        model             = model_short,
        vars              = vars_i,
        obs_depths        = obs_depths,
        depth_01          = 1,
        conversion_factor = conversion_factor_i
      )
      sim_df <- sim_list[[1]]
    }
    if (is.null(sim_df) || nrow(sim_df) == 0) {
      stop("get_output_wq() returned no data for model = '", model_short,
           "', vars = '", vars_i, "'.")
    }

    sim_long <- sim_df %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("Depth_"),
                          names_to = "depth", values_to = "Predicted") %>%
      dplyr::mutate(depth = as.numeric(gsub("Depth_", "", depth)))

    # Match observed (canonical, binned) depths to the nearest extracted
    # simulation depth for THIS model (guards against floating-point
    # formatting mismatches -- each model's own native depth grid can
    # round-trip through column-name string formatting slightly
    # differently, and different models may snap to slightly different
    # native grid points for the same observed depth).
    obs_depth_map <- vapply(unique(obs_long$depth), function(d) {
      sim_depths <- unique(sim_long$depth)
      sim_depths[which.min(abs(sim_depths - d))]
    }, numeric(1))
    names(obs_depth_map) <- as.character(unique(obs_long$depth))

    obs_long_i <- obs_long
    obs_long_i$depth <- unname(obs_depth_map[as.character(obs_long_i$depth)])

    # get_output_wq() returns the model's whole native output depth grid
    # (per Output.yaml's output/depths spacing), which is almost always much
    # finer than the observed depths. Restrict to just the (snapped) depths
    # observations actually matched to, so the plot doesn't fill up with
    # empty "NA" facets for every unobserved grid depth in between.
    sim_long <- sim_long[sim_long$depth %in% unique(obs_long_i$depth), , drop = FALSE]

    # Left join keeps the full simulated series (so the modeled line stays
    # continuous even where observations are sparse); Observed is NA
    # wherever there's no matching observation at that date/depth. Join key
    # is the calendar date (obs_long_i$datetime is a Date, from the
    # truncation above) -- sim_long keeps its original POSIXct datetime for
    # the x-axis/line.
    sim_long$.join_date <- as.Date(sim_long$datetime)
    joined_i <- dplyr::left_join(sim_long, obs_long_i,
                                 by = c(".join_date" = "datetime", "depth" = "depth"))
    joined_i$.join_date <- NULL

    # Re-express depth on the canonical (obs-bin) scale rather than this
    # model's own snapped value, so facets line up across models even when
    # two models' nearest native grid points for the same obs bin differ
    # very slightly. (In the rare case two different obs bins snap to the
    # very same native depth for this model, both map back to whichever
    # bin's name comes first in obs_depth_map -- a pre-existing limitation
    # of nearest-depth snapping, not something introduced here.)
    joined_i$depth <- as.numeric(names(obs_depth_map))[match(joined_i$depth, obs_depth_map)]

    joined_i$Model <- model_short
    joined_i
  }

  model <- unique(as.character(model))
  joined <- dplyr::bind_rows(lapply(model, .extract_one_model))

  if (all(is.na(joined$Observed))) {
    stop("No overlapping datetime/depth rows between simulated and observed data. ",
         "Check that 'obs_data' datetimes fall within the model's simulation period.")
  }

  matched <- joined[!is.na(joined$Observed), , drop = FALSE]
  stats_by_group <- lapply(
    split(matched, list(matched$Model, matched$depth), drop = TRUE),
    function(sub) {
      st <- cal_stats(sub$Observed, sub$Predicted)
      data.frame(Model = sub$Model[1], depth = sub$depth[1], NSE = st$NSE,
                 RMSE = st$RMSE, NRMSE = st$NRMSE, PBIAS = st$PBIAS,
                 KGE = st$KGE, n = nrow(sub))
    }
  )
  stats_df <- do.call(rbind, stats_by_group)
  rownames(stats_df) <- NULL

  # Drop facets for depths with zero matched observations for EVERY model --
  # nothing to plot there at all. A depth with a match for only some models
  # still shows (with those models' KGE/RMSE, when there's just one model
  # overall -- see strip_labels below).
  joined <- joined[joined$depth %in% unique(stats_df$depth), , drop = FALSE]

  facet_depths <- sort(unique(joined$depth))
  single_model <- length(unique(joined$Model)) == 1
  strip_labels <- stats::setNames(
    vapply(facet_depths, function(d) {
      if (single_model) {
        i <- match(d, stats_df$depth)
        if (is.na(i)) {
          sprintf("Depth %.4g m", d)
        } else {
          sprintf("Depth %.4g m  (KGE=%.2f, RMSE=%.2f)", d, stats_df$KGE[i], stats_df$RMSE[i])
        }
      } else {
        # Per-depth stats for every model at once don't fit legibly in one
        # strip label -- see the returned 'stats' data frame instead.
        sprintf("Depth %.4g m", d)
      }
    }, character(1)),
    as.character(facet_depths)
  )

  # Observed points are identical across models (same obs_long) -- keep one
  # set of points per datetime/depth rather than plotting length(model)
  # overlapping duplicates.
  obs_points <- unique(joined[!is.na(joined$Observed), c("datetime", "depth", "Observed")])

  p <- ggplot2::ggplot(joined, ggplot2::aes(x = datetime)) +
    ggplot2::geom_line(ggplot2::aes(y = Predicted, color = Model)) +
    ggplot2::geom_point(data = obs_points,
                        ggplot2::aes(y = Observed, color = "Observed"), size = 1.5) +
    ggplot2::facet_wrap(~depth, labeller = ggplot2::as_labeller(strip_labels)) +
    ggplot2::labs(x = "Date", y = y_title, color = NULL,
                 title = paste0(paste(unique(joined$Model), collapse = " / "), ": ",
                               variable_global_name)) +
    ggplot2::theme_bw()

  list(plot = p, data = joined, stats = stats_df)
}
