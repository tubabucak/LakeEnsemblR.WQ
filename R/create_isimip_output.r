#' ISIMIP lake-sector variables written by create_netcdf_output(format = "isimip")
#'
#' Each entry maps an ISIMIP variable to the cal_metrics() metrics it is
#' built from: \code{add} metrics are summed, \code{sub} metrics subtracted
#' (e.g. tpd = PO4 + DOP, pp = TP - PO4 - DOP). Values are divided by
#' \code{to_si} to go from the metric unit to the ISIMIP unit (molar mass in
#' g/mol for g/m3 -> mol/m3; mg/m3 needs a further 1000).
#'
#' @noRd
.isimip_vars <- list(
  chl      = list(long_name = "Chlorophyll Concentration", units = "g m-3",
                  add = "Total_Chla_miligramsPerCubicMeter", to_si = 1000),
  phytobio = list(long_name = "Phytoplankton Functional Group Biomass", units = "mol m-3",
                  add = "Phyto_C_miligramsPerCubicMeter", to_si = 12.011 * 1000, groups = TRUE),
  zoobio   = list(long_name = "Zooplankton Biomass", units = "mol m-3",
                  add = "Zoop_C_gramsPerCubicMeter", to_si = 12.011),
  tp       = list(long_name = "Total Phosphorus", units = "mol m-3",
                  add = "TP_gramsPerCubicMeter", to_si = 30.974),
  pp       = list(long_name = "Particulate Phosphorus", units = "mol m-3",
                  add = "TP_gramsPerCubicMeter",
                  sub = c("PO4_gramsPerCubicMeter", "DOP_gramsPerCubicMeter"), to_si = 30.974),
  tpd      = list(long_name = "Total Dissolved Phosphorus", units = "mol m-3",
                  add = c("PO4_gramsPerCubicMeter", "DOP_gramsPerCubicMeter"), to_si = 30.974),
  tn       = list(long_name = "Total Nitrogen", units = "mol m-3",
                  add = "TN_gramsPerCubicMeter", to_si = 14.007),
  pn       = list(long_name = "Particulate Nitrogen", units = "mol m-3",
                  add = "TN_gramsPerCubicMeter",
                  sub = c("NO3_gramsPerCubicMeter", "NH4_gramsPerCubicMeter", "DON_gramsPerCubicMeter"),
                  to_si = 14.007),
  tdn      = list(long_name = "Total Dissolved Nitrogen", units = "mol m-3",
                  add = c("NO3_gramsPerCubicMeter", "NH4_gramsPerCubicMeter", "DON_gramsPerCubicMeter"),
                  to_si = 14.007),
  do       = list(long_name = "Dissolved Oxygen", units = "mol m-3",
                  add = "DO_gramsPerCubicMeter", to_si = 31.998),
  doc      = list(long_name = "Dissolved Organic Carbon", units = "mol m-3",
                  add = "DOC_gramsPerCubicMeter", to_si = 12.011),
  # Silica_gramsPerCubicMeter is g SiO2/m3 for GLM, Simstrat and WET but
  # g Si/m3 for Selmaprotbas (see Metrics_dict), hence the per-model divisor
  si       = list(long_name = "Dissolved Silica", units = "mol m-3",
                  add = "Silica_gramsPerCubicMeter", to_si = 60.084,
                  to_si_model = c(SELMAPROTBAS = 28.086))
)

#' Default ISIMIP model names for the LakeEnsemblR.WQ model keys
#' @noRd
.isimip_model_names <- c(GLM = "glm-aed", SIMSTRAT = "simstrat-aed2",
                         WET = "gotm-wet", SELMAPROTBAS = "gotm-selmaprotbas")

#' @title One model's daily profile for a metric
#'
#' @description Collects the data.frames of \code{metric} for \code{model}
#'   from a cal_metrics() output (\code{metric -> MODEL -> instance -> df}),
#'   as a named list of daily-mean matrices (rows = days, cols = depths),
#'   named by instance. Returns \code{NULL} if the metric/model is absent.
#'
#' @noRd
.isimip_profiles <- function(output_lists, metric, model) {
  entry <- output_lists[[metric]][[model]]
  if (is.null(entry)) return(NULL)
  if (is.data.frame(entry)) entry <- stats::setNames(list(entry), metric)

  out <- list()
  for (inst in names(entry)) {
    df <- entry[[inst]]
    if (!is.data.frame(df) || ncol(df) < 2 || !"datetime" %in% names(df)) next
    vals <- df[, setdiff(names(df), "datetime"), drop = FALSE]
    depths <- suppressWarnings(as.numeric(sub("^Depth_", "", names(vals))))
    if (anyNA(depths)) next
    mat <- vapply(vals, function(x) suppressWarnings(as.numeric(x)), numeric(nrow(vals)))
    if (!is.matrix(mat)) mat <- matrix(mat, nrow = nrow(vals))
    day <- as.Date(as.POSIXct(df$datetime, tz = "UTC"), tz = "UTC")
    # daily means (ISIMIP daily output), keeping NA where a day has no data
    agg <- rowsum(ifelse(is.na(mat), 0, mat), format(day), reorder = TRUE)
    n <- rowsum(1 * !is.na(mat), format(day), reorder = TRUE)
    agg[n == 0] <- NA
    agg <- agg / pmax(n, 1)
    colnames(agg) <- as.character(depths)
    attr(agg, "depth") <- depths
    out[[inst]] <- agg
  }
  if (length(out) == 0) NULL else out
}

#' @title Add/subtract profile matrices on their common days and depths
#' @noRd
.isimip_combine <- function(add, sub = list()) {
  mats <- c(add, sub)
  days <- Reduce(intersect, lapply(mats, rownames))
  deps <- Reduce(intersect, lapply(mats, colnames))
  if (length(days) == 0 || length(deps) == 0) return(NULL)
  pick <- function(m) m[days, deps, drop = FALSE]
  res <- Reduce(`+`, lapply(add, pick))
  for (m in sub) res <- res - pick(m)
  attr(res, "depth") <- as.numeric(deps)
  res
}

#' @title Write one ISIMIP lake-sector NetCDF file
#' @noRd
.isimip_write_file <- function(path, var, info, mat, longitude, latitude,
                               time_ref, isimip, compression) {
  days <- as.Date(rownames(mat))
  depth <- attr(mat, "depth")
  ord <- order(depth)
  mat <- mat[, ord, drop = FALSE]
  depth <- depth[ord]

  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.double(longitude), longname = "Longitude")
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.double(latitude), longname = "Latitude")
  lev_dim <- ncdf4::ncdim_def("levlak", "-", vals = as.double(seq_along(depth)),
                              longname = "Vertical Water Layer Index")
  time_dim <- ncdf4::ncdim_def("time", paste("days since", time_ref, "00:00:00"),
                               vals = as.double(days - as.Date(time_ref)),
                               calendar = "proleptic_gregorian", longname = "Time")

  fill <- 1e20
  # ncdf4 lists dimensions fastest-varying first: (lon, lat, levlak, time)
  # is written as (time, levlak, lat, lon), as ISIMIP requires
  v_data <- ncdf4::ncvar_def(var, info$units, list(lon_dim, lat_dim, lev_dim, time_dim),
                             missval = fill, longname = info$long_name, prec = "float",
                             compression = as.integer(compression))
  v_depth <- ncdf4::ncvar_def("depth", "m", list(lev_dim), missval = NULL,
                              longname = "Depth of Vertical Layer Center Below Surface",
                              prec = "double")

  if (file.exists(path)) unlink(path)
  nc <- ncdf4::nc_create(path, list(v_data, v_depth), force_v4 = TRUE)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  arr <- array(NA_real_, dim = c(1, 1, length(depth), length(days)))
  arr[1, 1, , ] <- t(mat)
  arr[!is.finite(arr) | abs(arr) > 3.4e38] <- NA_real_
  ncdf4::ncvar_put(nc, v_data, arr)
  ncdf4::ncvar_put(nc, v_depth, depth)

  ncdf4::ncatt_put(nc, var, "standard_name", var)
  ncdf4::ncatt_put(nc, var, "missing_value", fill, prec = "float")
  ncdf4::ncatt_put(nc, "depth", "standard_name", "depth_below_surface")
  ncdf4::ncatt_put(nc, "depth", "positive", "down")
  for (d in list(c("lon", "longitude", "X"), c("lat", "latitude", "Y"), c("time", "time", "T"))) {
    ncdf4::ncatt_put(nc, d[1], "standard_name", d[2])
    ncdf4::ncatt_put(nc, d[1], "axis", d[3])
  }
  ncdf4::ncatt_put(nc, 0, "contact", isimip$contact)
  ncdf4::ncatt_put(nc, 0, "institution", isimip$institution)
  if (nzchar(isimip$comment)) ncdf4::ncatt_put(nc, 0, "comment", isimip$comment)
  invisible(path)
}

#' @title Write cal_metrics() output as ISIMIP lake-sector NetCDF files
#'
#' @description Backend of \code{create_netcdf_output(format = "isimip")}:
#'   one daily, full-profile file per ISIMIP variable and model. See
#'   \code{create_netcdf_output()} for the \code{isimip} settings.
#'
#' @return Invisibly, the paths of the files written.
#'
#' @noRd
.write_isimip_output <- function(output_lists, folder, model, longitude, latitude,
                                 lake_name, wq_config_file, compression, isimip) {
  defaults <- list(forcing = "gswp3-w5e5", bias_adjustment = "", climate_scenario = "obsclim",
                   soc_scenario = "histsoc", sens_scenario = "default",
                   lake = lake_name, model_names = NULL, time_ref = "1901-01-01",
                   contact = "", institution = "", comment = "",
                   variables = names(.isimip_vars),
                   out_dir = file.path(folder, "output", "isimip"))
  unknown <- setdiff(names(isimip), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown 'isimip' setting(s): ", paste(unknown, collapse = ", "),
         ". Allowed: ", paste(names(defaults), collapse = ", "), call. = FALSE)
  }
  isimip <- utils::modifyList(defaults, isimip)
  if (is.null(isimip$lake) || !nzchar(isimip$lake)) {
    stop("ISIMIP output needs a lake name: set isimip = list(lake = ...) or ",
         "location$name in LakeEnsemblR.yaml.", call. = FALSE)
  }
  bad_vars <- setdiff(isimip$variables, names(.isimip_vars))
  if (length(bad_vars) > 0) {
    stop("Unknown ISIMIP variable(s): ", paste(bad_vars, collapse = ", "),
         ". Available: ", paste(names(.isimip_vars), collapse = ", "), call. = FALSE)
  }
  if (!nzchar(isimip$contact) || !nzchar(isimip$institution)) {
    warning("ISIMIP files require 'contact' and 'institution' global attributes; ",
            "set them via isimip = list(contact = ..., institution = ...).", call. = FALSE)
  }

  model <- toupper(model)
  model_names <- .isimip_model_names
  if (!is.null(isimip$model_names)) {
    model_names[toupper(names(isimip$model_names))] <- isimip$model_names
  }

  phyto_groups <- character()
  if (!is.null(wq_config_file) && file.exists(wq_config_file)) {
    phyto_groups <- names(yaml::read_yaml(wq_config_file)$phytoplankton$groups)
  }
  dir.create(isimip$out_dir, showWarnings = FALSE, recursive = TRUE)
  written <- character()
  skipped <- character()

  for (m in model) {
    if (is.na(model_names[m])) {
      stop("No ISIMIP model name for '", m, "'; set isimip$model_names.", call. = FALSE)
    }
    for (var in isimip$variables) {
      res <- .isimip_var_matrices(output_lists, m, var, phyto_groups)
      skipped <- c(skipped, res$skipped)

      for (vname in names(res$matrices)) {
        mat <- res$matrices[[vname]]
        years <- format(as.Date(rownames(mat)), "%Y")
        path <- file.path(isimip$out_dir, .isimip_file_name(
          model_names[[m]], isimip, vname, isimip$lake, min(years), max(years)))
        .isimip_write_file(path, vname, .isimip_vars[[var]], mat, longitude, latitude,
                           isimip$time_ref, isimip, compression)
        written <- c(written, path)
      }
    }
  }

  if (length(skipped) > 0) {
    message("ISIMIP: skipped ", length(skipped), " variable(s) -- enable the metrics in ",
            "Output.yaml to include them:\n  ", paste(skipped, collapse = "\n  "))
  }
  message("ISIMIP: wrote ", length(written), " file(s) to ", isimip$out_dir)
  invisible(written)
}

#' @noRd
.isimip_clean_id <- function(x) gsub("[^a-z0-9-]+", "-", tolower(trimws(x)))

#' @title ISIMIP3 file name
#' @noRd
.isimip_file_name <- function(model_name, isimip, var, region, start, end) {
  ids <- c(model_name, isimip$forcing, isimip$bias_adjustment, isimip$climate_scenario,
           isimip$soc_scenario, isimip$sens_scenario, var, region, "daily", start, end)
  ids <- vapply(ids[nzchar(ids)], .isimip_clean_id, character(1))
  paste0(paste(ids, collapse = "_"), ".nc")
}

#' @title Daily profile matrices of one ISIMIP variable for one model
#'
#' @description Builds the variable from its metrics (sums/differences, see
#'   \code{.isimip_vars}), converted to ISIMIP units. Group metrics
#'   (phyto-/zooplankton) are summed over groups; \code{phytobio} also gets
#'   one matrix per group, named \code{phytobio-<group>}.
#'
#' @return A list with \code{matrices} (named list; rows = days, cols =
#'   depths) and \code{skipped} (character; reasons, for messages).
#'
#' @noRd
.isimip_var_matrices <- function(output_lists, m, var, phyto_groups) {
  info <- .isimip_vars[[var]]
  divisor <- if (m %in% names(info$to_si_model)) info$to_si_model[[m]] else info$to_si
  add <- lapply(info$add, function(x) .isimip_profiles(output_lists, x, m))
  subs <- lapply(info$sub, function(x) .isimip_profiles(output_lists, x, m))
  absent <- c(info$add, info$sub)[vapply(c(add, subs), is.null, logical(1))]
  if (length(absent) > 0) {
    return(list(matrices = list(),
                skipped = sprintf("%s/%s (missing %s)", m, var, paste(absent, collapse = ", "))))
  }

  out <- list()
  out[[var]] <- .isimip_combine(lapply(add, .isimip_combine), lapply(subs, .isimip_combine))
  if (isTRUE(info$groups)) {
    for (inst in names(add[[1]])) {
      g <- phyto_groups[vapply(phyto_groups, function(p) grepl(p, inst, fixed = TRUE), logical(1))]
      g <- if (length(g) > 0) g[which.max(nchar(g))] else
        if (length(phyto_groups) == 1) phyto_groups else sub(paste0("^", info$add, "_?"), "", inst)
      out[[paste0(var, "-", .isimip_clean_id(g))]] <- add[[1]][[inst]]
    }
  }

  empty <- names(out)[vapply(out, is.null, logical(1))]
  out <- lapply(out[!names(out) %in% empty], function(x) x / divisor)
  list(matrices = out,
       skipped = if (length(empty) > 0) sprintf("%s/%s (no common days/depths)", m, empty) else character())
}
