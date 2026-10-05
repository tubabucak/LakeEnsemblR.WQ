#' Write ISIMIP3 global lake-sector NetCDF files for many lakes
#'
#' Writes water-quality output of many LakeEnsemblR.WQ lake runs into
#' ISIMIP3 \code{lakes_global} files: each lake goes into its cell of the
#' global 0.5 degree grid (360 x 720; latitude 89.75 to -89.75, longitude
#' -179.75 to 179.75), all other cells are 1e20. There is one file per
#' variable, model and decade (\code{1901_1910}, \code{1911_1920}, ...; the
#' first and last period end/start at the simulation years), daily,
#' NETCDF4_CLASSIC (see below) with compression level 5, float32 data, double
#' coordinates, \code{_FillValue} and \code{missing_value} 1e20.
#'
#' The water-quality variables have dimensions \code{(time, levlak, lat,
#' lon)} with two levels: \code{levlak = 1} is the mean over the
#' epilimnion and \code{levlak = 2} the mean over the hypolimnion. They are
#' split at the daily thermocline from the lake's temperature profile
#' (\code{rLakeAnalyzer::thermo.depth()}, as used for the stratification
#' metrics); when the lake is mixed (no thermocline), both levels hold the
#' mean over the whole water column. Means are over the model layers in
#' the \code{cal_metrics()} output. See \code{create_netcdf_output()} for the
#' variables and how they are derived (\code{format = "isimip"}).
#'
#' Lakes are processed one at a time and written straight into the files, so
#' memory use does not grow with the number of lakes.
#'
#' File format: R's NetCDF packages cannot write compressed NETCDF4_CLASSIC
#' files, so the files are written as compressed NETCDF4 (within the classic
#' data model) and then converted to NETCDF4_CLASSIC with \code{nccopy} from
#' the netCDF tools, if it is on the PATH. Otherwise a message lists how to
#' convert them (\code{nccopy -k nc7 -d 5} or \code{cdo -f nc4c -z zip_5 copy}).
#'
#' @param lakes data.frame; one row per lake with columns \code{lat} and
#'   \code{lon} (decimal degrees; snapped to the 0.5 degree grid cell they
#'   fall in -- one lake per cell) and \code{folder} (the lake's
#'   LakeEnsemblR.WQ project folder; \code{cal_metrics()} is run there) and/or
#'   \code{rds} (path to a saved \code{cal_metrics()} result, used instead of
#'   running \code{cal_metrics()}). An optional \code{id} column is used in
#'   messages.
#' @param model character; models to write, e.g. \code{c("GLM", "SIMSTRAT")}.
#' @param metric_yaml_file character; metrics file inside each lake folder.
#'   It must enable \code{Temp_degreeCelcius} (for the thermocline) and the
#'   water-quality metrics to be written.
#' @param wq_config_file character; LakeEnsemblR_WQ config file inside each
#'   lake folder (for \code{cal_metrics()} and the phytoplankton group names).
#' @param start_year,end_year integer; period to write. If \code{NULL}, taken
#'   from \code{time} in the first lake's LakeEnsemblR.yaml.
#' @param phyto_groups character or \code{NULL}; phytoplankton group names
#'   for the \code{phytobio-<group>} files. If \code{NULL}, collected from the
#'   lakes' WQ config files.
#' @param out_dir character; output directory.
#' @param isimip list; file-name parts and attributes, as for
#'   \code{create_netcdf_output(format = "isimip")}: \code{forcing},
#'   \code{bias_adjustment}, \code{climate_scenario}, \code{soc_scenario},
#'   \code{sens_scenario}, \code{model_names}, \code{time_ref}
#'   (\code{"1901-01-01"} for ISIMIP3a, \code{"1601-01-01"} for ISIMIP3b),
#'   \code{contact}, \code{institution}, \code{comment} and \code{variables}.
#' @param verbose logical; print progress per lake.
#'
#' @return Invisibly, the paths of the files written. Files that would hold
#'   no data at all (e.g. a variable none of the lakes has) are not kept.
#'
#' @examples
#' \dontrun{
#' lakes <- data.frame(id = c("ravn", "mendota"),
#'                     lat = c(56.1, 43.1), lon = c(9.8, -89.4),
#'                     folder = c("runs/ravn", "runs/mendota"))
#' create_isimip_global(lakes, model = c("GLM", "SIMSTRAT"),
#'                      isimip = list(contact = "Name <mail>",
#'                                    institution = "Aarhus University"))
#' }
#' @export
create_isimip_global <- function(lakes,
                                 model,
                                 metric_yaml_file = "Output.yaml",
                                 wq_config_file = "LakeEnsemblR_WQ.yaml",
                                 start_year = NULL,
                                 end_year = NULL,
                                 phyto_groups = NULL,
                                 out_dir = "isimip_global",
                                 isimip = list(),
                                 verbose = TRUE) {
  if (!requireNamespace("RNetCDF", quietly = TRUE)) {
    stop("Package 'RNetCDF' is required to write NETCDF4_CLASSIC files for ISIMIP.", call. = FALSE)
  }
  lakes <- as.data.frame(lakes, stringsAsFactors = FALSE)
  if (!all(c("lat", "lon") %in% names(lakes)) || !any(c("folder", "rds") %in% names(lakes))) {
    stop("'lakes' needs columns lat, lon and folder and/or rds.", call. = FALSE)
  }
  if (is.null(lakes$id)) lakes$id <- if (!is.null(lakes$folder)) basename(lakes$folder) else seq_len(nrow(lakes))
  model <- toupper(model)

  defaults <- list(forcing = "gswp3-w5e5", bias_adjustment = "", climate_scenario = "obsclim",
                   soc_scenario = "histsoc", sens_scenario = "default", model_names = NULL,
                   time_ref = "1901-01-01", contact = "", institution = "", comment = "",
                   variables = names(.isimip_vars))
  unknown <- setdiff(names(isimip), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown 'isimip' setting(s): ", paste(unknown, collapse = ", "),
         ". Allowed: ", paste(names(defaults), collapse = ", "), call. = FALSE)
  }
  isimip <- utils::modifyList(defaults, isimip)
  if (!nzchar(isimip$contact) || !nzchar(isimip$institution)) {
    warning("ISIMIP files require 'contact' and 'institution' global attributes; ",
            "set them via isimip = list(contact = ..., institution = ...).", call. = FALSE)
  }
  model_names <- .isimip_model_names
  if (!is.null(isimip$model_names)) model_names[toupper(names(isimip$model_names))] <- isimip$model_names
  if (anyNA(model_names[model])) {
    stop("No ISIMIP model name for: ", paste(model[is.na(model_names[model])], collapse = ", "),
         "; set isimip$model_names.", call. = FALSE)
  }

  # Grid cells (one lake per cell)
  lakes$lat_i <- .isimip_grid_index(lakes$lat, 90)
  lakes$lon_i <- .isimip_grid_index(lakes$lon, 180)
  cell <- paste(lakes$lat_i, lakes$lon_i)
  if (anyDuplicated(cell)) {
    dup <- lakes$id[cell %in% cell[duplicated(cell)]]
    stop("More than one lake in the same 0.5 degree grid cell: ", paste(dup, collapse = ", "),
         ". ISIMIP lakes_global has one representative lake per cell.", call. = FALSE)
  }

  has_folder <- !is.null(lakes$folder)
  if (is.null(phyto_groups) && has_folder) {
    phyto_groups <- unique(unlist(lapply(file.path(lakes$folder, wq_config_file), function(f) {
      if (file.exists(f)) names(yaml::read_yaml(f)$phytoplankton$groups) else NULL
    })))
  }
  if (is.null(phyto_groups)) phyto_groups <- character()

  if (is.null(start_year) || is.null(end_year)) {
    ler <- if (has_folder) file.path(lakes$folder[1], "LakeEnsemblR.yaml") else ""
    if (!file.exists(ler)) {
      stop("Give start_year and end_year (no LakeEnsemblR.yaml found to read them from).", call. = FALSE)
    }
    ler_time <- yaml::read_yaml(ler)$time
    if (is.null(start_year)) start_year <- as.integer(substr(ler_time$start, 1, 4))
    if (is.null(end_year)) end_year <- as.integer(substr(ler_time$stop, 1, 4))
  }
  periods <- .isimip_periods(start_year, end_year)

  var_names <- unlist(lapply(isimip$variables, function(v) {
    if (isTRUE(.isimip_vars[[v]]$groups)) c(v, paste0(v, "-", .isimip_clean_id(phyto_groups))) else v
  }))
  base_var <- sub("-.*$", "", var_names)
  names(base_var) <- var_names

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  kept <- character()
  converted <- character()
  nccopy <- Sys.which("nccopy")
  lat_vals <- seq(89.75, -89.75, by = -0.5)
  lon_vals <- seq(-179.75, 179.75, by = 0.5)

  for (m in model) {
    # Create this model's files (empty: all 1e20) and keep them open
    files <- list()
    for (vn in var_names) for (p in seq_len(nrow(periods))) {
      path <- file.path(out_dir, .isimip_file_name(model_names[[m]], isimip, vn, "global",
                                                   periods$start[p], periods$end[p]))
      days <- seq(as.Date(paste0(periods$start[p], "-01-01")),
                  as.Date(paste0(periods$end[p], "-12-31")), by = "day")
      files[[paste(vn, p)]] <- list(
        nc = .isimip_global_create(path, vn, .isimip_vars[[base_var[[vn]]]], days,
                                   lat_vals, lon_vals, isimip),
        path = path, days = days, n_written = 0L)
    }

    for (k in seq_len(nrow(lakes))) {
      lk <- lakes[k, ]
      metrics <- tryCatch(.isimip_lake_metrics(lk, m, metric_yaml_file, wq_config_file),
                          error = function(e) e)
      if (inherits(metrics, "error")) {
        warning("Lake ", lk$id, " / ", m, ": skipped (", conditionMessage(metrics), ")", call. = FALSE)
        next
      }
      temp <- .isimip_profiles(metrics, "Temp_degreeCelcius", m)
      if (is.null(temp)) {
        warning("Lake ", lk$id, " / ", m, ": skipped (no Temp_degreeCelcius for the thermocline; ",
                "enable it in ", metric_yaml_file, ")", call. = FALSE)
        next
      }
      thermo <- .isimip_thermocline(temp[[1]])

      n_vars <- 0L
      for (v in unique(base_var)) {
        res <- .isimip_var_matrices(metrics, m, v, phyto_groups)
        for (vn in intersect(names(res$matrices), var_names)) {
          eh <- .isimip_epi_hypo(res$matrices[[vn]], thermo)
          for (p in seq_len(nrow(periods))) {
            f <- files[[paste(vn, p)]]
            vals <- matrix(NA_real_, nrow = 2, ncol = length(f$days))
            hit <- match(as.character(f$days), rownames(eh))
            vals[, !is.na(hit)] <- t(eh[hit[!is.na(hit)], , drop = FALSE])
            if (all(is.na(vals))) next
            vals[is.na(vals) | !is.finite(vals)] <- 1e20
            RNetCDF::var.put.nc(f$nc, vn, array(vals, dim = c(1, 1, 2, length(f$days))),
                                start = c(lk$lon_i, lk$lat_i, 1, 1),
                                count = c(1, 1, 2, length(f$days)))
            files[[paste(vn, p)]]$n_written <- f$n_written + 1L
          }
          n_vars <- n_vars + 1L
        }
      }
      if (verbose) message("ISIMIP global: ", m, " lake ", k, "/", nrow(lakes), " (", lk$id, "): ",
                           n_vars, " variable(s) written")
    }

    for (f in files) {
      RNetCDF::close.nc(f$nc)
      if (f$n_written == 0L) unlink(f$path) else kept <- c(kept, f$path)
    }
    if (nzchar(nccopy)) for (p in kept[!kept %in% converted]) {
      if (.isimip_to_classic(p, nccopy)) converted <- c(converted, p)
    }
  }

  message("ISIMIP global: wrote ", length(kept), " file(s) to ", out_dir)
  if (length(converted) < length(kept)) {
    message("ISIMIP global: ", length(kept) - length(converted), " file(s) are NETCDF4, not ",
            "NETCDF4_CLASSIC as ISIMIP requires", if (!nzchar(nccopy)) " (nccopy not found on PATH)",
            ". Convert them with e.g.\n  nccopy -k nc7 -d 5 in.nc out.nc   or   ",
            "cdo -f nc4c -z zip_5 copy in.nc out.nc")
  }
  invisible(kept)
}

#' @title Convert a NETCDF4 file to NETCDF4_CLASSIC (compression 5) with nccopy
#' @return \code{TRUE} if converted, \code{FALSE} (with a warning) otherwise.
#' @noRd
.isimip_to_classic <- function(path, nccopy) {
  tmp <- paste0(path, ".tmp")
  status <- suppressWarnings(system2(nccopy, c("-k", "nc7", "-d", "5", shQuote(path), shQuote(tmp)),
                                     stdout = FALSE, stderr = FALSE))
  if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
    unlink(tmp)
    warning("nccopy could not convert ", basename(path), " to NETCDF4_CLASSIC.", call. = FALSE)
    return(FALSE)
  }
  file.rename(tmp, path)
}

#' @title 1-based index of the 0.5 degree cell containing x
#' @description Latitude cells run north to south (89.75 first), longitude
#'   cells west to east (-179.75 first), as in ISIMIP3 global files.
#' @noRd
.isimip_grid_index <- function(x, half_range) {
  x <- as.numeric(x)
  if (any(is.na(x) | abs(x) > half_range)) {
    stop("Coordinates must be within +-", half_range, " degrees.", call. = FALSE)
  }
  centre <- pmin(floor(x / 0.5) * 0.5 + 0.25, half_range - 0.25)
  if (half_range == 90) round((89.75 - centre) / 0.5) + 1 else round((centre + 179.75) / 0.5) + 1
}

#' @title ISIMIP decade periods, e.g. 1901-1910, ..., 2011-2019
#' @noRd
.isimip_periods <- function(start_year, end_year) {
  if (end_year < start_year) stop("end_year is before start_year.", call. = FALSE)
  years <- start_year:end_year
  ends <- unique(c(years[years %% 10 == 0], end_year))
  starts <- c(start_year, utils::head(ends, -1) + 1)
  data.frame(start = starts, end = ends)
}

#' @title cal_metrics() output for one lake and model
#' @noRd
.isimip_lake_metrics <- function(lake, model, metric_yaml_file, wq_config_file) {
  if (!is.null(lake$rds) && !is.na(lake$rds) && nzchar(lake$rds)) {
    return(readRDS(lake$rds))
  }
  if (!dir.exists(lake$folder)) stop("folder not found: ", lake$folder)
  old <- setwd(lake$folder)
  on.exit(setwd(old), add = TRUE)
  res <- NULL
  utils::capture.output(res <- suppressWarnings(suppressMessages(
    cal_metrics(metric_yaml_file = metric_yaml_file, model_filter = model,
                wq_config_file = wq_config_file))))
  res
}

#' @title Daily thermocline depth from a temperature profile matrix
#' @return Named numeric (by day): depth, \code{NaN} when mixed, \code{NA}
#'   when the profile has too few values.
#' @noRd
.isimip_thermocline <- function(temp) {
  depths <- attr(temp, "depth")
  th <- vapply(seq_len(nrow(temp)), function(i) {
    ok <- !is.na(temp[i, ])
    if (sum(ok) < 3) return(NA_real_)
    td <- rLakeAnalyzer::thermo.depth(temp[i, ok], depths[ok])
    if (length(td) == 0 || is.na(td)) NaN else as.numeric(td)
  }, numeric(1))
  stats::setNames(th, rownames(temp))
}

#' @title Epilimnion / hypolimnion means of a daily profile matrix
#' @description Layers at or above the thermocline are epilimnion, below it
#'   hypolimnion; a mixed day (thermocline \code{NaN}) gives the
#'   whole-column mean in both. Days without a temperature profile are NA.
#' @return Matrix (days x 2: epi, hypo), rownames = days.
#' @noRd
.isimip_epi_hypo <- function(mat, thermo) {
  depths <- attr(mat, "depth")
  th <- thermo[rownames(mat)]
  mixed <- is.nan(th)
  unknown <- is.na(th) & !mixed
  th_num <- ifelse(is.na(th), 0, th)
  epi_mask <- outer(th_num, depths, function(t, d) d <= t)
  hypo_mask <- !epi_mask
  epi_mask[mixed, ] <- TRUE
  hypo_mask[mixed, ] <- TRUE
  layer_mean <- function(mask) {
    v <- mat
    v[!mask] <- NA
    out <- rowMeans(v, na.rm = TRUE)
    out[is.nan(out)] <- NA
    out
  }
  res <- cbind(epi = layer_mean(epi_mask), hypo = layer_mean(hypo_mask))
  res[unknown, ] <- NA
  rownames(res) <- rownames(mat)
  res
}

#' @title Create one empty ISIMIP global file (NETCDF4_CLASSIC) and leave it open
#' @noRd
.isimip_global_create <- function(path, var, info, days, lat_vals, lon_vals, isimip) {
  if (file.exists(path)) unlink(path)
  # RNetCDF ignores chunking/compression for "classic4" (the full grid would be
  # stored uncompressed, GBs per file), so write NETCDF4 within the classic
  # data model and convert with nccopy afterwards (see .isimip_to_classic())
  nc <- RNetCDF::create.nc(path, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "lon", length(lon_vals))
  RNetCDF::dim.def.nc(nc, "lat", length(lat_vals))
  RNetCDF::dim.def.nc(nc, "levlak", 2)
  RNetCDF::dim.def.nc(nc, "time", length(days))

  .att <- function(v, ...) {
    a <- list(...)
    for (n in names(a)) RNetCDF::att.put.nc(nc, v, n, "NC_CHAR", a[[n]])
  }
  RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
  .att("lon", standard_name = "longitude", long_name = "Longitude", units = "degrees_east", axis = "X")
  RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
  .att("lat", standard_name = "latitude", long_name = "Latitude", units = "degrees_north", axis = "Y")
  RNetCDF::var.def.nc(nc, "levlak", "NC_DOUBLE", "levlak")
  .att("levlak", long_name = "Vertical Water Layer Index", units = "-",
       comment = paste("1 = mean epilimnion, 2 = mean hypolimnion; split at the daily",
                       "thermocline, whole-column mean in both when mixed"))
  RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
  .att("time", standard_name = "time", long_name = "Time",
       units = paste("days since", isimip$time_ref, "00:00:00"),
       calendar = "proleptic_gregorian", axis = "T")

  # (lon, lat, levlak, time) fastest-first = (time, levlak, lat, lon) in CDL;
  # one chunk per lake cell and file, so each lake is a single chunk write
  RNetCDF::var.def.nc(nc, var, "NC_FLOAT", c("lon", "lat", "levlak", "time"),
                      chunking = TRUE, chunksizes = c(1, 1, 2, length(days)), deflate = 5)
  RNetCDF::att.put.nc(nc, var, "_FillValue", "NC_FLOAT", 1e20)
  RNetCDF::att.put.nc(nc, var, "missing_value", "NC_FLOAT", 1e20)
  .att(var, standard_name = var, long_name = info$long_name, units = info$units)

  .att("NC_GLOBAL", contact = isimip$contact, institution = isimip$institution,
       creation_date = format(Sys.Date(), "%Y-%m-%d"))
  if (nzchar(isimip$comment)) .att("NC_GLOBAL", comment = isimip$comment)

  RNetCDF::var.put.nc(nc, "lon", lon_vals)
  RNetCDF::var.put.nc(nc, "lat", lat_vals)
  RNetCDF::var.put.nc(nc, "levlak", c(1, 2))
  RNetCDF::var.put.nc(nc, "time", as.numeric(days - as.Date(isimip$time_ref)))
  nc
}
