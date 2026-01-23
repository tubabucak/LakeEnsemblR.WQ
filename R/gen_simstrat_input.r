
#' Extract active AED2 modules from an \code{aed2.nml} file
#'
#' This helper reads an AED2 namelist file (typically \code{aed2.nml})
#' and parses the \code{&aed2_models} block to determine which AED2
#' modules are activated. The returned character vector can be used to
#' filter [aed2_inflow_map] and to decide which Simstrat–AED2 inflow
#' files need to be created.
#'
#' @param aed2_file Character string; path to \code{aed2.nml}.
#'
#' @return A character vector with the names of the active AED2 modules,
#'   e.g. \code{c("aed2_carbon", "aed2_nitrogen", "aed2_phytoplankton")}.
#'
#' @examples
#' \dontrun{
#' active <- get_active_aed2_modules("aed2.nml")
#' active
#' }
#'
#' @seealso  [generate_simstrat_aed2_inflows()]
#' @keywords internal

get_active_aed2_modules <- function(aed2_file) {
  lines <- readLines(aed2_file)
  
  start <- grep("^\\s*&aed2_models", lines)
  if (length(start) == 0) {
    stop("Could not find &aed2_models block in ", aed2_file)
  }
  
  # find the end of this namelist (first '/' after start)
  end <- grep("^\\s*/\\s*$", lines)
  end <- end[end > start][1]
  
  block <- paste(lines[start:end], collapse = " ")
  
  # pull out the models = 'a','b','c' ... part
  models_str <- sub(".*models\\s*=\\s*", "", block)
  models_str <- sub("/.*$", "", models_str)         # drop after '/'
  models_str <- gsub("'", "", models_str)           # remove quotes
  
  mods <- strsplit(models_str, ",")[[1]]
  mods <- trimws(mods)
  mods[mods != ""]
}

#' Extract phytoplankton group names from \code{aed2_phyto_pars.nml}
#'
#' This helper reads the AED2 phytoplankton parameter file
#' (typically \code{aed2_phyto_pars.nml}) and extracts the phytoplankton
#' group names from the \code{pd\%p_name} line. These names are then used
#' to construct Simstrat–AED2 inflow file names for phytoplankton
#' variables, replacing the \code{"XX"} placeholder in entries such as
#' \code{"PHY_XX_inflow"} 
#'
#' By default, names are "sanitized" for safe use in file names:
#' converted to lower case and with non–alphanumeric characters
#' replaced by underscores.
#'
#' @param phyto_pars_file Character string; path to
#'   \code{aed2_phyto_pars.nml}.
#' @param sanitize Logical; if \code{TRUE} (default), return
#'   file–name–safe versions of the group names (lowercase, with
#'   non–alphanumeric characters replaced by underscores). If
#'   \code{FALSE}, the raw names from \code{pd\%p_name} are returned.
#'
#' @return A character vector of phytoplankton group names (possibly
#'   sanitized), e.g. \code{c("diatoms", "cyanobacteria",
#'   "some_random_group")}.
#'
#' @examples
#' \dontrun{
#' get_phyto_names("aed2_phyto_pars.nml")
#' }
#'
#' @seealso [generate_simstrat_aed2_inflows()]
#' @keywords internal

get_phyto_names <- function(phyto_pars_file, sanitize = TRUE) {
  lines <- readLines(phyto_pars_file)
  
  # Find the line with pd%p_name
  idx <- grep("pd%p_name", lines)
  if (length(idx) == 0) {
    stop("Could not find 'pd%p_name' in ", phyto_pars_file)
  }
  
  line <- lines[idx[1]]
  
  # Keep the part after '='
  rhs <- sub(".*=", "", line)  # Remove trailing '/' or commas at the end
  rhs <- sub("/.*$", "", rhs)
  
  # Remove single quotes and split on commas
  rhs <- gsub("'", "", rhs)
  names_vec <- strsplit(rhs, ",")[[1]]
  names_vec <- trimws(names_vec)
  names_vec <- names_vec[names_vec != ""]
  
  if (!sanitize) return(names_vec)
  
  # Sanitize for file names: lower case, non-alnum -> "_"
  safe <- tolower(names_vec)
  safe <- gsub("[^0-9A-Za-z]+", "_", safe)
  safe <- gsub("^_+|_+$", "", safe)  # trim leading/trailing "_"
  
  safe
}

#' Format a Simstrat-style AED2 inflow file (character vector)
#'
#' This helper creates the contents of a Simstrat inflow file for a single
#' AED2 variable, given a time axis and simple inflow geometry (deep vs
#' surface inflows). It mirrors the structure of \code{format_flow_simstrat()}
#' used for Q/T/S inflows, but is applied to AED2 tracers.
#'
#' @param varname Character; AED2 inflow variable name, e.g. \code{"PHS_frp_inflow"}.
#' @param levels Numeric vector; inflow depths (m), one per branch.
#' @param surf_flow Logical vector; same length as \code{levels}, TRUE for
#'   surface inflows, FALSE for deep inflows.
#' @param sim_par Character; path to \code{simstrat.par}-style JSON config.
#' @param times Numeric vector of times [days] for the template time series.
#'   If \code{NULL}, \code{Simulation$"Start d"} and \code{Simulation$"End d"}
#'   are used with a daily step.
#' @param default_value Numeric; default value to assign at all depths and times
#'   (e.g. 0 for most tracers, 8 for \code{CAR_pH_inflow}).
#' @param unit Character; unit string to include in the header, e.g.
#'   \code{"millimolesPerMeterCubed"} or \code{"pH"}.
#'
#' @return A character vector of lines to be written to a \code{.dat} file.
#' @keywords internal
#'
#' @seealso [generate_simstrat_aed2_inflows()]


format_aed_inflow_simstrat <- function(varname,
                                       levels,
                                       surf_flow,
                                       sim_par = "simstrat.par",
                                       times = NULL,
                                       default_value = 0,
                                       unit = "millimolesPerMeterCubed") {
  inflow_mode <- get_json_value(sim_par, "ModelConfig", "InflowMode")
  
  # If we only want an "off" file, mimic inflow_mode 0
  if (is.null(times)) {
    start_sim <- get_json_value(sim_par, "Simulation", "Start d")
    end_sim   <- get_json_value(sim_par, "Simulation", "End d")
    times     <- seq(start_sim, end_sim, by = 1)
  }
  
  # ---- header ----
  line1 <- sprintf("Time [d]\t%s [%s]", varname, unit)
  
  if (inflow_mode == 0L) {
    line2 <- "1"
    line3 <- "-1\t0.00"
    start_sim <- get_json_value(sim_par, "Simulation", "Start d")
    end_sim   <- get_json_value(sim_par, "Simulation", "End d")
    
    line4 <- paste0(start_sim, "\t", 0.000)
    line5 <- paste0(end_sim, "\t", 0.000)
    
    return(c(line1, line2, line3, line4, line5))
  }
  
  # ---- geometry for InflowMode = 1 ----
  num_deep_flows <- length(levels) - sum(surf_flow)
  num_surf_flows <- sum(surf_flow)
  
  line2 <- paste0(num_deep_flows * 3, "\t", ifelse(num_surf_flows == 0L, 0, 2))
  
  line3 <- "-1"
  for (i in levels[!surf_flow]) {
    line3 <- paste(line3, i - 1, i, i + 1, sep = "\t")
  }
  if (any(surf_flow)) {
    line3 <- paste(line3, "-1.00\t0.00", sep = "\t")
  }
  
  # ---- values ----
  # For now, just zeros everywhere
  n_branches <- num_deep_flows * 3 + ifelse(num_surf_flows == 0L, 0, 2)
  # But for AED, you might also want 1 value per depth, not *3* — this part
  # would need to match exactly how Simstrat expects AED2 tracers per mode.
  
  # Here I'll keep it conceptually simple: 1 value per column in geometry
  # i.e. n_cols = length(depth entries), but you'll need to confirm with docs.
  
  vals_per_time <- rep(default_value, times = n_branches)
  lines <- character(length(times))
  
  for (k in seq_along(times)) {
    lines[k] <- paste(c(times[k], vals_per_time), collapse = "\t")
  }
  
  c(line1, line2, line3, lines)
}




#' Generate Simstrat–AED2 inflow files for active modules
#'
#' Creates template Simstrat inflow \code{.dat} files for all AED2 variables
#' associated with the active modules in \code{aed2_file}. Phytoplankton
#' variables are expanded based on group names in \code{aed2_phyto_pars.nml}.
#'
#' By default, all inflows are treated as surface inflows at 0 m, and all
#' variables are initialised with \code{default_value}, except
#' \code{CAR_pH_inflow}, which is initialised to 8 with unit \code{"pH"}.
#'
#' @param aed2_file Character; path to \code{aed2.nml}.
#' @param phyto_pars_file Character; path to \code{aed2_phyto_pars.nml}.
#'   Required if \code{aed2_phytoplankton} is active.
#' @param sim_par Character; path to \code{simstrat.par}-style JSON config.
#' @param out_dir Character; directory to which inflow \code{.dat} files
#'   will be written.
#' @param inflow_map Data frame mapping AED2 modules to inflow variable base
#'   names; defaults to [aed2_inflow_map].
#' @param levels Numeric vector; inflow depths (m). If \code{NULL} together
#'   with \code{surf_flow}, a single surface inflow at 0 m is assumed.
#' @param surf_flow Logical vector; TRUE for surface inflows, FALSE for deep
#'   inflows. Must be the same length as \code{levels}.
#' @param default_value Numeric; default initial inflow value (0 for most
#'   tracers).
#' @param unit Character; default unit string to use for non-pH variables.
#'
#' @return Invisibly returns the character vector of inflow variable names
#'   for which files were written (without \code{.dat} extension).
#' @export
generate_simstrat_aed2_inflows <- function(aed2_file,
                                           phyto_pars_file = NULL,
                                           sim_par,
                                           out_dir,
                                           levels = NULL,
                                           surf_flow = NULL,
                                           default_value = 0,
                                           unit = "millimolesPerMeterCubed", overwrite= FALSE) {
  
  
  
  inflow_map <- data.frame(
    module = c(
      "aed2_carbon","aed2_carbon","aed2_carbon","aed2_carbon",
      "aed2_noncohesive",
      "aed2_nitrogen","aed2_nitrogen","aed2_nitrogen","aed2_nitrogen",
      "aed2_organic_matter","aed2_organic_matter",
      "aed2_phosphorus","aed2_phosphorus","aed2_phosphorus",
      "aed2_oxygen",
      "aed2_silica",
      "aed2_phytoplankton","aed2_phytoplankton","aed2_phytoplankton",
      "aed2_zooplankton"
    ),
    inflow_var = c(
      "CAR_ch4_bub_inflow",
      "CAR_ch4_inflow",
      "CAR_dic_inflow",
      "CAR_pH_inflow",
      "NCS_ss1_inflow",
      "NIT_amm_inflow",
      "NIT_nit_inflow",
      "OGM_pon_inflow",
      "OGM_don_inflow",
      "OGM_doc_inflow",
      "OGM_poc_inflow",
      "OGM_dop_inflow",
      "OGM_pop_inflow",
      "PHS_frp_inflow",
      "OXY_oxy_inflow",
      "SIL_rsi_inflow",
      "PHY_XX_inflow",
      "PHY_XX_IN_inflow",
      "PHY_XX_IP_inflow",
      "ZOO_"
    ),
    stringsAsFactors = FALSE
  )
  
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ---- 1) Time axis from simstrat.par ----
  start_sim <- get_json_value(sim_par, "Simulation", "Start d")
  end_sim   <- get_json_value(sim_par, "Simulation", "End d")
  times     <- seq(start_sim, end_sim, by = 1)
  
  # ---- 2) Default geometry: all surface inflow at 0 m ----
  if (is.null(levels) || is.null(surf_flow)) {
    levels    <- 0      # one surface branch
    surf_flow <- TRUE
  }
  
  # ---- 3) Determine inflow_vars as you already do (active modules + phyto names) ----
  active_modules <- get_active_aed2_modules(aed2_file)
  map_active <- inflow_map[inflow_map$module %in% active_modules, , drop = FALSE]
  
  phyto_rows <- map_active$module == "aed2_phytoplankton"
  map_phyto  <- map_active[phyto_rows, , drop = FALSE]
  map_other  <- map_active[!phyto_rows, , drop = FALSE]
  
  inflow_vars <- character(0)
  inflow_vars <- c(inflow_vars, map_other$inflow_var)
  
  if (nrow(map_phyto) > 0) {
    if (is.null(phyto_pars_file)) {
      stop("aed2_phytoplankton is active, but 'phyto_pars_file' was not provided.")
    }
    
    phyto_names <- get_phyto_names(phyto_pars_file, sanitize = TRUE)
    if (length(phyto_names) == 0) {
      warning("No phytoplankton names found in ", phyto_pars_file,
              "; no phyto inflow files will be created.")
    } else {
      for (tmpl in map_phyto$inflow_var) {
        for (nm in phyto_names) {
          inflow_vars <- c(
            inflow_vars,
            gsub("XX", nm, tmpl, fixed = TRUE)
          )
        }
      }
    }
  }
  
  inflow_vars <- unique(inflow_vars)
  
  # Write one .dat file per AED inflow variable
  for (varname in inflow_vars) {
    out_file <- file.path(out_dir, paste0(varname, ".dat"))
    
    # --- HERE is the special rule for pH ---
    var_default <- if (identical(varname, "CAR_pH_inflow")) 8 else default_value
    var_unit    <- if (identical(varname, "CAR_pH_inflow")) "pH" else unit
    # ---------------------------------------
    
    # Do not overwrite if file exists 
    if (!overwrite && file.exists(out_file)) next
    lines <- format_aed_inflow_simstrat(
      varname       = varname,
      levels        = levels,
      surf_flow     = surf_flow,
      sim_par       = sim_par,
      times         = times,
      default_value = var_default,
      unit          = var_unit
    )
    
    writeLines(lines, out_file)
  }
  
  invisible(inflow_vars)
}
  
  
  
  
  


