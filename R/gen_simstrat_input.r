


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
  end <- which(lines == "/")
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
  rhs <- sub(".*=", "", line)
  # Remove trailing '/' or commas at the end
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

#' Generate empty Simstrat–AED2 inflow files for active modules
#'
#' This function creates template inflow files (\code{.dat}) for
#' Simstrat–AED2 based on the AED2 configuration and phytoplankton
#' parameter files. It is intended as a helper for automatically
#' generating "empty" (typically zero) inflow time series for those
#' AED2 state variables that require inflow forcing when the module is
#' activated.
#'
#' The function:
#' \enumerate{
#'   \item Parses the AED2 namelist (\code{aed2_file}) via
#'     [get_active_aed2_modules()] to determine which modules are active.
#'   \item Filters [aed2_inflow_map] to retain only inflow variables
#'     associated with the active modules.
#'   \item For \code{aed2_phytoplankton}, replaces the \code{"XX"}
#'     placeholder in inflow variable names (e.g. \code{"PHY_XX_inflow"})
#'     with the phytoplankton group names extracted from
#'     \code{phyto_pars_file} via [get_phyto_names()].
#'   \item Writes one \code{.dat} file per inflow variable in
#'     \code{out_dir}, with a header of the form
#'     \code{"Time [d]\\t<var> [millimolesPerMeterCubed]"} and a
#'    simple template time series defined by \code{times} and
#'     \code{default_value}.
#' }
#'
#' These template inflow files can then either be left as zero
#' (representing no inflow) or filled with modelled / observed inflow
#' time series in a separate step. - to be decided later-
#'
#' @param aed2_file Character string; path to \code{aed2.nml}.
#' @param phyto_pars_file Character string; path to
#'   \code{aed2_phyto_pars.nml}. Required if \code{aed2_phytoplankton}
#'   is active in \code{aed2_file}.
#' @param out_dir Character string; directory in which to write inflow
#'   \code{.dat} files. The directory is created if it does not exist.
#' @param inflow_map A data frame mapping AED2 modules to inflow
#'   variable base names; defaults to [aed2_inflow_map].
#' @param times Numeric vector of time values (in days) to use for the
#'   template time series.
#' @param default_value Numeric; default value to assign to all time
#'   points in the template time series (typically \code{0} for "no
#'   inflow").
#'
#' @return Invisibly returns a character vector with the inflow
#'   variable names for which files were created (without \code{.dat}
#'   extension).
#'
#' @examples
#' \dontrun{
#' generate_simstrat_aed2_inflows(
#'   aed2_file       = "aed2.nml",
#'   phyto_pars_file = "aed2_phyto_pars.nml",
#'   out_dir         = "Simstrat_inflows",
#'   times           = 0:7,
#'   default_value   = 0
#' )
#' }
#'
#' @seealso [get_active_aed2_modules()],
#'   [get_phyto_names()]
#' @export
generate_simstrat_aed2_inflows <- function(aed2_file,
                                           phyto_pars_file = NULL,
                                           out_dir,
                                           inflow_map ,
                                           times = 0:7,
                                           default_value = 0) {
  
  
inflow_map <- data.frame(
  module = c(
    "aed2_carbon","aed2_carbon","aed2_carbon","aed2_carbon",
    "aed2_noncohesive",
    "aed2_nitrogen","aed2_nitrogen","aed2_nitrogen","aed2_nitrogen",
    "aed2_organic_matter","aed2_organic_matter",
    "aed2_phosphorus","aed2_phosphorus","aed2_phosphorus",
    "aed2_oxygen",
    "aed2_silica",
    "aed2_phytoplankton","aed2_phytoplankton","aed2_phytoplankton"
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
    "PHY_XX_IP_inflow"
  ),
  stringsAsFactors = FALSE
)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  active_modules <- get_active_aed2_modules(aed2_file)
  
  # keep only rows for active modules
  map_active <- inflow_map[inflow_map$module %in% active_modules, , drop = FALSE]
  
  # split phyto vs other modules
  phyto_rows <- map_active$module == "aed2_phytoplankton"
  map_phyto  <- map_active[phyto_rows, , drop = FALSE]
  map_other  <- map_active[!phyto_rows, , drop = FALSE]
  
  inflow_vars <- character(0)
  
  ## ---- non-phyto inflows ----
  inflow_vars <- c(inflow_vars, map_other$inflow_var)
  
  ## ---- phyto inflows based on p_name ----
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
          # e.g. "PHY_XX_inflow" -> "PHY_diatoms_inflow"
          inflow_vars <- c(
            inflow_vars,
            gsub("XX", nm, tmpl, fixed = TRUE)
          )
        }
      }
    }
  }
  
  inflow_vars <- unique(inflow_vars)
  
  ## ---- template time series ----
  template <- data.frame(
    Time = times,
    value = rep(default_value, length(times))
  )
  
  ## ---- write .dat files ----
  for (varname in inflow_vars) {
    file_path <- file.path(out_dir, paste0(varname, ".dat"))
    
    header <- sprintf("Time [d]\t%s [millimolesPerMeterCubed]", varname)
    
    con <- file(file_path, open = "w")
    writeLines(header, con)
    utils::write.table(
      template,
      con,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE
    )
    close(con)
  }
  
  invisible(inflow_vars)
}



