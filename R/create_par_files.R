#' Create parameter files from the dictionary
#'
#' Writes one \code{par_file} CSV per active module (one per group for
#' phytoplankton, zooplankton and fish) containing every dictionary parameter
#' of the selected models, with \code{value} set to the dictionary default.
#' Unlike \code{\link{create_calibration_tables}}, nothing is filtered out:
#' boolean and integer switches (e.g. \code{buoyancy_regulation},
#' \code{nitrogen_fixation}, \code{lNfix}) and zero-default parameters are all
#' included.
#'
#' @details
#' The files are meant as templates for group- or lake-specific parameter
#' sets: edit \code{value} for the parameters to change, delete the rows to
#' leave at their dictionary default, and point the corresponding
#' \code{par_file} entry of the LakeEnsemblR_WQ config file at the result.
#' \code{\link{export_config_wq}} then applies the listed values; the
#' \code{unit}, \code{path} and \code{note} columns are for reference only.
#'
#' @param folder path; directory containing the config file.
#' @param config_file character; name of the LakeEnsemblR_WQ YAML config file.
#' @param folder_out path; output directory for the CSV files (created if needed).
#' @param models_coupled character vector; model couplings to include. Defaults
#'   to the \code{models} listed in the config file.
#' @param overwrite logical; overwrite existing files in \code{folder_out}?
#'   Defaults to \code{FALSE} so edited parameter files are not replaced.
#'
#' @return Invisibly returns the paths of the files written.
#'
#' @importFrom configr read.config
#'
#' @examples
#' ex <- lerwq_example()
#' create_par_files(
#'   folder         = ex,
#'   config_file    = "LakeEnsemblR_WQ.yaml",
#'   folder_out     = file.path(ex, "par_files"),
#'   models_coupled = "GOTM-Selmaprotbas"
#' )
#' list.files(file.path(ex, "par_files"))
#'
#' @export

create_par_files <- function(folder = ".",
                             config_file,
                             folder_out     = folder,
                             models_coupled = NULL,
                             overwrite      = FALSE) {

  if (!file.exists(folder_out)) {
    dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)
  }

  lst_config <- read.config(file.path(folder, config_file))
  if (is.null(models_coupled)) {
    models_coupled <- unlist(lst_config[["models"]])
  }

  dict <- LakeEnsemblR_WQ_dictionary
  wq_models <- .wq_model_key(models_coupled)

  par_table <- do.call(rbind, lapply(seq_along(models_coupled), function(i) {
    rows <- dict[dict$model == wq_models[i], , drop = FALSE]
    if (nrow(rows) == 0) return(NULL)
    rows$model_coupled <- models_coupled[i]
    rows
  }))
  if (is.null(par_table)) {
    stop("No dictionary parameters found for models: ",
         paste(models_coupled, collapse = ", "))
  }

  par_table$value <- trimws(as.character(par_table$default))
  par_table <- par_table[!is.na(par_table$value) & nzchar(par_table$value), ]
  # "NAME" marks group/prey names (p_name, zoop_name, zoop_prey) that
  # set_coupling() fills from the config file; they aren't parameters.
  par_table <- par_table[par_table$value != "NAME", ]
  # num_prey is set by set_coupling() from the prey listed in the config file
  par_table <- par_table[trimws(par_table$parameter) != "num_prey", ]

  # Rows that only differ in path (e.g. WET kNMinS of DOM and humus) are kept:
  # export_config_wq() passes the path column to set_value_config(), which
  # uses it to pick the right dictionary row.
  key_cols <- c("module", "domain", "process", "subprocess", "model_coupled", "parameter", "path")
  par_table <- par_table[!duplicated(par_table[, key_cols]), ]

  out_cols <- c("domain", "process", "subprocess", "model_coupled", "parameter",
                "value", "unit", "path", "note")

  # Active modules, grouped the same way export_config() reads par_files
  modules <- names(lst_config)[!(names(lst_config) %in%
                                   c("models", "config_files", "run_settings", "input", "output"))]
  modules <- Filter(function(x) {
    !is.null(lst_config[[x]]) && isTRUE(lst_config[[x]][["use"]])
  }, modules)

  written <- character(0)
  for (i in modules) {
    subset <- par_table[par_table$module == i, out_cols]
    if (nrow(subset) == 0) next

    groups <- if (i %in% c("phytoplankton", "zooplankton", "fish")) {
      names(lst_config[[i]][["groups"]])
    } else {
      i
    }

    for (j in groups) {
      fname <- file.path(folder_out, paste0("par_", j, ".csv"))
      if (file.exists(fname) && !overwrite) {
        message("Skipped (exists, overwrite = FALSE): ", fname)
        next
      }
      write.csv(subset, fname, row.names = FALSE)
      message("Created: ", fname)
      written <- c(written, fname)
    }
  }

  message("\nEdit 'value' (and delete rows to keep at dictionary default), then ",
          "set the par_file entries in ", config_file, ".")
  invisible(written)
}
