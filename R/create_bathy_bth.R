#' Create a Lake Analyzer bathymetry file (.bth)
#'
#' Converts a standard LakeEnsemblR bathymetry CSV file to a two-column
#' bathymetry file compatible with rLakeAnalyzer::load.bathy().
#'
#' @param standard_bathy_file Character path to a CSV containing bathymetry
#'   depth and area columns (for example: Bathymetry Depths and
#'   Bathymetry Areas).
#' @param output_file Optional character path to the output .bth file.
#'   Defaults to the input filename with .bth extension in the same folder.
#' @param overwrite Logical; overwrite output_file if it already exists.
#'
#' @return Character path to the generated .bth file.
#' @export
create_bathy_bth <- function(standard_bathy_file,
                             output_file = NULL,
                             overwrite = FALSE) {
  if (!is.character(standard_bathy_file) || length(standard_bathy_file) != 1L) {
    stop("'standard_bathy_file' must be a single file path.")
  }
  if (!file.exists(standard_bathy_file)) {
    stop("Bathymetry file not found: ", standard_bathy_file)
  }

  if (is.null(output_file)) {
    output_file <- sub("\\.[^.]*$", ".bth", standard_bathy_file)
  }

  if (file.exists(output_file) && !isTRUE(overwrite)) {
    stop("Output file already exists: ", output_file,
         ". Set overwrite = TRUE to replace it.")
  }

  out <- load_bathy_depth_area(standard_bathy_file)

  utils::write.table(
    out,
    file = output_file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE
  )

  return(output_file)
}


#' Load bathymetry data as depth-area table
#'
#' Reads either a standard bathymetry CSV or a Lake Analyzer .bth file and
#' returns a normalized data frame with columns `depths` and `areas`.
#'
#' @param bathy_file Character path to bathymetry file (.csv or .bth).
#'
#' @return data.frame with columns `depths` and `areas`.
#' @keywords internal
load_bathy_depth_area <- function(bathy_file) {
  if (!is.character(bathy_file) || length(bathy_file) != 1L) {
    stop("'bathy_file' must be a single file path.")
  }
  if (!file.exists(bathy_file)) {
    stop("Bathymetry file not found: ", bathy_file)
  }

  if (grepl("\\.csv$", bathy_file, ignore.case = TRUE)) {
    bathy_raw <- utils::read.csv(bathy_file, check.names = FALSE)
    out <- .extract_depth_area_columns(bathy_raw)
  } else {
    out <- rLakeAnalyzer::load.bathy(bathy_file)
    out <- data.frame(
      depths = suppressWarnings(as.numeric(out$depths)),
      areas = suppressWarnings(as.numeric(out$areas))
    )
  }

  out <- out[stats::complete.cases(out), , drop = FALSE]
  if (nrow(out) == 0L) {
    stop("No valid numeric depth/area rows found in bathymetry file.")
  }

  out <- out[order(out$depths), , drop = FALSE]
  rownames(out) <- NULL
  out
}


.extract_depth_area_columns <- function(bathy) {
  if (!is.data.frame(bathy) || ncol(bathy) < 2L) {
    stop("Bathymetry table must contain at least two columns.")
  }

  col_names <- tolower(trimws(names(bathy)))

  depth_candidates <- which(col_names %in% c("bathymetry depths", "bathymetry_depths", "depths", "depth"))
  area_candidates <- which(col_names %in% c("bathymetry areas", "bathymetry_areas", "areas", "area"))

  if (length(depth_candidates) == 0L) {
    depth_candidates <- grep("depth", col_names)
  }
  if (length(area_candidates) == 0L) {
    area_candidates <- grep("area", col_names)
  }

  if (length(depth_candidates) == 0L || length(area_candidates) == 0L) {
    stop("Could not identify depth/area columns in bathymetry table.")
  }

  data.frame(
    depths = suppressWarnings(as.numeric(bathy[[depth_candidates[1L]]])),
    areas = suppressWarnings(as.numeric(bathy[[area_candidates[1L]]]))
  )
}
