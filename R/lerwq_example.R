#' Copy the bundled example setup
#'
#' Copies the example setup shipped with the package to a folder of your
#' choice, so it can be configured and run without modifying the installed
#' package. The example covers one year (1995) of Lake Mendota (Wisconsin,
#' USA): meteorological forcing, inflow/outflow, bathymetry, observed water
#' temperature and water quality, and LakeEnsemblR/LakeEnsemblR.WQ
#' configuration files for GLM-AED, GOTM-WET, GOTM-Selmaprotbas and
#' Simstrat-AED2. It is used by the examples throughout this package.
#'
#' @param dest character; folder to copy the example into. Created if it does
#'   not exist. Defaults to a folder in \code{tempdir()}.
#' @param overwrite logical; overwrite files that already exist in \code{dest}.
#'
#' @return The normalized path to \code{dest}.
#'
#' @examples
#' ex <- lerwq_example()
#' list.files(ex)
#'
#' @export
lerwq_example <- function(dest = file.path(tempdir(), "lerwq_example"),
                          overwrite = FALSE) {
  src <- system.file("extdata", "example", package = "LakeEnsemblR.WQ")
  if (!nzchar(src)) {
    stop("Example data not found in the installed LakeEnsemblR.WQ package.")
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(src, full.names = TRUE), dest,
            recursive = TRUE, overwrite = overwrite)
  normalizePath(dest, winslash = "/")
}
