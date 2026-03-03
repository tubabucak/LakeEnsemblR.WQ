load_wq_dictionary <- function() {
  path <- system.file(
    "extdata",
    "LakeEnsemblR_WQ_dictionary.csv",
    package = "LakeEnsemblR.WQ"
  )

  if (path == "") {
    stop("WQ dictionary CSV not found in package extdata/")
  }

  utils::read.csv(path, stringsAsFactors = FALSE)
}