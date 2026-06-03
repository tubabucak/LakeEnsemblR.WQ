repo_dir <- "c:/Users/au721619/Documents/Github/LakeEnsemblR.WQ"
setup_dir <- "c:/Users/au721619/Documents/LakeModeling/Mendota_setup"

setwd(setup_dir)

source(file.path(repo_dir, "R", "run_lhc_wq.r"))
source(file.path(repo_dir, "R", "run_lhc_wq_parallel.R"))

calib_setup <- read.csv("calibration_master.csv", stringsAsFactors = FALSE)
param_names <- c("Kw", "Knitrif")

n_samples <- 4
n_workers <- 2
seed <- 123

cat("Running sequential benchmark...\n")
set.seed(seed)
seq_time <- system.time({
  results_seq <- run_lhc_wq(
    model          = "GLM-AED2",
    param_names    = param_names,
    calib_setup    = calib_setup,
    yaml_file      = "Output.yaml",
    model_dir      = "GLM-AED2",
    n_samples      = n_samples,
    model_filter   = "GLM",
    wq_config_file = "LakeEnsemblR_WQ.yaml",
    verbose        = TRUE,
    save_results   = FALSE
  )
})

cat("\nRunning parallel benchmark...\n")
set.seed(seed)
par_time <- system.time({
  results_par <- run_lhc_wq_parallel(
    model            = "GLM-AED2",
    param_names      = param_names,
    calib_setup      = calib_setup,
    yaml_file        = "Output.yaml",
    model_dir        = "GLM-AED2",
    n_samples        = n_samples,
    model_filter     = "GLM",
    wq_config_file   = "LakeEnsemblR_WQ.yaml",
    verbose          = TRUE,
    n_workers        = n_workers,
    parallel_dir     = "tmp_lhc_workers",
    keep_worker_dirs = FALSE,
    save_results     = FALSE
  )
})

seq_elapsed <- unname(seq_time[["elapsed"]])
par_elapsed <- unname(par_time[["elapsed"]])
speedup <- seq_elapsed / par_elapsed

cat("\nBenchmark summary\n")
cat("Sequential elapsed:", seq_elapsed, "seconds\n")
cat("Parallel elapsed:", par_elapsed, "seconds\n")
cat("Speedup:", round(speedup, 2), "x\n")
cat("Sequential runs:", length(results_seq), "\n")
cat("Parallel runs:", length(results_par), "\n")
