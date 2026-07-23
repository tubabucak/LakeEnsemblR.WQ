# Run an ensemble of LakeEnsemblR.WQ models selected

Runs selected models via their own runner packages, and optionally
skipping invalid models with a warning, then optionally post-processes
the produced NetCDF outputs.

## Usage

``` r
run_ensemble_wq(
  config_file,
  models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
  folder = ".",
  parallel = FALSE,
  ncores = NULL,
  verbose = TRUE,
  on_error = c("skip", "stop"),
  validate = TRUE,
  post_process = FALSE
)
```

## Arguments

- config_file:

  Character. Path to master LakeEnsebmlerWQ.yaml configuration file

- models:

  Character vector. Models to be run (e.g.
  c("GLM-AED","GOTM-WET","GOTM_SELMA","SIMSTRAT-AED2")).

- folder:

  Character. Base folder containing model run directories

- parallel:

  Logical. Run in parallel (not implemented yet).

- ncores:

  Integer. Number of cores (not used yet).

- verbose:

  Logical. Print progress messages.

- on_error:

  Character. "skip" (default) skips invalid/failed models with warnings,
  "stop" aborts on first invalid/failed model.

- validate:

  Logical. If TRUE, run model-specific validation first. This may be
  important to prevent getting error on a later stage

- post_process:

  Logical. If TRUE, call your harmonization/post-processing on
  successful runs. (not integrated yet)

## Value

A list containing validation results, run results, and NetCDF paths for
successful runs.
