# LakeEnsemblR.WQ

Run, harmonize, and compare water quality simulations across multiple 1D
lake model frameworks.

LakeEnsemblR.WQ extends LakeEnsemblR workflows with tools to:

- configure model-specific WQ settings,
- validate model setups before run,
- run model ensembles,
- calibrate model ensembles – Latin Hypercube sampling, optional
  Differential Evolution refinement, and single-model or
  all-coupled-models-at-once (sequential or concurrent) calibration via
  [`cali_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cali_ensemble_wq.md),
- extract and harmonize output variables,
- calculate standardized ecosystem metrics and summary statistics,
- compare and visualize model outputs against each other and
  observations.

Supported model frameworks:

- GLM-AED2
- GOTM-WET
- GOTM-Selmaprotbas
- Simstrat-AED2

## Installation

``` r

# install.packages("remotes")
# Currently in development on tubabucak's fork; will move to the aemon-j
# organization (https://github.com/aemon-j/LakeEnsemblR.WQ) after release.
remotes::install_github("tubabucak/LakeEnsemblR.WQ")
```

Several dependencies are GitHub-only wrappers around external lake model
binaries (`GLM3r`, `WETr`, `SelmaprotbasR`, `SimstratR`, `MyLakeR`) and
are optional – you only need the ones matching the model(s) you actually
run
([`run_ensemble_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_ensemble_wq.md)/[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md)
check for them at call time and give an informative error if missing).
They’re declared in `Remotes:` in `DESCRIPTION` so
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html)
resolves them automatically.

`glmtools` (a hard dependency, needed regardless of which model you run)
has a known upstream packaging gap: its own `DESCRIPTION` depends on
`GLM3r` but doesn’t declare where to find it, which can make
dependency-solving tools (`pak`, and by extension
`devtools::install_deps()`/`build_vignettes()`) fail with
`Can't find package called GLM3r` even though `GLM3r` is in this
package’s own `Remotes:`. If you hit that, pre-install `GLM3r` first:

``` r

remotes::install_github("aemon-j/GLM3r@v3.3")
remotes::install_github("tubabucak/LakeEnsemblR.WQ")
```

## Typical workflow

``` r

library(LakeEnsemblR)

# 0) Export model-specific configuration and inputs for physical setup

export_config(
  config_file = "LakeEnsemblR.yaml",
  folder = ".",
  verbose = TRUE
)

library(LakeEnsemblR.WQ)


# 1) Export model-specific configuration and inputs for WQ setup
export_config_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  folder = ".",
  verbose = TRUE
)

# 2) Run selected models
run_res <- run_ensemble_wq(
  config_file = "LakeEnsemblR_WQ.yaml",
  models = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "SIMSTRAT-AED2"),
  folder = ".",
  validate = TRUE,
  verbose = TRUE
)

# 3) Compute metrics listed in Output.yaml
metric_out <- cal_metrics(
  metric_yaml_file = "Output.yaml",
  model_filter = "all",
  wq_config_file = "LakeEnsemblR_WQ.yaml"
)

# 4) Optional: compute summary statistics
stat_out <- cal_stats(metric_out)
```

## Calibration

``` r

# Build a calibration setup table from editable calibration CSVs
# (see create_calibration_tables() to generate the templates first)
cs_all <- calib_setup_from_tables(
  folder_in = "calibration",
  model_coupled = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2")
)

# Calibrate all coupled models at once, concurrently, with LHC + DE refinement
result <- cali_ensemble_wq(
  models          = c("GLM-AED2", "GOTM-WET", "GOTM-Selmaprotbas", "Simstrat-AED2"),
  calib_setup     = cs_all,
  yaml_file       = "Output.yaml",
  wq_config_file  = "LakeEnsemblR_WQ.yaml",
  obs_file        = "obs_data.csv",   # datetime, depth, variable_global_name, value
  n_samples       = 20,
  best_metric     = "KGE",
  use_de          = TRUE,
  parallel_models = TRUE,   # one worker process per model, run concurrently
  write_best      = TRUE,
  config_file     = "LakeEnsemblR_WQ.yaml"
)

result$summary                            # success/failure + row counts per model
result$best_parameter_sets[["GLM-AED2"]]  # winning parameters for one model
```

See
[`vignette("full-workflow")`](https://aemon-j.github.io/LakeEnsemblR.WQ/articles/full-workflow.md)
for the full calibration workflow, including single-model LHC/DE runs
via
[`run_lhc_wq()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md),
writing best parameters back with
[`write_best_calib_to_par_files()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/write_best_calib_to_par_files.md),
and sensitivity analysis.

## Key functions

- Setup and export:
  - export_config_wq()
  - export_inputs()
  - set_up_configs()
  - set_value_config()
- Validation and run:
  - validate_glm_aed()
  - validate_gotm_wet()
  - validate_simstrat()
  - run_ensemble_wq()
- Extraction and metrics:
  - get_output_wq()
  - extract_variable_list()
  - cal_metrics()
  - cal_stats()
  - export_all_stats()
- Calibration:
  - create_calibration_tables() / calib_setup_from_tables()
  - run_lhc_wq() – single-model LHC sampling, with optional DE
    refinement (`use_de = TRUE`) and optional internal parallelism
    (`parallel`/`de_parallel`)
  - cali_ensemble_wq() – calibrate multiple coupled models at once,
    sequentially or concurrently (`parallel_models = TRUE`)
  - write_best_calib_to_par_files() – write the best parameter set back
    to calibration files
  - run_sensitivity() / run_multi_param_sensitivity()
- Compare and visualize:
  - compare_models_metric() / compare_models_metric_netcdf()
  - compare_plot() / scat_plot()
  - plot_heatmap_wq()
  - plot_strat_metrics() / plot_anoxic_metrics() / plot_ice_metrics()

## Input files

Common project files include:

- LakeEnsemblR_WQ master config (for model/module settings)
- LakeEnsemblR physical config
- Output metric config (for selected metric families)

Example from a typical setup:

- LakeEnsemblR_WQ.yaml
- LakeEnsemblR.yaml
- Output.yaml
- LakeEnsemblR_bathymetry_standard
- LakeEnsemblR_ice-height_standard
- LakeEnsemblR_inflow_standard
- LakeEnsemblR_meteo_standard
- LakeEnsemblR_outflow_standard

## Output structure

[`cal_metrics()`](https://aemon-j.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
returns a nested list by metric and model. For each metric/model
combination, values are stored as data frames with `datetime` and
depth/value columns.

Example access:

``` r

metric_out$GLM$TP_gramsPerCubicMeter
```

## Troubleshooting

If a model run succeeds but some metrics are missing:

1.  Confirm the metric is enabled in Output.yaml.
2.  Confirm the metric-variable mapping exists in the metrics
    dictionary.
3.  Confirm the model actually wrote that variable to output.

For Simstrat-AED2 specifically, missing derived metrics (for example
total chlorophyll-a or total phosphorus) are often caused by missing
output variables in the Simstrat/AED2 output configuration
(OutputDiagnosticVars should be ‘true’ ), not by run failure.

If you’re developing against a local clone and see errors like
`unused arguments (...)` for a parameter that clearly exists in the
function’s source, your session is likely running a stale **installed**
copy of the package rather than your edited source – reload with
`devtools::load_all(".")` instead of
[`library(LakeEnsemblR.WQ)`](https://github.com/aemon-j/LakeEnsemblR.WQ).

## Development

``` r

devtools::load_all(".")     # load current source (not a stale install)
devtools::document()        # regenerate NAMESPACE/man/ from roxygen tags
devtools::test()            # run the tests/testthat suite
devtools::check()           # full R CMD check
devtools::build_readme()    # regenerate README.md from this file
```

CI (`.github/workflows/R-CMD-check.yaml`) runs the same check on every
push and PR; `.github/workflows/pkgdown.yaml` rebuilds and publishes the
documentation site to the `gh-pages` branch.

## Contributing

1.  Fork this repository.
2.  Clone your fork.
3.  Add the original repository as `upstream`.
4.  Pull latest changes from `upstream/main`.
5.  Create a feature branch, commit changes, and push.
6.  Run `devtools::test()` (and ideally `devtools::check()`) before
    opening a PR.
7.  Open a pull request.

Example git commands:

``` bash
git clone git@github.com:<username>/LakeEnsemblR.WQ.git
cd LakeEnsemblR.WQ
git remote add upstream git@github.com:aemon-j/LakeEnsemblR.WQ.git
git fetch upstream
git checkout -b my-feature
git merge upstream/main
git push -u origin my-feature
```
