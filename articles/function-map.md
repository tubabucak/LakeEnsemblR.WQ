# Function Map

LakeEnsemblR.WQ · package map

# Where each function sits in the workflow

The package mirrors a lake profile: work starts at the surface with
configuration, passes through the calibration layer where most of the
complexity lives, and settles into comparison and visualization at the
bottom. This map follows that order — each depth is one stage of the
pipeline, each card is one exported function.

Companion to the `full-workflow` vignette · read top to bottom for a
first run-through

Setup & execution Measurement & calibration Analysis & visualization
Click a card for what it takes in / hands off

## Configure

stage 1

Turn one master YAML into every coupled model's native config files.

export_config_wq()▶

Distributes LakeEnsemblR_WQ.yaml settings — module activation, parameter
defaults, coupling — into GLM/GOTM/Simstrat's own config files.

**Takes in** LakeEnsemblR_WQ.yaml (+ LakeEnsemblR.yaml if converting).  
**Hands off** populated model folders, ready for run_ensemble_wq().

load_config()▶

Reads and validates an Output-style YAML — bathymetry, metric
dictionary, model output folders — into resolved paths.

**Takes in** Output.yaml.  
**Hands off** a config list used internally by nearly every function
below.

**Under the hood —** export_config_wq() calls set_up_configs(),
set_coupling(), export_inputs(), export_pclake_physics(), and
convert_ler_to_lerwq() for you — rarely called directly.

## Validate

stage 2

Catch a broken config before spending minutes on a run that was always
going to fail.

validate_glm_aed()▶

Checks a GLM-AED2 folder's glm3.nml / aed2.nml for required sections.

validate_gotm_wet()▶

Same check for GOTM-WET / GOTM-Selmaprotbas's gotm.yaml + fabm.yaml.

validate_simstrat()▶

Same check for Simstrat-AED2's simstrat.par.

Also runs automatically inside run_ensemble_wq(validate = TRUE) — call
these directly only when you want failures surfaced earlier.

## Run the ensemble

stage 3

Execute each coupled model through its own runner package.

run_ensemble_wq()▶

Runs the requested models via GLM3r / WETr / SelmaprotbasR / SimstratR,
skipping or stopping on failure per on_error.

**Takes in** exported model folders.  
**Hands off** each model's output.nc, ready for metrics.

## Measure

stage 4

Pull harmonized variables and standard limnological metrics out of each
model's output.

get_output_wq()▶

Reads output.nc and returns harmonized-unit variables at requested
depths — the shared extraction engine behind calibration scoring and
plotting.

cal_metrics()▶

Computes every metric listed in Output.yaml, for one model or all of
them.

cal_stats()▶

NSE / RMSE / NRMSE / PBIAS / KGE between observed and predicted vectors
— the scoring primitive used everywhere else.

extract_variable_list()▶

Batch-extracts and caches variables across models for a metrics
dictionary.

integrate_depths()▶

Depth-integrates a profile variable into a single water-column value.

create_netcdf_output() / create_netcdf_wq()▶

Builds harmonized NetCDF output from extracted variables.

cal_epi_depth(), cal_meta_depth(), cal_strat_date() …▶

Metric-specific calculators (anoxic days, ice duration, stratification
onset, DO exceedance, nitrification, temperature difference) referenced
by the dictionary's function_name column.

## Calibrate

stage 5 · core

The deepest and busiest layer — sample parameters, edit configs, run,
score, repeat, then commit the winner.

create_calibration_tables()▶

Generates editable per-module calibration CSVs (bounds, include flags)
from the parameter dictionary.

**Hands off** CSVs you edit by hand — set include = TRUE, adjust bounds.

calib_setup_from_tables()▶

Reads the edited CSVs into the calib_setup table every calibration
function below consumes.

run_lhc_wq()▶

Latin Hypercube sampling (+ optional DE refinement) for one model: edits
config files, runs, scores against obs_file, restores originals on exit.

**Takes in** calib_setup, observed CSV.  
**Hands off** a results table with attr(., "best_parameter_set").

run_lhc_wq_parallel()▶

Same sampling, distributed across worker processes.

cali_ensemble_wq()▶

Multi-model wrapper — runs run_lhc_wq() per coupled model, sequentially
or concurrently, one call.

write_best_calib_to_par_files()▶

Writes the winning parameter set back — into par_file CSVs, or directly
into model configs with write_target = "config".

**Takes in** LHC/DE results + calib_setup.  
**Hands off** an updated model config — re-run run_ensemble_wq() to see
it take effect.

set_value_config()▶

Writes one parameter value into a model's native config, resolved via
the dictionary — the mechanism behind write_target = "config".

## Test sensitivity

stage 6

See which parameters actually move the outcome, before or after
calibrating.

run_sensitivity()▶

Sweeps a single parameter across its bounds and records the response.

run_multi_param_sensitivity()▶

Same, across several parameters in one pass.

## Compare & visualize

stage 7

Read whatever's currently sitting in output.nc against observations, or
across models.

plot_model_vs_obs_wq()▶

Plots one model's existing output against observed data at matching
depths, KGE/RMSE per depth. Reads output.nc — doesn't run the model.

compare_plot() / scat_plot()▶

Multi-model (GLM, WET, SELMAPROTBAS, Simstrat) comparison against
observations, with stats annotated.

compare_models_metric() / \_netcdf()▶

Cross-model metric comparison tables and plots.

plot_heatmap_wq()▶

Depth–time heatmap of a single variable.

plot_anoxic_metrics(), plot_ice_metrics(), plot_strat_metrics()▶

Metric-specific comparison plots across years and models.

visualise_dictionary()▶

Renders the parameter dictionary itself for browsing.

Generated by hand from the package's exported `NAMESPACE` — not
auto-synced. Update this file if functions are renamed or added.
