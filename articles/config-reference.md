# Configuration File Reference

## Purpose

LakeEnsemblR.WQ is driven by two YAML configuration files, not by an R
object you build up interactively. Almost everything the package does –
exporting model configs, running the ensemble, calibrating, computing
metrics – starts by reading one or both of these files:

- **`LakeEnsemblR_WQ.yaml`** – describes *which coupled models to run
  and which biogeochemical modules are active in them*. Read by
  [`export_config_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
  and everything downstream of it.
- **`Output.yaml`** – describes the metrics that the user would like to
  calculate after calibrationg the models: which support files
  (bathymetry, dictionary) to use, and which derived metrics to
  compute\*. Read by
  [`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md),
  [`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md),
  [`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md),
  and most calibration/plotting functions.

This vignette documents both files that you can use in your setup as a
template. If you’re looking for the step-by-step procedure instead, see
[`vignette("full-workflow")`](https://tubabucak.github.io/LakeEnsemblR.WQ/articles/full-workflow.md).

## `LakeEnsemblR_WQ.yaml`

A complete, real example (from a coupled
GLM-AED2/GOTM-WET/GOTM-Selmaprotbas/ Simstrat-AED2 setup):

``` yaml
models:
   - GOTM-WET
   - GOTM-Selmaprotbas
   - GLM-AED2
   - Simstrat-AED2
config_files:
   GOTM-WET: GOTM-WET/fabm.yaml
   GOTM-Selmaprotbas: GOTM-Selmaprotbas/fabm.yaml
   GLM-AED2: GLM-AED2/aed2.nml
   Simstrat-AED2: Simstrat-AED2/aed2.nml
run_settings:
   bio-shading: true
   ode_method: Euler
   split_factor: 1
   bottom_everywhere: true
   repair_state: true
input:
   inflows: LakeRavn_inflow_nutrients.csv
oxygen:
   use: true
   par_file:
carbon:
   use: true
   par_file:
sediment:
   use: true
   par_file:
detritus:
   use: true
   par_file:
nitrogen:
   use: true
   par_file:
phosphorus:
   use: true
   par_file:
silicon:
   use: true
   par_file:
phytoplankton:
   use: true
   groups:
      diatoms:
         par_file:
      cyanobacteria:
         par_file:
zooplankton:
   use: true
   groups:
      daphnia:
         par_file:
         prey:
         - phytoplankton/diatoms
         - phytoplankton/cyanobacteria
fish:
   use: false
   groups:
      benthivores:
         par_file: WQ_input_files/fish.csv
         prey:
         - zooplankton/daphnia
```

### Top-level keys

| Key | Purpose |
|----|----|
| `models` | Which coupled models to set up/run. Supported values: `GLM-AED2`, `GOTM-WET`, `GOTM-Selmaprotbas`, `Simstrat-AED2` (`MyLake`/`PCLake` are recognized but not fully supported yet). |
| `config_files` | One entry per model in `models`, pointing at that model’s own native biogeochemistry config file (`fabm.yaml` for GOTM-based models, `aed2.nml` for GLM/Simstrat). [`export_config_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md) writes parameter values into these files. |
| `run_settings` | Solver/numerics settings shared across models – see below. |
| `input` | Currently just `inflows`: path to the nutrient-inflow CSV used by [`export_inputs()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_inputs.md). |
| `output` | Reserved for future use – not currently read anywhere in the package. Safe to leave commented out. |
| Everything else | Treated as a **biogeochemical module** (`oxygen`, `carbon`, `sediment`, `detritus`, `nitrogen`, `phosphorus`, `silicon`, `phytoplankton`, `zooplankton`, `fish`, `macrophytes`, `zoobenthos`, `pathogens`). See below. |

### `run_settings`

| Key | Meaning |
|----|----|
| `bio-shading` | Whether biogeochemical state variables (e.g. chlorophyll) contribute to light attenuation. `true`/`false`. |
| `ode_method` | Numerical integration scheme. One of `Euler`, `RK2`, `RK4`, `Pat1`, `PatRK2`, `PatRK4`, `ModPat1`, `ModPatRK2`, `ModPatRK4`, `ExtModPat1`, `ExtModPatRK2`. An unrecognized value stops [`export_config_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md) with an error rather than silently falling back to a default. |
| `split_factor` | Sub-stepping factor for the biogeochemical solver relative to the physical model’s timestep. |
| `bottom_everywhere` | Whether benthic/sediment processes are applied at every depth layer that touches the bottom (rather than only the single deepest layer). |
| `repair_state` | Whether the solver is allowed to clip/repair state variables that go negative or otherwise invalid mid-run, instead of failing. |

These get translated into each model’s own native settings – e.g. into
GLM’s `aed2.nml` `&wq_setup` block, or written directly into GOTM’s
`fabm.yaml`. Note that the *specific value* is model-specific even
though the setting is declared once here (e.g. `ode_method: Euler` maps
to a different underlying number in GLM’s namelist than in GOTM’s).

### Biogeochemical modules

Two shapes, depending on the module:

**Simple modules** (`oxygen`, `carbon`, `sediment`, `detritus`,
`nitrogen`, `phosphorus`, `silicon`) each take:

``` yaml
<module>:
   use: true            # whether this module is active at all
   par_file:            # optional CSV overriding default parameter values;
                         # blank = use dictionary defaults
```

**Group-aware modules** (`phytoplankton`, `zooplankton`, `fish`,
`macrophytes`, `zoobenthos`, `pathogens`) instead nest one or more named
groups under `groups:`, each with its own `par_file`:

``` yaml
<module>:
   use: true
   groups:
      <group_name>:
         par_file:
         prey:                        # zooplankton/fish only
         - phytoplankton/diatoms      # references another group by
                                       # "<module>/<group_name>"
```

Group names are yours to choose (`diatoms`, `cyanobacteria`, `daphnia`
above are just labels) – they show up later as the `group_name` column
in calibration tables
([`create_calibration_tables()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/create_calibration_tables.md)),
which matters because **a parameter name can repeat across groups**
(e.g. `r0` for both `diatoms` and `cyanobacteria`) and needs
`group_name` to disambiguate which one you’re calibrating or running
sensitivity on.

`prey` lists (zooplankton, fish) declare grazing/predation links between
groups by `"<module>/<group_name>"` string, as seen in the `daphnia`
example above eating both phytoplankton groups.

If `use: false` for a module,
[`export_config_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/export_config_wq.md)
disables it in every model’s native config rather than leaving it
partially configured.

## `Output.yaml`

A complete example:

``` yaml
folder:
files:
  bathy_file: "LakeEnsemblR_bathymetry_standard.csv"
  metric_yaml_file: "Output.yaml"
  LER_config_file: "LakeEnsemblR.yaml"
model_folders:
  GLM: "GLM-AED2/Output"
  WET: "GOTM-WET/Output"
  SELMAPROTBAS: "GOTM-Selmaprotbas/Output"
  SIMSTRAT: "Simstrat-AED2/output"
Level1:
   LER:
      Temp_degreeCelcius:
   Oxygen:
      DO_gramsPerCubicMeter:
   Nitrogen:
      NH4_gramsPerCubicMeter:
      NO3_gramsPerCubicMeter:
   Phosphorus:
      PO4_gramsPerCubicMeter:
      TP_gramsPerCubicMeter:
   Phytoplankton:
      Total_Chla_miligramsPerCubicMeter:
```

### `folder`

Base directory every relative path in this file is resolved against.
Leave it **blank** (as above) to default to the directory containing
`Output.yaml` itself – this is what you want in almost every case, since
it lets the same config work no matter where the project folder is
checked out. Only set it explicitly if you’re pointing `Output.yaml` at
files that live somewhere other than alongside it.

### `files`

Three entries, all **required** –
[`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)
will error if any is missing or doesn’t resolve to an existing file:

| Key | Meaning |
|----|----|
| `bathy_file` | Path to the bathymetry CSV (or `.bth` file). |
| `metric_yaml_file` | Path to *this same file* – yes, `Output.yaml` self-references its own filename. Used by code that needs to know where the metrics config lives without being told separately. |
| `LER_config_file` | Path to the physical `LakeEnsemblR.yaml` config. |

Paths are resolved relative to `folder` **unless** they’re already
absolute (start with `/` on Linux/Mac or `C:`-style on Windows) – see
[`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)’s
`resolve_path()`.

### `model_folders`

Maps a short model key to the subfolder (relative to `folder`) where
that model writes its NetCDF output:

| Key            | Typical value              |
|----------------|----------------------------|
| `GLM`          | `GLM-AED2/Output`          |
| `WET`          | `GOTM-WET/Output`          |
| `SELMAPROTBAS` | `GOTM-Selmaprotbas/Output` |
| `SIMSTRAT`     | `Simstrat-AED2/Output`     |

By default (`required_models = NULL`/`"all"`),
[`load_config()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/load_config-colon.md)
requires **every** entry listed here to exist on disk – even if you only
intend to work with one model right now. Several functions
([`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md),
[`run_lhc_wq()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/run_lhc_wq.md))
accept a `required_models`/`model_filter` argument specifically to
narrow this down to just the model(s) you’re actually using, which
avoids failing on an unrelated model you haven’t run yet.

### `Level1` / `Level2` / `Level3`

These blocks declare which derived metrics
[`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
computes, grouped by category (`LER`, `Oxygen`, `Nitrogen`,
`Phosphorus`, `Phytoplankton`, etc.) and by complexity tier:

- **Level1** – direct/simple variables (e.g. `Temp_degreeCelcius`,
  `DO_gramsPerCubicMeter`) – one value per depth per timestep.
- **Level2** – first-order derived quantities (e.g.
  `Evaporation_metersPerSecond`, `Nitrif_gramsPerCubicMeterPerDay`).
- **Level3** – more complex/aggregated derived quantities (e.g.
  `Phyto_GPP_miligramsPerCubicMeterPerDay`).

A metric name with nothing under it (just a trailing `:`) is enabled
with default settings. Some metrics take extra parameters nested
underneath them, e.g.:

``` yaml
Duration_of_Stratification:
   hemisphere: "N"
Number_Of_Anoxic_Day:
   threshold: 1.0
   duration: "full"
```

**Comment a metric out with `#` to disable it** rather than deleting the
line – this is the standard way these files get edited day to day (as
seen throughout the shipped example, where most Level2/Level3 metrics
are commented out by default).
[`cal_metrics()`](https://tubabucak.github.io/LakeEnsemblR.WQ/reference/cal_metrics.md)
only computes what’s left uncommented.

## Common pitfalls

A short list of things that have actually gone wrong with these two
files in practice:

- **Case mismatches on Linux.** Windows filesystems are
  case-insensitive, so a typo like `SIMSTRAT-AED2` instead of
  `Simstrat-AED2` works silently on Windows and fails with
  “file/directory not found” on Linux. This applies to `models`,
  `model_folders` values, and anywhere else a model-folder name gets
  typed literally.
