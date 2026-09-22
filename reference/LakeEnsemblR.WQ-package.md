# LakeEnsemblR.WQ: Run Ensembles of Water Quality 1D Lake Models

Extends the 'LakeEnsemblR' physical-model ensemble framework with water
quality and biogeochemical simulation. Configures, runs, and calibrates
coupled physical-biogeochemical 1D lake models – GLM-AED2,
GOTM-Selmaprotbas, GOTM-WET, and Simstrat-AED2 – built on the same
bathymetry, meteorology, and inflow setup that 'LakeEnsemblR' produces.
Provides tools to build calibration parameter tables from a shared
metrics dictionary, run Latin hypercube and differential evolution
calibration with optional parallel backends, perform sensitivity
analysis, extract and harmonize output across the coupled models, and
compare simulated water quality variables (e.g. dissolved oxygen,
nutrients, chlorophyll, and phytoplankton/zooplankton groups) against
observations using standard goodness-of-fit metrics such as KGE, NSE,
RMSE, and PBIAS. Includes visualization functions for depth-time
heatmaps, stratification and ice metrics, and model-versus-observation
diagnostic plots.

## See also

Useful links:

- <https://github.com/tubabucak/LakeEnsemblR.WQ>

- <https://tubabucak.github.io/LakeEnsemblR.WQ>

## Author

**Maintainer**: Tuba Bucak <tbo@ecos.au.dk>

Authors:

- Tuba Bucak <tbo@ecos.au.dk>

- Robert Ladwig <rladwig2@wisc.edu>

- Johannes Feldbauer <johannes.feldbauer@tu-dresden.de>

- Jorrit Mesman <jorrit.mesman@unige.ch>
