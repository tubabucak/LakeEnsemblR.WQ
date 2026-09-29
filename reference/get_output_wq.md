# Get lake model outputs

Get output data for each model (so far GLM-AED, SELMAPROTBAS-GOTM,
WET-GOTM) that is specified in the output.yaml

## Usage

``` r
get_output_wq(
  config_file,
  model,
  vars,
  obs_depths = NULL,
  depth_01 = 1,
  conversion_factor = 1
)
```

## Arguments

- config_file:

  character:filepath; to Output yaml.

- model:

  character; Model for which scaling parameters will be applied. So far
  options include c('GLM', 'SELMAPROTBAS', 'WET')

- vars:

  character vector; variables to be extracted to calculate the metric

- obs_depths:

  numeric vector; Observation depths. Its required if we need to
  interpolate the modelled output with observation depths. Defaults to
  NULL

- depth_01:

  integer; Indicates if the variable has 'z' dimension. 0: The variable
  has no depth component, 1: variable has depth component

- conversion_factor:

  numeric; unit conversion factors for common metric unit for each
  variable

## Value

A list or dataframe of extracted variables from the specified model. If
only one variable is extracted, a dataframe is returned. Otherwise, a
list of dataframes is returned.

dataframe or list of output variables

## Examples

``` r
if (FALSE) { # requireNamespace("GLM3r", quietly = TRUE) && requireNamespace("WETr", quietly = TRUE) && requireNamespace("SelmaprotbasR", quietly = TRUE) && requireNamespace("SimstratR", quietly = TRUE)
# \donttest{
ex <- lerwq_example()
library(LakeEnsemblR)  # export_config() needs LakeEnsemblR attached
export_config("LakeEnsemblR.yaml", folder = ex,
              model = c("GLM", "GOTM", "Simstrat"))
export_config_wq("LakeEnsemblR_WQ.yaml", folder = ex)
run_ensemble_wq("LakeEnsemblR_WQ.yaml", models = "GLM-AED2", folder = ex,
                verbose = FALSE)

# GLM-AED2 oxygen (mmol O2/m3) at 1, 10 and 20 m, converted to g/m3
old <- setwd(ex)
do <- get_output_wq(config_file = "Output.yaml", model = "GLM", vars = "OXY_oxy",
                    obs_depths = c(1, 10, 20), conversion_factor = 0.032)
head(do$OXY_oxy)
setwd(old)
# }
}
```
