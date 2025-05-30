## LakeEnsemblR.WQ: Calculate and Export Lake Ecosystem Metrics

This version of the package focuses on harmonizing and post-processing lake model outputs from **GLM-AED2**, **GOTM-WET**, and **GOTM-SELMAPROTBAS**. Using raw model outputs, the package calculates system metrics (as outlined in Hipsey et al. 2021) and returns them in a structured list format. Model running & calibration functions will be added soon.

The `output.yaml` file should specify:

- Paths to required input files (e.g., bathymetry, metric dictionary, NetCDF model outputs)
- A list of metrics to compute

The core function of the package is `cal_metrics()`. It reads the YAML configuration file, extracts relevant data, harmonizes variable formats and units, and applies metric functions to generate outputs.

---

### `cal_metrics()`: Main Metric Calculation Function

#### What it does:

- Loads model output and bathymetry files based on a YAML config
- Extracts variables across different models (GLM, SELMAPROTBAS, WET)
- Applies predefined or custom metric functions
- Returns results as a named list organized by metric and model

---

### Input

```r
cal_metrics(
  metric_yaml_file = "output.yaml",   # YAML file defining variables and metrics
  model_filter = "all"                # Options: "GLM", "WET", "SELMAPROTBAS", or "all"
)
```

---

### Output

A nested list:
```r
$MetricName
  $ModelName
    data.frame (datetime × value(s))
```

**Example:**
```r
result$Temp_degreeCelcius$GLM
result$Chla_TP_ratio$SELMAPROTBAS
```


### Example Usage

```r
library(LakeEnsemblR.WQ)

result <- cal_metrics("config/output.yaml")

# Plot example: Temperature at 1m depth
plot(result$Temp_degreeCelcius$GLM$datetime,
     result$Temp_degreeCelcius$GLM$Depth_1,
     type = "l", xlab = "Date", ylab = "Temperature (°C) at 1m")
```




# How do I contribute new code back to the LakeEnsemblR.WQ project?
In order to contribute to this code, we recommend the following workflow:

- "fork" this repository to your own personal github account

- clone the github repository to your computer:

- `$git clone <git@github.com:{username}/LakeEnsemblR.WQ.git>`

- modify code or add new functionality, save the code

add the repository main to a remote main called "upstream"

- `$cd LakeEnsemblR.WQ`

- `$git remote add upstream <git@github.com:aemon-j/LakeEnsemblR.WQ.git>`

before pushing your changes to your repository, pull in the current version of the aemon-j main:

- `$git fetch upstream`

- merge these differences with your own "main" version:

- `$git merge upstream/main`

push your changes to your github repository, in addition to changes made by pulling in the aemon-j main:

- `$git push`

submit a pull request to aemon-j main using your account at github.com
