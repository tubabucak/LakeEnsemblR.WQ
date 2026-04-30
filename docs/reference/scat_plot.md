# Scatter plot modeled vs observed

This function plots the observed versus predicted across depths and
annotate KGE and RMSE on the graph

## Usage

``` r
scat_plot(
  temp_glm,
  temp_wet,
  temp_selma,
  temp_avg,
  temp_avg_pareto,
  temp_obs,
  y_title
)
```

## Arguments

- temp_glm:

  Data frame: Modeled values from the GLM model in wide format with
  columns named "datetime" and "Depth\_\*".

- temp_wet:

  Data frame: Modeled values from the WET model in wide format with
  columns named "datetime" and "Depth\_\*".

- temp_selma:

  Data frame: Modeled values from the SELMAPROTBAS model in wide format
  with columns named "datetime" and "Depth\_\*".

- temp_avg:

  Data frame: Averaged modeled values across models in wide format with
  columns named "datetime" and "Depth\_\*".

- temp_avg_pareto:

  Data frame: Pareto-optimized averaged modeled values in wide format
  with columns named "datetime" and "Depth\_\*".

- temp_obs:

  Data frame: Observed values in wide format with columns named
  "datetime" and "Depth\_\*".

- y_title:

  Character: A string to label the y-axis (e.g., "Temperature (°C)", "DO
  (mg/L)").

## Value

A list containing:

- A ggplot2 object with the observed vs predicted scatter plot and model
  statistics annotated.

- A named list of statistics (RMSE and KGE) for each model.
