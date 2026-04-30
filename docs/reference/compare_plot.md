# Comparing observed/predicted data and calculate the stats

The aim is to plot observed vs predicted and calculate statistical
measures for each model. So far we have GLM, WET and SELMAPROTBAS

## Usage

``` r
compare_plot(
  data_glm,
  data_wet,
  data_selma,
  data_simstrat,
  data_obs,
  depth,
  y_title
)
```

## Arguments

- data_glm:

  dataframe. Timeseries output of GLM model ...

- data_wet:

  dataframe. Timeseries output of WET model ...

- data_selma:

  dataframe. Timeseries output of SELMAPROTBAS model ...

- data_obs:

  dataframe. Timeseries output of OBSERVED data ...

- depth:

  numeric. Certain depth to be compared.

- y_title:

  character. Name of the variable to be shown in the plot.

## Value

A list with comparison plot with statistical metrics. order: plot,
stats_glm, stats_wet, stats_selma
