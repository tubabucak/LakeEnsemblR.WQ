# Calculate surface-bottom temperature difference

Calculate the temperature difference between bottom and surface of the
lake. Uses layer.temperature function from rlakeAnalyzer

## Usage

``` r
cal_bot_surf_temp_dif(temp_data)
```

## Arguments

- temp_data:

  data.frame A dataframe containing temperature profiles. The first
  column should be \`datetime\`, and the remaining columns should
  represent temperature at different depths.

## Value

data.frame A dataframe with two columns:

- datetime:

  The datetime of each observation.

- temp_diff:

  The difference between surface and bottom temperature (°C).
