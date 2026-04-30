# Calculate Epilimnion Thickness

Calculate the epilimnion thickness (in meter) from the temperature data

## Usage

``` r
cal_epi_depth(temp_data, folder = ".")
```

## Arguments

- temp_data:

  dataframe; The output of the model/observed temperature data. It
  should include Datetime and depth-specific temperature values in each
  column.

- folder:

  character; Optional folder path for saving results (default is '.').

## Value

A dataframe with Datetime and epilimnion thickness.
