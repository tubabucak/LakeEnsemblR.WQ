# Calculate Stratified Days

Calculate the starting date of spring/summer stratification, Duration of
stratification Start date of mixing

## Usage

``` r
cal_strat_date(temp_data, hemisphere = "N")
```

## Arguments

- temp_data:

  dataframe; The output of the model/observed output. It should include
  Datetime and depth specific values per each column.

- hemisphere:

  character; N for Northern Hemisphere, S for Southern hemisphere

## Value

dataframe with starting date of stratification, start date of mixing and
duration of stratification
