# Calculate Anoxic Days

Calculates Number of Anoxic Days and Anoxic Factors (Nürnberg, 1995) for
each year/season

## Usage

``` r
cal_anoxic_date(oxy_data, bathy_file, threshold = 1, duration = "full")
```

## Arguments

- oxy_data:

  data.frame The output of the model/observed output. It should include
  Datetime and depth-specific values per column.

- bathy_file:

  data.frame The file includes depth and area relationship

- threshold:

  numeric Anoxic O2 threshold (mg L-1). Default is 1 mg/L.

- duration:

  character Argument for calculating the anoxic factor and number of
  anoxic days. Use "full" for all anoxic days or "longest" for only the
  longest anoxic period.

## Value

A list with three dataframes:

- AF_yearly:

  A dataframe with the yearly anoxic factor values.

- num_anoxic_days:

  A dataframe with the number of anoxic days per year.

- anoxic_depths:

  A dataframe with the anoxic depth recorded for each day.
