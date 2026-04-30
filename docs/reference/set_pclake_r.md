# Sets a value in a PCLake par data.frame

Sets a value in a PCLake parameter or initial states file that has been
read into R as a data.frame

## Usage

``` r
set_pclake_r(file, par_list, column = "sSet1", verbose = FALSE)
```

## Arguments

- file:

  data.frame;

- par_list:

  list; parameter names without underscores and corresponding value to
  enter

- column:

  character; column name to change in file. defaults to sSet1

- verbose:

  logical; print changed parameters to screen
