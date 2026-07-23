# Load bathymetry data as depth-area table

Reads either a standard bathymetry CSV or a Lake Analyzer .bth file and
returns a normalized data frame with columns \`depths\` and \`areas\`.

## Usage

``` r
load_bathy_depth_area(bathy_file)
```

## Arguments

- bathy_file:

  Character path to bathymetry file (.csv or .bth).

## Value

data.frame with columns \`depths\` and \`areas\`.
