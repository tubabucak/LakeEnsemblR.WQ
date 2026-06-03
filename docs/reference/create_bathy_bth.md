# Create a Lake Analyzer bathymetry file (.bth)

Converts a standard LakeEnsemblR bathymetry CSV file to a two-column
bathymetry file compatible with rLakeAnalyzer::load.bathy().

## Usage

``` r
create_bathy_bth(standard_bathy_file, output_file = NULL, overwrite = FALSE)
```

## Arguments

- standard_bathy_file:

  Character path to a CSV containing bathymetry depth and area columns
  (for example: Bathymetry Depths and Bathymetry Areas).

- output_file:

  Optional character path to the output .bth file. Defaults to the input
  filename with .bth extension in the same folder.

- overwrite:

  Logical; overwrite output_file if it already exists.

## Value

Character path to the generated .bth file.
