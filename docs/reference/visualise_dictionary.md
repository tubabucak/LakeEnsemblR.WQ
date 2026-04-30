# Visualise LakeEnsemblR.WQ dictionary

Visualise the file structure of the dictionary

## Usage

``` r
visualise_dictionary(
  print_console = TRUE,
  save_as_table = FALSE,
  folder = ".",
  filename = "dictionary.txt",
  module = TRUE,
  domain = TRUE,
  process = TRUE,
  subprocess = TRUE,
  model = FALSE,
  parameter = FALSE
)

visualize_dictionary(
  print_console = TRUE,
  save_as_table = FALSE,
  folder = ".",
  filename = "dictionary.txt",
  module = TRUE,
  domain = TRUE,
  process = TRUE,
  subprocess = TRUE,
  model = FALSE,
  parameter = FALSE
)
```

## Arguments

- print_console:

  boolean; print result to console

- save_as_table:

  boolean; save result as a txt file? If yes, use folder and filename
  arguments

- folder:

  path; path where to write table

- filename:

  character; file name of table

- module:

  boolean; include module in visualisation?

- domain:

  boolean; include domain in visualisation?

- process:

  boolean; include process in visualisation?

- subprocess:

  boolean; include subprocess in visualisation?

- model:

  boolean; include model in visualisation?

- parameter:

  boolean; include parameter in visualisation?
