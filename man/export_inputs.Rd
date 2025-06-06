% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_inputs.R
\name{export_inputs}
\alias{export_inputs}
\title{Reads and sets nutrient inputs for all models}
\usage{
export_inputs(
  config_file,
  folder = ".",
  ler_config_file = "LakeEnsemblR.yaml",
  verbose = FALSE
)
}
\arguments{
\item{config_file}{character; name of LakeEnsemblR_WQ config file}

\item{folder}{path; location of config_file}

\item{ler_config_file}{character; name of LakeEnsemblR config file}

\item{verbose}{boolean; print changed parameters on screen}
}
\description{
Handles nutrient concentrations in inflows and other sources
}
