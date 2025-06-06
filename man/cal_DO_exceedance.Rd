% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_DO_exceedance.r
\name{cal_DO_exceedance}
\alias{cal_DO_exceedance}
\title{Calculate DO exceedance}
\usage{
cal_DO_exceedance(oxy_data, depth = 0)
}
\arguments{
\item{oxy_data}{{data.frame} A dataframe containing oxygen concentrations. 
The first column should be `datetime`, and the remaining columns should represent 
oxygen values at different depths.}

\item{depth}{{numeric} The depth at which to extract data. Must match a column name 
in `oxy_data` (e.g., 1.0, 1.5, etc.).}
}
\value{
a data.frame
}
\description{
Calculates the percentage time exceeded curve for O2 (similar approach to Flow Duration Curve)
}
