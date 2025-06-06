% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_epi_depth.r
\name{cal_epi_depth}
\alias{cal_epi_depth}
\title{Calculate Epilimnion Thickness}
\usage{
cal_epi_depth(temp_data, folder = ".")
}
\arguments{
\item{temp_data}{dataframe; The output of the model/observed temperature data. It should include Datetime and depth-specific temperature values in each column.}

\item{folder}{character; Optional folder path for saving results (default is '.').}
}
\value{
A dataframe with Datetime and epilimnion thickness.
}
\description{
Calculate the epilimnion thickness (in meter) from the temperature data
}
