% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_anoxic_date.r
\name{cal_anoxic_date}
\alias{cal_anoxic_date}
\title{Calculate Anoxic Days}
\usage{
cal_anoxic_date(oxy_data, bathy_file, threshold = 1, duration = "full")
}
\arguments{
\item{oxy_data, }{dataframe; The output of the model/observed output. It should include Datetime and depth specific values per each column.}

\item{bathy_file, }{dataframe; The file includes depth and area relationship}

\item{threshold, }{anoxic O2 threshold (mg L-1)}

\item{duration, }{argument for calculating the anoxic factor and number of anoxic days. If duration = full, the number of all anoxic days will be calculated. If duration = longest, AF for the longest anoxic period will be returned.}
}
\value{
dataframe with number of anoxic days for each year and anoxic factor. Also returns the depth for each day where anoxic layer starts
}
\description{
Calculates Number of Anoxic Days and Anoxic Factors (Nürnberg, 1995) for each year/season
}
