% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal_metrics.r
\name{cal_metrics}
\alias{cal_metrics}
\title{Calculate the Metrics}
\usage{
cal_metrics(metric_yaml_file, folder, dict_file, config_file, bathy_file)
}
\arguments{
\item{metric_yaml_file}{Character: Name of the YAML file containing the list of metrics.}

\item{folder}{character: main filepath; where all the model files are stored.}

\item{dict_file}{character: full filepath for the metric dictionary.}

\item{config_file}{character:filepath; To LER configuration yaml file.}
}
\value{
A list of extractedf variables for each model and for each metric defined in output.yaml
}
\description{
The aim is to calculate the metrics in output.yaml and export as netcdf file
}
\examples{
Example usage:
# temp_diff_result <- cal_bot_surf_temp_dif(temp_glm)
# print(temp_diff_result)


}
