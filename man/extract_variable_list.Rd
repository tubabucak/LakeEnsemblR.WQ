% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_variable_list.r
\name{extract_variable_list}
\alias{extract_variable_list}
\title{Create variable list for calculating the metrics}
\usage{
extract_variable_list(extracted_metric_dict, folder, config_file)
}
\arguments{
\item{folder}{character: main filepath; where all the model files are stored.}

\item{config_file}{character:filepath; To LER configuration yaml file.}

\item{xtracted_metric_dict}{character: full filepath for the metric dictionary.}
}
\value{
A list of extractedf variables for each model and for each metric defined in output.yaml
}
\description{
The aim is to extract & apply the necessary unit conversions the list of variables data that is selected in output.yaml.
}
\examples{
Example usage:
# temp_diff_result <- cal_bot_surf_temp_dif(temp_glm)
# print(temp_diff_result)
}
