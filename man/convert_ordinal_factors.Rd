% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preprocessing.R
\name{convert_ordinal_factors}
\alias{convert_ordinal_factors}
\title{Convert regular factors that appear to be ordinal to numeric}
\usage{
convert_ordinal_factors(data, target_column)
}
\arguments{
\item{data}{A data.frame}

\item{target_column}{Character. Name of target column to preserve}
}
\value{
List with processed data and conversion metadata
}
\description{
Convert regular factors that appear to be ordinal to numeric
}
