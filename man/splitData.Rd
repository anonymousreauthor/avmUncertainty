% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{splitData}
\alias{splitData}
\title{Create train/test split}
\usage{
splitData(study_df, horizon_date)
}
\arguments{
\item{study_df}{A dataframe of transactions}

\item{horizon_date}{Date after which to set aside as test data}
}
\value{
List of training and test data
}
\description{
Split data based on a horizon ate
}
