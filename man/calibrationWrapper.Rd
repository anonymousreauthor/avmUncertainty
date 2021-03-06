% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrappers.R
\name{calibrationWrapper}
\alias{calibrationWrapper}
\title{Wrapper for train/score/calibrate sequence with splitting by a field}
\usage{
calibrationWrapper(data_df, split_date, model_type, model_spec,
  split_field = NULL, ...)
}
\arguments{
\item{data_df}{A dataframe of transactions}

\item{split_date}{Date to split for train/test}

\item{model_type}{Type of model (current just 'lm' or 'rf')}

\item{model_spec}{Model specification}

\item{split_field}{[NULL] Field on which to split up the data}
}
\value{
full uncertainty object
}
\description{
Wrapper for train/score/calibrate sequence with splitting by a field
}
