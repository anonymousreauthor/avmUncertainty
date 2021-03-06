% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{trainModel.lm}
\alias{trainModel.lm}
\title{Train a linear model}
\usage{
\method{trainModel}{lm}(train_df, mod_type, mod_spec, boot_cnt = NULL,
  boot_per = 0.5, ...)
}
\arguments{
\item{train_df}{Training data}

\item{mod_type}{Type of model to train}

\item{mod_spec}{Model specification}

\item{...}{Additional arguments}
}
\value{
Trained linear model plut optional boot strapped approach
}
\description{
Train a linear model
}
