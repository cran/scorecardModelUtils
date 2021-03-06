% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{sampling}
\alias{sampling}
\title{Random sampling of data into train and test}
\usage{
sampling(base, train_perc = 0.7, seed = NA, replace = F)
}
\arguments{
\item{base}{input dataframe}

\item{train_perc}{(optional) percentage of total base to be kept as training sample, to be provided as decimal/fraction (default percentage is 0.7)}

\item{seed}{(optional) seed value (if not given random seed is generated)}

\item{replace}{(optional) whether replacement will e with or without replacement (default is FALSE ie. without replacement)}
}
\value{
An object of class "sampling" is a list containing the following components:

\item{train_sample}{training sample as a dataframe}

\item{test_sample}{test sample as a dataframe}

\item{seed}{seed used}
}
\description{
The function does random sampling of the data and split it into train and test datasets. Training base percentage and seed value(optional) is taken as arguments. If seed value is not specified, random seed will be generated on different iterations.
}
\examples{
data <- iris
sampling_list <- sampling(base = data,train_perc = 0.7,seed = 1234)
sampling_list$train
sampling_list$test
sampling_list$seed
}
\author{
Arya Poddar <aryapoddar290990@gmail.com>
}
