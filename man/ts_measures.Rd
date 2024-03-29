% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_measures.R
\name{ts_measures}
\alias{ts_measures}
\title{Calculate time series characteristics}
\usage{
ts_measures(x)
}
\arguments{
\item{x}{A time series object}
}
\value{
A vector of time series characteristics
}
\description{
Test for seasonality in a time series based on lags.
}
\examples{
ts_measures(ts(rnorm(120, 10,10),
               freq=12))
}
\references{
https://robjhyndman.com/hyndsight/tscharacteristics/
}
\author{
Rob Hyndman
}
