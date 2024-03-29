% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SyntheticData3.R
\docType{data}
\name{SyntheticData3}
\alias{SyntheticData3}
\title{SyntheticData3}
\format{
A \code{tsibble} with four columns, which are:
\describe{
\item{\code{Value}}{The categorical values of the time series in the dataset.}
\item{\code{Series}}{Integer values indicating the considered time series (there are 80 time series in the dataset).}
\item{\code{Time}}{Integer values indicating the temporal indexes of the observations.}
\item{\code{Class}}{Integer values indicating the class of each time series.}
}
 @details The column \code{Value} is the concatenation of 80 time series of length 600
taking three categorical values. Series 1-20, 21-40, 41-60 and 61-80 were generated from
NDARMA processes with different orders and vectors of coefficients (see Scenario 3 in \insertCite{lopez2023hard;textual}{ctsfeatures}).
Therefore, there are 4 different classes in the dataset.
}
\usage{
data(SyntheticData3)
}
\description{
Synthetic dataset containing 80 CTS generated from four
different generating processes.
}
\references{
{

  \insertRef{lopez2023hard}{ctsfeatures}

}
}
\keyword{datasets}
