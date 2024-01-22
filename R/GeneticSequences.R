#'
#' @title GeneticSequences
#' @description Categorical time series (CTS) of DNA sequences from different
#' viruses
#' @usage data(GeneticSequences)
#' @format A \code{tsibble} with four columns, which are:
#' \describe{
#' \item{\code{Value}}{The categorical values of the time series in the dataset.}
#' \item{\code{Series}}{Integer values indicating the considered time series (there are 32 time series in the dataset).}
#' \item{\code{Time}}{Integer values indicating the temporal indexes of the observations.}
#' \item{\code{Class}}{Integer values indicating the class of each time series.}
#' }
#' @details The column \code{Value} is the concatenation of 32 time series
#' taking four categorical values (DNA bases). The column \code{Class} is formed
#' by integers from 1 to 4, indicating that there are 4 different classes in the database. Each class is associated with a different
#' family of viruses. For more information, see \insertCite{lopez2023hard;textual}{ctsfeatures}.
#' @references{
#'
#'   \insertRef{lopez2023hard}{ctsfeatures}
#'
#' }
"GeneticSequences"


