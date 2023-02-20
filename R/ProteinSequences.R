#'
#' @title ProteinSequences
#' @description Categorical time series (CTS) of protein sequences from different
#' species
#' @usage data(ProteinSequences)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 40 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes
#' associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a categorical time series
#' containing three categories (amino-acids). The numeric vector \code{classes} is formed
#' by integers from 1 to 4, indicating that there are 4 different classes in the database. Each class is associated with a different
#' family of viruses. For more information, see \insertCite{lopez2023hard;textual}{ctsfeatures}.
#' @references{
#'
#'   \insertRef{lopez2023hard}{ctsfeatures}
#'
#' }
"ProteinSequences"


