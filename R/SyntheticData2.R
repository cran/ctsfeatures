#'
#' @title SyntheticData2
#' @description Synthetic dataset containing 80 CTS generated from four
#' different generating processes.
#' @usage data(SyntheticData2)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 80 CTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a CTS of length 600 containing three
#' different categories. Series 1-20, 21-40, 41-60 and 61-80 were generated from
#' Hidden Markov Models with different matrices of transition and emission probabilities (see Scenario 2 in \insertCite{lopez2023hard;textual}{ctsfeatures}).
#' Therefore, there are 4 different classes in the dataset.
#' @references{
#'
#'   \insertRef{lopez2023hard}{ctsfeatures}
#'
#' }
"SyntheticData2"


