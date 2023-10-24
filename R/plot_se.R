

#' Represents the spectral envelope of a categorical time series
#'
#' \code{plot_se} represents the spectral envelope of
#' a categorical time series
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @return Returns returns a plot of the spectral envelope.
#' @examples
#' plot_se(GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')))
#' # Representing the spectral envelope for the first series in dataset
#' # GeneticSequences
#' @details
#' The function represents the spectral envelope of a categorical time series
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{stoffer1993spectral}{ctsfeatures}
#'
#' }
#' @export

plot_se <- function(series, categories) {

  binarized_series <- binarization(series, categories)
  auxiliary_spectral_envelope(binarized_series, plot = TRUE)

}



