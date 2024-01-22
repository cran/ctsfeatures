

#' Represents the spectral envelope of a categorical time series
#'
#' \code{plot_se} represents the spectral envelope of
#' a categorical time series
#'
#' @param series An object of type \code{tsibble} (see R package \code{tsibble}), whose column named Values
#' contains the values of the corresponding CTS. This column must be of class \code{factor} and its levels
#' must be determined by the range of the CTS.
#' @return Returns returns a plot of the spectral envelope.
#' @examples
#' sequence_1 <- GeneticSequences[which(GeneticSequences$Series==1),]
#' plot_se(sequence_1)
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

plot_se <- function(series) {

  binarized_series <- binarization(series)
  auxiliary_spectral_envelope(binarized_series, plot = TRUE)

}



