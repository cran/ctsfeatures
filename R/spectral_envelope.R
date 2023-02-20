

#' Computes the spectral envelope of a categorical time series
#'
#' \code{spectral_envelope} computes the spectral envelope
#' a categorical time series
#'
#' @param binarized_series A CTS in binarized form.
#' @param plot Logical. If \code{plot = TRUE} (default), returns a plot of the
#' spectral envelope. Otherwise, returns the values of the spectral envelope
#' at each frequency and the corresponding set of optimal scalings
#' @return If \code{plot = TRUE} (default), returns returns a plot of the spectral envelope. Otherwise, the function
#' returns the values of the spectral envelope  at each frequency and the corresponding set
#' of optimal scalings
#' @examples
#' binarized_series <- binarization(GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')))
#' se <- spectral_envelope(binarized_series = binarized_series) # Representing the spectral envelope
#' # for the first series in dataset GeneticSequences
#' spectral_quantities <- spectral_envelope(binarized_series = binarized_series,
#' plot = FALSE) # Computing the corresponding
#' # spectral quantities
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

spectral_envelope <- function(binarized_series, plot = TRUE) {

  check_cts(binarized_series)
  if (plot == FALSE) {

    return(astsa::specenv(binarized_series, plot = plot))

  } else {

    return(astsa::specenv(binarized_series, plot = plot))

  }

}



