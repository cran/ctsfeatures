

#' Computes the Chebycheff dispersion of a categorical time series
#'
#' \code{chebycheff_dispersion} returns the value of the Chebycheff dispersion for
#' a categorical time series
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @return The value of the Chebycheff dispersion.
#' @examples
#' cd <- chebycheff_dispersion(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Chebycheff dispersion
#' # for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Chebycheff dispersion, \eqn{\widehat{c}=\frac{r}{r-1}(1-\max_i\widehat{p}_i)},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, \eqn{i=1, \ldots, r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

chebycheff_dispersion <- function(series, categories) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  max_mp <- max(vector_mp)


  return((n_categories/(n_categories - 1)) * (1 - max_mp))

}
