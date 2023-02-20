

#' Computes the gini index of a categorical time series
#'
#' \code{gini_index} returns the value of the gini index for
#' a categorical time series
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @return The value of the gini index.
#' @examples
#' gi <- gini_index(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the gini index
#' # for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated gini index, \eqn{\widehat{g}=\frac{r}{r-1}(1-\sum_{i=1}^{r}\widehat{p}_i^2)},
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

gini_index <- function(series, categories) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  sum_sq_mp <- sum(vector_mp^2)


  return((n_categories/(n_categories - 1)) * (1 - sum_sq_mp))

}
