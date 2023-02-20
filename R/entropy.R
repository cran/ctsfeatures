

#' Computes the entropy of a categorical time series
#'
#' \code{entropy} returns the value of the entropy for
#' a categorical time series
#'
#' @param series A CTS.
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the entropy is returned. Otherwise, the function
#' returns a vector with the individual components of the entropy.
#' @return The value of the entropy.
#' @examples
#' et <- entropy(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the entropy
#' # for the first series in dataset GeneticSequences
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated entropy, \eqn{\widehat{e}=\frac{-1}{\ln(r)}\sum_{i=1}^{r}\widehat{p}_i\ln \widehat{p}_i},
#' where \eqn{\widehat{p}_i} is the natural estimate of the marginal probability of the \eqn{i}th
#' category, \eqn{i=1, \ldots, r}. If \code{features = TRUE}, the function
#' returns a vector whose components are the quantities \eqn{\widehat{p}_i\ln(\widehat{p}_i)},
#' \eqn{i=1,2, \ldots,r}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

entropy <- function(series, categories, features = FALSE) {

  check_cts(series)
  n_categories <- length(categories)
  vector_mp <- marginal_probabilities(series = series, categories = categories)
  vector_log_mp <- log(vector_mp)
  vector_product <- vector_mp * vector_log_mp


  if (features == FALSE) {

    return((-1/log(n_categories)) * sum(vector_product))

  } else {

    return(vector_product)

  }

}
