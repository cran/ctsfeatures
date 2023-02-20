

#' Computes the Sakoda measure of a categorical time series
#'
#' \code{sakoda_measure} returns the value of the Sakoda measure for
#' a categorical time series
#'
#' @param series A CTS.
#' @param lag The considered lag (default is 1).
#' @param categories A vector of type factor containing the corresponding
#' categories.
#' @param features Logical. If \code{features = FALSE} (default), the value of the Sakoda measure is returned. Otherwise, the function
#' returns a matrix with the individual components of the Sakoda measure.
#' @return If \code{features = FALSE} (default), returns the value of the Sakoda measure. Otherwise, the function
#' returns a matrix of features, i.e., the matrix contains the features employed to compute the
#' Sakoda measure.
#' @examples
#' sm <- sakoda_measure(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't'))) # Computing the Sakoda measure
#' # for the first series in dataset GeneticSequences
#' feature_matrix <- sakoda_measure(series = GeneticSequences$data[[1]],
#' categories = factor(c('a', 'c', 'g', 't')), features = TRUE) # Computing the corresponding
#' # matrix of features
#' @details
#' Given a CTS of length \eqn{T} with range \eqn{\mathcal{V}=\{1, 2, \ldots, r\}},
#' \eqn{\overline{X}_t=\{\overline{X}_1,\ldots, \overline{X}_T\}}, the function computes the
#' estimated Sakoda measure, \eqn{\widehat{p}^*(l)=\sqrt{\frac{r\widehat{\Phi}^2(l)}{(r-1)(1+\widehat{\Phi}^2(l))}}},
#' where \eqn{\widehat{\Phi}^2(l)} is the estimated Phi2 measure. If \code{features = TRUE}, the function
#' returns the same output as the function \code{\link{pearson_measure}}.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @references{
#'
#'   \insertRef{weiss2008measuring}{ctsfeatures}
#'
#' }
#' @export

sakoda_measure <- function(series, lag = 1, categories, features = FALSE) {

  check_cts(series)
  if (features == FALSE) {

  n_categories <- length(categories)
  phi2 <- phi2_measure(series = series, lag = lag, categories = categories, features = features)
  numerator <- n_categories * phi2
  denominator <- (n_categories - 1) * (1 + phi2)


  return(sqrt(numerator/denominator))

  } else {

    phi2 <- phi2_measure(series = series, lag = lag, categories = categories, features = features)
    return(phi2)

  }

}
